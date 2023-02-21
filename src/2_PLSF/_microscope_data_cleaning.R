## Title:      Petit-lac-Saint-François biota data
##             Data formatting and cleaning
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 05/11/2022

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/","/Export/","/Data/PLSF_Microscope/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# -------------------------------------------------------------------------

files.all=list.files(data.path,full.names=T)
files.all=files.all[grepl("Phytoplankton 2009-2015",files.all)==F]

tmp=lapply(files.all,getSheetNames)

tmp=sapply(tmp,"[",1)
tmp=sapply(tmp,"[",2)

tmp=sapply(tmp,"[",3)
files.all[grepl("Protozoa",tmp)]


## First pattern
# vals1=files.all[grepl("PLSF Phyto-Zoop ",files.all)]
vals1=files.all[grepl("PLSF Phyto-Zoop ",files.all)]
vals1=c(vals1,files.all[grepl("PLSF Phyto-Zoop-Protoz",files.all)])
vals1=c(vals1,files.all[grepl("PLSF Phyto ",files.all)])# phyto only

length(vals1)
# Phytoplankton
# Read header and find startRow
phyto.col.names=list()
for(i in 1:length(vals1)){
  tmp=names(read.xlsx(vals1[i],sheet=1,startRow = 9))
  startrow.val=ifelse(sum(tmp%in%c("Class","Division"))==1,9,8)
  tmp=names(read.xlsx(vals1[i],sheet=1,startRow = startrow.val))
  phyto.col.names=append(phyto.col.names,list(tmp))
  print(i)
}
lengths(phyto.col.names)
phyto.col.names
## Read Data
colnm.vals=c("Class","GenusSpp","Conc.cellsmL","totbiovol.um3mL")
colvars1_common=c("Class","Taxa","Genus.species","Concentration.(cells/mL)","Total.biovolume.(µm3/mL)",
                  "Cells/mL","Total.biovol..(µm3/mL)")

phyto.dat=data.frame()
for(i in 1:length(vals1)){
  
tmp.col.vars=phyto.col.names[[i]][phyto.col.names[[i]]%in%colvars1_common]
tmp=names(read.xlsx(vals1[i],sheet=1,startRow = 9))
startrow.val=ifelse(sum(tmp%in%c("Class","Division"))==1,9,8)
tmp=read.xlsx(vals1[i],sheet=1,startRow = startrow.val)
ncol.vals=ncol(tmp)

tmp=tmp[,tmp.col.vars]
colnames(tmp)=colnm.vals
date.val=convertToDate(read.xlsx(vals1[i],sheet=1,startRow =1)[1,ncol.vals])
if(is.na(date.val)==T){date.val=convertToDate(read.xlsx(vals1[i],sheet=1,startRow =1)[2,ncol.vals])}
tmp$date=date.val
# tmp$file=vals1[i]

phyto.dat=rbind(phyto.dat,tmp)
print(i)
}

# unique(subset(phyto.dat,is.na(date))$file)
# shell.exec(unique(subset(phyto.dat,is.na(date))$file)[1])


## only data, removes extra rows with index and total values in report
class.vals=c("Bacillariophyceae", "Chlorophyceae", "Cryptophyceae", "Cyanophyceae", 
             "Dinophyceae", "Euglenophyceae","Chrysophyceae","Conjugatophyceae",
             "Prymnesiophyceae")
phyto.dat=subset(phyto.dat,Class%in%class.vals)

### Zooplankton
vals2=files.all[grepl("PLSF Phyto-Zoop ",files.all)]
vals2=c(vals2,files.all[grepl("PLSF Phyto-Zoop-Protoz",files.all)])

dat.col.nam=c("conc.numbL","rel.abund","ind.biomass.ug","tot.biomass.ugL","rel.biomass")
zoo.col.names=list()
for(i in 1:length(vals2)){
  tmp=names(read.xlsx(vals2[i],sheet=2,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Order"))==1,10,11)
  tmp=names(read.xlsx(vals2[i],sheet=2,startRow = startrow.val))
  if(length(tmp[substr(tmp,1,1)=="X"])==6){
    tmp[substr(tmp,1,1)=="X"]=c("blank",dat.col.nam)
  }else{
    tmp[substr(tmp,1,1)=="X"]=dat.col.nam
  }
  
  zoo.col.names=append(zoo.col.names,list(tmp))
}
zoo.col.names
lengths(zoo.col.names)

## Read Data
colnm.vals=c("Order","Taxa","conc.numbL","tot.biomass.ugL")

zoo.dat=data.frame()
for(i in 1:length(vals2)){
  
  tmp.col.vars=zoo.col.names[[i]][zoo.col.names[[i]]%in%colnm.vals]
  tmp=names(read.xlsx(vals2[i],sheet=2,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Order"))==1,10,11)
  tmp=read.xlsx(vals2[i],sheet=2,startRow = startrow.val)
  if(sum(tmp$Taxa%in%c("No zooplankton present"))==1){next}else{
  colnames(tmp)=zoo.col.names[[i]]
  
  ncol.vals=ncol(tmp)
  
  tmp=tmp[,tmp.col.vars]
  colnames(tmp)=colnm.vals
  tmp$date=convertToDate(read.xlsx(vals2[i],sheet=2,startRow =1)[2,ncol.vals])
  
  zoo.dat=rbind(zoo.dat,tmp)
  }
  print(i)
}

## only data, removes extra rows with index and total values in report
order.vals=c("Cladocera", "Copepoda", "Cyclopoida", "Ploima", "Flosculariaceae","Bdelloidea")
zoo.dat=subset(zoo.dat,Order%in%order.vals)

## Second pattern
files.all[!(files.all%in%vals1)]

vals3=files.all[grepl("BlueLeaf PLSF Reports ",files.all)]

length(vals3)
# Phytoplankton
# Read header and find startRow
phyto.col.names=list()
for(i in 1:length(vals3)){
  tmp=names(read.xlsx(vals3[i],sheet=1,startRow = 9))
  startrow.val=ifelse(sum(tmp%in%c("Class","Division"))==1,9,8)
  tmp=names(read.xlsx(vals3[i],sheet=1,startRow = startrow.val))
  phyto.col.names=append(phyto.col.names,list(tmp))
  print(i)
}
lengths(phyto.col.names)
phyto.col.names
## Read Data
colnm.vals=c("Class","GenusSpp","Conc.cellsmL","totbiovol.um3mL")
colvars1_common=c("Division","Taxa","Genus.species","Concentration.(cells/mL)","Total.biovolume.(µm3/mL)",
                  "Cells/mL","Total.biovol..(µm3/mL)")

phyto.dat2=data.frame()
for(i in 1:length(vals3)){
  
  tmp.col.vars=phyto.col.names[[i]][phyto.col.names[[i]]%in%colvars1_common]
  tmp=names(read.xlsx(vals3[i],sheet=1,startRow = 9))
  startrow.val=ifelse(sum(tmp%in%c("Class","Division"))==1,9,8)
  tmp=read.xlsx(vals3[i],sheet=1,startRow = startrow.val)
  ncol.vals=ncol(tmp)-1
  
  tmp=tmp[,tmp.col.vars]
  colnames(tmp)=colnm.vals
  tmp$date=convertToDate(read.xlsx(vals3[i],sheet=1,startRow =1)[2,ncol.vals])
  phyto.dat2=rbind(phyto.dat2,tmp)
  print(i)
}
unique(phyto.dat2$Class)
class.vals=c("Bacillariophyceae", "Chlorophyta", "Chrysophyta", "Cryptophycophyceae", 
             "Cyanophyta", "Euglenophyceae", "Pyrrophycophyta")
phyto.dat2=subset(phyto.dat2,Class%in%class.vals)

# phyto.dat=rbind(phyto.dat,phyto.dat2)

### Zooplankton
dat.col.nam=c("conc.numbL","rel.abund","ind.biomass.ug","tot.biomass.ugL","rel.biomass")
zoo.col.names=list()
for(i in 1:length(vals3)){
  tmp=names(read.xlsx(vals3[i],sheet=2,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Order"))==1,10,11)
  tmp=names(read.xlsx(vals3[i],sheet=2,startRow = startrow.val))
  if(length(tmp[substr(tmp,1,1)=="X"])==6){
    tmp[substr(tmp,1,1)=="X"]=c("blank",dat.col.nam)
  }else{
    tmp[substr(tmp,1,1)=="X"]=dat.col.nam
  }
  
  zoo.col.names=append(zoo.col.names,list(tmp))
}
zoo.col.names
lengths(zoo.col.names)

## Read Data
colnm.vals=c("Order","Taxa","conc.numbL","tot.biomass.ugL")

zoo.dat2=data.frame()
for(i in 1:length(vals3)){
  
  tmp.col.vars=zoo.col.names[[i]][zoo.col.names[[i]]%in%colnm.vals]
  tmp=names(read.xlsx(vals3[i],sheet=2,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Order"))==1,10,11)
  tmp=read.xlsx(vals3[i],sheet=2,startRow = startrow.val)
  if(sum(tmp$Taxa%in%c("No zooplankton present"))==1){next}else{
    colnames(tmp)=zoo.col.names[[i]]
    
    ncol.vals=ncol(tmp)
    
    tmp=tmp[,tmp.col.vars]
    colnames(tmp)=colnm.vals
    tmp$date=convertToDate(read.xlsx(vals3[i],sheet=2,startRow =1)[2,ncol.vals])
    
    zoo.dat2=rbind(zoo.dat2,tmp)
  }
  print(i)
}
unique(zoo.dat2$Order)

order.vals=c("Cladocera", "Copepoda", "Cyclopoida", "Ploima")
zoo.dat2=subset(zoo.dat2,Order%in%order.vals)

## Third pattern Zooplankton only
files.all[!(files.all%in%c(vals1,vals3))]

vals4=files.all[grepl("BlueLeaf Zoop Report ",files.all)]
vals4=c(vals4,files.all[grepl("PLSF Zoop revised ",files.all)])

### Zooplankton
dat.col.nam=c("conc.numbL","rel.abund","ind.biomass.ug","tot.biomass.ugL","rel.biomass")
zoo.col.names=list()
for(i in 1:length(vals4)){
  tmp=names(read.xlsx(vals4[i],sheet=1,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Order"))==1,10,11)
  tmp=names(read.xlsx(vals4[i],sheet=1,startRow = startrow.val))
  if(length(tmp[substr(tmp,1,1)=="X"])==6){
    tmp[substr(tmp,1,1)=="X"]=c("blank",dat.col.nam)
  }else{
    tmp[substr(tmp,1,1)=="X"]=dat.col.nam
  }
  
  zoo.col.names=append(zoo.col.names,list(tmp))
}
zoo.col.names
lengths(zoo.col.names)

## Read Data
colnm.vals=c("Order","Taxa","conc.numbL","tot.biomass.ugL")

zoo.dat3=data.frame()
for(i in 1:length(vals4)){
  
  tmp.col.vars=zoo.col.names[[i]][zoo.col.names[[i]]%in%colnm.vals]
  tmp=names(read.xlsx(vals4[i],sheet=1,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Order"))==1,10,11)
  tmp=read.xlsx(vals4[i],sheet=1,startRow = startrow.val)
  if(sum(tmp$Taxa%in%c("No zooplankton present"))==1){next}else{
    colnames(tmp)=zoo.col.names[[i]]
    
    ncol.vals=ncol(tmp)
    
    tmp=tmp[,tmp.col.vars]
    colnames(tmp)=colnm.vals
    tmp$date=convertToDate(read.xlsx(vals4[i],sheet=1,startRow =1)[2,ncol.vals])
    
    zoo.dat3=rbind(zoo.dat3,tmp)
  }
  print(i)
}
unique(zoo.dat3$Order)

order.vals=c("Cladocera", "Copepoda", "Cyclopoida", "Ploima")
zoo.dat3=subset(zoo.dat3,Order%in%order.vals)

### combine phyto and zoo data
phyto.dat.all=rbind(phyto.dat,phyto.dat2)
zoo.dat.all=rbind(zoo.dat,zoo.dat2,zoo.dat3)
is.na(zoo.dat.all$conc.numbL)

is.na(zoo.dat.all$tot.biomass.ugL)

## quick check of data
subset(phyto.dat.all,is.na(date))
subset(zoo.dat.all,is.na(date))

phyto.dat.all$GenusSpp[grepl("mic",phyto.dat.all$GenusSpp)]
phyto.dat.all$GenusSpp[grepl("Mic",phyto.dat.all$GenusSpp)]
unique(subset(phyto.dat.all,Class=="Cyanophyceae")$GenusSpp)

head(phyto.dat.all)

phyto.class.conc=dcast(phyto.dat.all,date~Class,value.var = "Conc.cellsmL",sum)
head(phyto.class.conc)


grepl("^\\s*$", phyto.dat.all$GenusSpp)
grepl("^\\s*$", zoo.dat.all$Taxa)


## forth pattern
files.all[!(files.all%in%c(vals1,vals3,vals4))]

files.all[grepl("Proto",files.all)]

vals5=files.all[grepl("PLSF Phyto-Zoop-Protoz",files.all)]

# Protozoa
# Read header and find startRow
dat.col.nam=c("conc.numbL","rel.abund","ind.biovol.um3","tot.biovol.um3L","rel.biovol")

proto.col.names=list()
for(i in 1:length(vals5)){
  tmp=names(read.xlsx(vals5[i],sheet=3,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Taxa"))==1,10,11)
  tmp=names(read.xlsx(vals5[i],sheet=3,startRow = startrow.val))
  if(length(tmp[substr(tmp,1,1)=="X"])==6){
    tmp[substr(tmp,1,1)=="X"]=c("blank",dat.col.nam)
  }else{
    tmp[substr(tmp,1,1)=="X"]=dat.col.nam
  }
  
  proto.col.names=append(proto.col.names,list(tmp))
}
proto.col.names
lengths(proto.col.names)


colnm.vals=c("Taxa","conc.numbL","tot.biovol.um3L")

proto.dat=data.frame()
for(i in 1:length(vals5)){
  
  tmp.col.vars=proto.col.names[[i]][proto.col.names[[i]]%in%colnm.vals]
  tmp=names(read.xlsx(vals5[i],sheet=3,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Taxa"))==1,10,11)
  tmp=read.xlsx(vals5[i],sheet=3,startRow = startrow.val)
  
    colnames(tmp)=proto.col.names[[i]]
    
    ncol.vals=ncol(tmp)
    
    tmp=tmp[,tmp.col.vars]
    colnames(tmp)=colnm.vals
    tmp$date=convertToDate(read.xlsx(vals5[i],sheet=3,startRow =1)[2,ncol.vals])
    
    proto.dat=rbind(proto.dat,tmp)
  print(i)
}

##
vals6=files.all[grepl("Protozoan Reports",files.all)]

tmp1=names(read.xlsx(vals6,sheet=1,startRow = 10))
tmp1
tmp2=names(read.xlsx(vals6,sheet=2,startRow = 10))
tmp2
dat.col.nam=c("conc.numbL", "rel.abund", "ind.biovol.um3", "tot.biovol.um3L", 
              "rel.biovol")
proto.col.names=list()
for(i in 1:3){
  tmp=names(read.xlsx(vals6,sheet=i,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Taxa"))==1,10,11)
  tmp=names(read.xlsx(vals6,sheet=i,startRow = startrow.val))
  if(length(tmp[substr(tmp,1,1)=="X"])==6){
    tmp[substr(tmp,1,1)=="X"]=c("blank",dat.col.nam)
  }else{
    tmp[substr(tmp,1,1)=="X"]=dat.col.nam
  }
  
  proto.col.names=append(proto.col.names,list(tmp))
}
proto.col.names
lengths(proto.col.names)


proto.dat2=data.frame()
for(i in 1:3){
  
  tmp.col.vars=proto.col.names[[i]][proto.col.names[[i]]%in%colnm.vals]
  tmp=names(read.xlsx(vals6,sheet=i,startRow = 10))
  startrow.val=ifelse(sum(tmp%in%c("Taxa"))==1,10,11)
  tmp=read.xlsx(vals6,sheet=i,startRow = startrow.val)
  
  colnames(tmp)=proto.col.names[[i]]
  
  ncol.vals=ncol(tmp)
  
  tmp=tmp[,tmp.col.vars]
  colnames(tmp)=colnm.vals
  tmp$date=convertToDate(read.xlsx(vals6,sheet=i,startRow =1)[2,ncol.vals])
  
  proto.dat2=rbind(proto.dat2,tmp)
  print(i)
}

proto.dat.all=rbind(proto.dat,proto.dat2)

unique(proto.dat.all$Taxa)

proto.taxa=c("Unidentified ciliates", "Vorticella", "Strobilidium", "Tintinnopsis", 
             "Rotifers", "Tintinnopsis sp. 1", "Tintinnopsis sp. 2", "Ciliated protozoans (unidentified)", 
             "Strobilidium sp.", "Unidentified ciliates ", "Difflugia sp.")
proto.dat.all=subset(proto.dat.all,Taxa%in%proto.taxa)

unique(proto.dat.all$date)

### 2009 - 2015 phytoplankton data
head(phyto.dat.all)
unique(phyto.dat.all$Class)

phyto_20092015=read.xlsx(paste0(data.path,"PLSF Phytoplankton 2009-2015 (condensed).xlsx"))
phyto_20092015=subset(phyto_20092015,is.na(Sampling.Date)==F)
phyto_20092015$date=with(phyto_20092015,ifelse(grepl("-",Sampling.Date)==F,
                                               as.character(convertToDate(as.numeric(Sampling.Date))),
                                               Sampling.Date))

phyto_20092015$date=as.Date(phyto_20092015$date)




unique(phyto_20092015$Algal.Group)

head(phyto_20092015,1L)
phyto_20092015$GenusSpp=with(phyto_20092015, paste(Genus,Species))
vars=c("Algal.Group","GenusSpp","Species.Cells/mL","Species.Biovolume/mL","date")

phyto_20092015=phyto_20092015[,vars]

colnames(phyto_20092015)=names(phyto.dat.all)

## Combine all phyto data
phyto.dat.all=rbind(phyto.dat.all,phyto_20092015)

unique(phyto_20092015$Class)
unique(phyto.dat.all$Class)

range(phyto.dat.all$date)

##


# write.csv(phyto.dat.all[,c("GenusSpp", "Conc.cellsmL", "totbiovol.um3mL", "date")],
#           paste0(export.path,"PLSF_microscope_data_phyto.csv"),row.names = F)
# write.csv(zoo.dat.all[,c("Taxa", "conc.numbL", "tot.biomass.ugL", "date")],
#           paste0(export.path,"PLSF_microscope_data_zoo.csv"),row.names = F)
# write.csv(proto.dat.all[,c("Taxa", "conc.numbL", "tot.biovol.um3L", "date")],
#           paste0(export.path,"PLSF_microscope_data_proto.csv"),row.names = F)


head(phyto.dat.all)

phyto.dat.biovol=ddply(subset(phyto.dat.all,as.numeric(format(date,"%Y"))>2009),c("date"),summarise,T.biovol=sum(totbiovol.um3mL,na.rm=T))

plot(T.biovol~date,phyto.dat.biovol,log="y")


phyto.dat.conc.xtab=dcast(phyto.dat.all,date~GenusSpp,value.var = "Conc.cellsmL",sum)

phyto.dat.conc.xtab[1,]

# vegan::specnumber(phyto.dat.conc.xtab[1,2:ncol(phyto.dat.conc.xtab)])
library(vegan)
## proportional abundance
phyto.dat.conc.xtab.prop=phyto.dat.conc.xtab
x.val=apply(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab.prop)],1,sum)
phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)]=sweep(phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)],1,x.val,"/")

## Diversity (alpha) Indices
# Shannon Index
shannon=-phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)]*log(phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)])
shannon_H=apply(shannon,1,sum,na.rm=T)

# Simpson
simpson_D=1-apply(phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)]^2,1,sum,na.rm=T)
simpson_invD=1/apply(phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)]^2,1,sum,na.rm=T)

plot(simpson_D)

#species richness
richness=specnumber(phyto.dat.conc.xtab.prop[,2:ncol(phyto.dat.conc.xtab.prop)])
plot(richness)

# Pielou's evenness J' #https://www.rpubs.com/roalle/mres_2019
pielou_even=shannon_H/log(richness)

phyto.metric=data.frame(date=phyto.dat.conc.xtab$date,
           shannon_H=shannon_H,
           simpson_D=simpson_D,
           simpson_invD=simpson_invD,
           richness=richness,
           pielou_even=pielou_even)

## zooplankton
zoo.dat.conc.xtab=dcast(zoo.dat.all,date~Taxa,value.var = "conc.numbL",sum)

## proportional abundance
zoo.dat.conc.xtab.prop=zoo.dat.conc.xtab
x.val=apply(zoo.dat.conc.xtab[,2:ncol(zoo.dat.conc.xtab.prop)],1,sum)
zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)]=sweep(zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)],1,x.val,"/")

## Diversity (alpha) Indices
# Shannon Index
shannon=-zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)]*log(zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)])
shannon_H=apply(shannon,1,sum,na.rm=T)

# Simpson
simpson_D=1-apply(zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)]^2,1,sum,na.rm=T)
simpson_invD=1/apply(zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)]^2,1,sum,na.rm=T)

plot(simpson_D)

#species richness
richness=specnumber(zoo.dat.conc.xtab.prop[,2:ncol(zoo.dat.conc.xtab.prop)])
plot(richness)

# Pielou's evenness J' #https://www.rpubs.com/roalle/mres_2019
pielou_even=shannon_H/log(richness)

zoo.metric=data.frame(date=zoo.dat.conc.xtab$date,
                        shannon_H=shannon_H,
                        simpson_D=simpson_D,
                        simpson_invD=simpson_invD,
                        richness=richness,
                        pielou_even=pielou_even)

# write.csv(phyto.metric,
#            paste0(export.path,"PLSF_microscope_phyto_mertrics.csv"),row.names = F)
# write.csv(zoo.metric,
#            paste0(export.path,"PLSF_microscope_zoo_mertrics.csv"),row.names = F)


# -------------------------------------------------------------------------
# library(taxizedb)
# GenusSpp.list=unique(trimws(phyto.dat.all$GenusSpp))
# GenusSpp.list[is.na(name2taxid(GenusSpp.list,db="itis"))==T]
# tmp1=name2taxid(GenusSpp.list[1],db="itis")
# classification(tmp1, db='itis')
# subset(phyto.dat.all,GenusSpp==GenusSpp.list[1])



# library(taxize)
# gnr_resolve(sci=c("Helianthus annuus"))
# specieslist <- c("Abies procera","Pinus contorta")
# classification(specieslist, db = 'itis')
# classification(specieslist, db = 'ncbi')
# 
# 
# GenusSpp.list=unique(trimws(phyto.dat.all$GenusSpp))
# # GenusSpp.list[is.na(name2taxid(GenusSpp.list,db="itis"))==T]
# 
# GenusSpp.list2=GenusSpp.list[grepl("Unid|Mis",GenusSpp.list)==F]
# SppList.tree=data.frame()
# for(i in 1:length(GenusSpp.list2)){
# tmp=gnr_resolve(GenusSpp.list[i])
# # head(tmp)
# # tmp$matched_name[1]
# rslt=classification(tmp$submitted_name[1],db='ncbi')
# if(is.na(rslt[tmp$submitted_name[1]])==T){
#   rslt=classification(tmp$matched_name[2],db='ncbi')  
#   rslt2=as.data.frame(rslt[tmp$matched_name[2]])
# }else{
# rslt=classification(tmp$submitted_name[1],db='ncbi')
# rslt2=as.data.frame(rslt[tmp$submitted_name[1]])
# }
# 
# colnames(rslt2)=c("name","rank","id")
# vars=c("order",'class',"genus","species")
# rslt3=subset(rslt2,rank%in%vars)
# rslt3=tidyr::spread(rslt3[,1:2],rank,name)
# 
# vars.fill=vars[vars%in%names(rslt3)==F]
# rslt3[,vars.fill]=NA
# rslt3[,vars]
# 
# rslt3$GenusSpp=GenusSpp.list[i]
# SppList.tree=rbind(SppList.tree,rslt3)
# }
# 
# SppList.tree
## loop species class, etc info doesn't work. Supplied names don't match 