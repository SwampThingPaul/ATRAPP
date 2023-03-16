## Title:      Petit-lac-Saint-François - biotia changes
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

library(mblm)
library(vegan)

library(flextable)
library(magrittr)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_Paper2/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

# -------------------------------------------------------------------------

plsf.phyto=read.csv(paste0(export.path,"PLSF_microscope_data_phyto.csv"))
plsf.phyto$date=date.fun(plsf.phyto$date)
plsf.phyto$month=as.numeric(format(plsf.phyto$date,"%m"))
plsf.phyto$CY=as.numeric(format(plsf.phyto$date,"%Y"))

unique(plsf.phyto$GenusSpp)


# Classification table ----------------------------------------------------
library(rgbif)
library(taxize)
library(Rocc)

GenusSpp.list=unique(trimws(plsf.phyto$GenusSpp))
GenusSpp.df=data.frame(GenusSpp=GenusSpp.list)

GenusSpp.list=gsub("sp.","sp",gsub("spp.","sp",GenusSpp.list))
GenusSpp.df$GenusSpp2=GenusSpp.list

splt=strsplit(GenusSpp.df$GenusSpp2,"/")
lst.vals=grep("/",GenusSpp.df$GenusSpp2)
GenusSpp.df[lst.vals,]
GenusSpp.df$GenusSpp2=with(GenusSpp.df,ifelse(grepl("/",GenusSpp2)==T,
                                              paste(sapply(splt,"[",1),"sp"),GenusSpp2))
GenusSpp.df[lst.vals,]

splt=strsplit(GenusSpp.df$GenusSpp2,"#")
lst.vals=grep("#",GenusSpp.df$GenusSpp2)
GenusSpp.df$GenusSpp2=with(GenusSpp.df,ifelse(grepl("#",GenusSpp2)==T,
                                              sapply(splt,"[",1),GenusSpp2))
GenusSpp.df[lst.vals,]

GenusSpp.df$GenusSpp2=trimws(gsub("cf","",
            gsub("cf.","",
                 gsub("(cell pair)","",
                      gsub("(unicell)","",
                           gsub("(unid)","",
                                gsub("(colony)","",GenusSpp.df$GenusSpp2)))))))

splt=strsplit(GenusSpp.df$GenusSpp2,"v. ")
lst.vals=grep("v. ",GenusSpp.df$GenusSpp2)
GenusSpp.df[lst.vals,]
GenusSpp.df$GenusSpp2=with(GenusSpp.df,ifelse(grepl("v. ",GenusSpp2)==T,
                                              sapply(splt,"[",1),GenusSpp2))
GenusSpp.df[lst.vals,]

splt=strsplit(GenusSpp.df$GenusSpp2,"var. ")
lst.vals=grep("var. ",GenusSpp.df$GenusSpp2)
GenusSpp.df[lst.vals,]
GenusSpp.df$GenusSpp2=with(GenusSpp.df,ifelse(grepl("var. ",GenusSpp2)==T,
                                              sapply(splt,"[",1),GenusSpp2))
GenusSpp.df[lst.vals,]

# fix individual errors
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="Synedra acus sp"]="Synedra acus"
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="cyanophyte , oval sp"]="cyanophyte sp"
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="Pennate diatoms()"]="Pennate diatoms"
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="Microcystis sp()"]="Microcystis sp"
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="cyanophyte l, oval sp"]="cyanophyte sp"
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="Nitzchia"]="Nitzschia sp"
GenusSpp.df$GenusSpp2[GenusSpp.df$GenusSpp2=="Nitzschia"]="Nitzschia sp"

GenusSpp.df[grepl("chlorophyte ,",GenusSpp.df$GenusSpp2),"GenusSpp2"]="chlorophyte sp"
GenusSpp.df[grepl("chlorophyte tetrad sp",GenusSpp.df$GenusSpp2),"GenusSpp2"]="chlorophyte sp"
GenusSpp.df[grepl("chlorophyte flagellate sp",GenusSpp.df$GenusSpp2),"GenusSpp2"]="chlorophyte sp"
GenusSpp.df[grepl("Microcystis flos aquae",GenusSpp.df$GenusSpp2),"GenusSpp2"]="Microcystis aeruginosa"
GenusSpp.df[grepl("Microcystis",GenusSpp.df$GenusSpp2),"GenusSpp2"]="Microcystis sp"

GenusSpp.df[grepl("chrysophyte",GenusSpp.df$GenusSpp2),"GenusSpp2"]="chrysophyte sp"
GenusSpp.df[grepl("Dictytosphaerium",GenusSpp.df$GenusSpp2),"GenusSpp2"]="Dictyosphaerium sp"
GenusSpp.df[grepl("unknown",GenusSpp.df$GenusSpp2),"GenusSpp2"]="unid"
GenusSpp.df[grepl("Unid",GenusSpp.df$GenusSpp2),"GenusSpp2"]="unid"

GenusSpp.df[grepl("cyanophyte , spere",GenusSpp.df$GenusSpp2),"GenusSpp2"]="cyanophyte sp"
GenusSpp.df[grepl("spere",GenusSpp.df$GenusSpp2),"GenusSpp2"]="unid"

## Fix spelling
GenusSpp.df$GenusSpp2=trimws(GenusSpp.df$GenusSpp2)
GenusSpp.df$GenusSpp3=suggest_flora(GenusSpp.df$GenusSpp2)$species
subset(GenusSpp.df,is.na(GenusSpp3)==T)
sum(is.na(GenusSpp.df$GenusSpp3))

GenusSpp.df$GenusSpp3=with(GenusSpp.df,ifelse(grepl(" sp",GenusSpp2)==T&grepl("sp",GenusSpp3)==F,
                                              GenusSpp2,GenusSpp3))


# subset(GenusSpp.df,grepl(" sp",GenusSpp2)==T&grepl("sp",GenusSpp3)==F)

GenusSpp.df$GenusSpp3=with(GenusSpp.df,ifelse(is.na(GenusSpp3)==T,GenusSpp2,GenusSpp3))
GenusSpp.list=unique(GenusSpp.df$GenusSpp3)
# write.csv(data.frame(GenusSpp=GenusSpp.list),paste0(export.path,"micro_phyto_spplist.csv"),row.names = F)

### Just incase you forget dummy - https://study.com/cimages/multimages/16/taxonomy1856701098547838353.png
vars=c("class","order","genus","verbatim_name")# c("class","order","family","genus","verbatim_name")
GenusSpp.list2=data.frame()
for(i in 1:length(GenusSpp.list)){
  tmp=data.frame(name_backbone(trimws(GenusSpp.list[i])))
  if(sum(vars%in%names(tmp)==F)>0){
    # fills empty fields
    tmp[,vars[vars%in%names(tmp)==F]]=NA
    tmp=tmp[,vars]
  }else{
    tmp=tmp[,vars]
  }
  GenusSpp.list2=rbind(GenusSpp.list2,tmp)
  print(i)
}

subset(GenusSpp.list2,is.na(class)==T)
sum(is.na(GenusSpp.list2$class))

GenusSpp.list2[GenusSpp.list2$verbatim_name=="unid",c("class","order","genus")]="unid"

##
spp.vals=subset(GenusSpp.list2,is.na(class)==T)

ncbi.trees=data.frame()
for(i in 1:nrow(spp.vals)){
  tmp=classification(spp.vals$verbatim_name[i],db="ncbi")
  tmp=data.frame(tmp[1])
  if(is.na(tmp)==T){
    tmp$verbatim_name=spp.vals$verbatim_name[i]
    tmp[,names(spp.vals)[names(spp.vals)%in%names(tmp)==F]]=NA
    tmp=tmp[,names(spp.vals)]
  }else{
  colnames(tmp)=c("name","rank","id")
  tmp=subset(tmp,rank!="no rank")
  tmp=subset(tmp,rank%in%c("class","order","genus"))
  tmp=tidyr::spread(tmp[,1:2],rank,name)
  
  tmp$verbatim_name=spp.vals$verbatim_name[i]
  if(sum(names(spp.vals)%in%names(tmp)==F)>0){
    tmp[,names(spp.vals)[names(spp.vals)%in%names(tmp)==F]]=NA
    tmp=tmp[,names(spp.vals)]
  }else{
    tmp=tmp[,names(spp.vals)]
  }
  }
  ncbi.trees=rbind(ncbi.trees,tmp)
  
}

ncbi.trees
subset(ncbi.trees,is.na(class)==F)

## update species list
GenusSpp.list2=rbind(
subset(GenusSpp.list2,is.na(class)==F),
subset(ncbi.trees,is.na(class)==F)
)

subset(ncbi.trees,is.na(class)==T)
nrow(subset(ncbi.trees,is.na(class)==T))
# Complete the rest by hand
# write.csv(subset(ncbi.trees,is.na(class)==T),
#           paste0(export.path,"phyto_spp_class_mt.csv"),
#           row.names = F)

# other.trees=read.csv(paste0(export.path,"phyto_spp_class_filled.csv"))
other.trees.df=data.frame(
  class=c("Dinophyceae", "Bacillariophyceae", "Katablepharidea", "Chlorophyceae", 
          "Dinophyceae", NA, NA, "Chlorophyceae", "Dinophyceae", "Bacillariophyceae", 
          "Chlorophyceae", "Chlorophyceae", "Cyanophyceae", "Cyanophyceae", 
          "Cyanophyceae", "Cryptophyceae", "Cyanophyceae", NA, "Xanthophyceae", 
          "Trebouxiophyceae", "Euglenophyceae", "Bacillariophyceae", "Chlorophyceae", 
          "Eurotiomycetes", "Cyanophyceae", "Cyanophyceae", "Trebouxiophyceae", 
          NA, "Chrysophyceae"),
  order=c("Gonyaulacales", NA, "Katablepharida", "Chlamydomonadales", 
          NA, NA, NA, "Sphaeropleales", NA, NA, NA, NA, NA, NA, NA, NA, 
          NA, NA, NA, "Chlorellales", NA, "Achnanthales", "Sphaeropleales", 
          "Onygenales", "Nostocales", NA, "Chlorellales", NA, NA),
  genus=c("Ceratium", NA, "Kateblepharis", "Tetraspora", NA, NA, NA, 
          "Kirchneriella", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
          NA, NA, "Achnanthdium", "Coenocystis", "Mucidospaerium", NA, 
          NA, "Dictytospaerium", NA, NA),
  verbatim_name=c("Ceratium sp", "Pennate diatoms", "Kateblepharis ovalis", "Tetraspra sp", 
                  "Misc. dinoflagellates", "Colonial greens", "Green flagellate", 
                  "Kirchneriella sp", "centric diatom sp", "pennate diatom sp", 
                  "chlorophyte  sp", "chlorophyte sp", "cyanophyte tetrad sp", 
                  "cyanophyte  sp", "cyanophyte sp", "cryptophyte sp", "Pseudanbaena sp", 
                  "microflagellate sp", "xanthophyte  sp", "Didymocystis sp", "euglenophyte sp", 
                  "Achnanthdium sp", "Coenocystis sp", "Mucidospaerium sp", "nostocalean filament sp", 
                  "cyanophyte filament sp", "Dictytospaerium sp", "Pseudostaurastum sp", 
                  "chrysophyte sp")
)
other.trees.df
is.na(other.trees.df$order)

GenusSpp.list2=rbind(GenusSpp.list2,
                     other.trees.df)
subset(GenusSpp.list2,is.na(class)==T)

GenusSpp.list2[is.na(GenusSpp.list2$class)==T,"class"]="unid"
GenusSpp.list2[is.na(GenusSpp.list2$order)==T,"order"]="unid"

subset(GenusSpp.list2,class=="unid")
subset(GenusSpp.list2,order=="unid")


## rejoin GenusSpp list with GenusSpp.list2 to match with main data 