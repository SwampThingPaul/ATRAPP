## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             62-day sampling
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

library(flextable)
library(magrittr)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_62day/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

## Other Functions
MDL_func=function(data,MDL,rng.val=TRUE){
  tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
  tmp=ifelse(tmp<MDL,MDL/2,tmp)
  if(rng.val==TRUE){print(range(tmp,na.rm=T))}
  return(tmp)
}

# MDLs --------------------------------------------------------------------
# units = mg/L
TP.MDL=0.7*0.001
SRP.MDL=TP.MDL
DP.MDL=TP.MDL
DOP.MDL=NA; # DP-SRP
PP.MDL=NA; # TP-DP
NOx.MDL=0.0004
NH4.MDL=0.0014
TN.MDL=0.004
SolN.MDL=0.004
Urea.MDL=0.03
TKN.MDL=NA# TN.MDL; # Calculated value TN+NH4
DON.MDL=NA; # Calculated value DN-(DIN+urea)
TOC.MDL=0.15
SolOC.MDL=0.15
Turb.MDL=2
Color.MDL=15
Chla.MDL=0.1

Al.MDL=0.04
Sb.MDL=0.02; #antimony
As.MDL=0.05
Ba.MDL=0.002
Cd.MDL=0.0002
Ca.MDL=0.5
Cr.MDL=0.01
Co.MDL=0.001
Cu.MDL=0.009
Fe.MDL=0.1
Mg.MDL=0.5
Mn.MDL=0.01
Mo.MDL=0.001
Ni.MDL=0.01
Pb.MDL=0.01
K.MDL=0.5
Se.MDL=0.05
Ag.MDL=0.01
Na.MDL=0.5
Zn.MDL=0.02
Hard.MDL=NA; # Calculated as 2.497 (Ca) + 4.119 (Mg)


# Other helpers -----------------------------------------------------------
consec.event=date.fun(c("2019-06-18","2019-08-19"))

# WQ Data -----------------------------------------------------------------
## From Nico
dat=read.csv(paste0(data.path,"PLSF_metadata_dailyandplus2019.csv"))
dat$Date=date.fun(dat$Date)
range(dat$Date)

ID_date=dat[,c("ID",'Date')]

## Make MDL/units table
field.param=c("Dissolved_oxygen_DO", "Dissolved_oxygen_DO_.",
              "Total_dissolved_solids","Specific_conductivity",
              "Phycocyanine._intake_Bedford_high_intesity",
              "Secchi", "Ambient_temperature", "Temperature_water", 
              "Average_wind_speed","pH")
calc.param=c("Dissolved_organic_P", "Particulate_P_PP",
             "Total_nitrogen_Kjeldahl","Soluble_organic_nitrogen_.DON",
             "Total_hardness_CaCO3")

MDL.tab=data.frame(variable=c("Aluminum", "Antimony", "Arsenic", "Amonia_nitrogen", "Soluble_nitrogen_DN", 
                              "Soluble_organic_nitrogen_.DON", "Total_nitrogen_Kjeldahl", "Total_nitrogen_TN", 
                              "Barium", "Cadmium", "Calcium", "Total_soluble_organic_carbon", 
                              "Total._organic_carbon", "Chrome", "Cobalt", "Specific_conductivity", 
                              "Copper", "Iron", "Magnesium", "Manganese", "Molybdenum", "Nickel", 
                              "Nitrite_nitrate", "SRP", "Dissolved_oxygen_DO", "Dissolved_oxygen_DO_.", 
                              "Dissolved_organic_P", "Dissolved_P_DP", "Particulate_P_PP", 
                              "Total_P_TP", "Lead", "Potassium", "Selenium", "Silver", "Sodium", 
                              "Total_dissolved_solids", "Total_hardness_CaCO3", "Turbidity", 
                              "Urea", "Zinc", "Chlorophylle_a", "Phycocyanine._intake_Bedford_high_intesity", 
                              "Secchi", "Ambient_temperature", "Temperature_water", "Average_wind_speed", 
                              "pH"),
                   MDL=c(Al.MDL,Sb.MDL,As.MDL,NH4.MDL/1000,SolN.MDL,
                         DON.MDL,TKN.MDL,TN.MDL,
                         Ba.MDL,Cd.MDL,Ca.MDL,SolOC.MDL,
                         TOC.MDL,Cr.MDL,Co.MDL,NA,
                         Cu.MDL,Fe.MDL,Mg.MDL,Mn.MDL,Mo.MDL,Ni.MDL,
                         NOx.MDL,SRP.MDL,NA,NA,
                         DOP.MDL,DP.MDL,PP.MDL,
                         TP.MDL,Pb.MDL,K.MDL,Se.MDL,Ag.MDL,Na.MDL,
                         NA,Hard.MDL,Turb.MDL,
                         Urea.MDL,Zn.MDL,Chla.MDL,NA,
                         NA,NA,NA,NA,NA),
                   Units=c("mgL","mgL","mgL","ugL","mgL",
                           "mgL","mgL","mgL",
                           "mgL","mgL","mgL","mgL",
                           "mgL","mgL","mgL","uScm",
                           "mgL","mgL","mgL","mgL","mgL","mgL",
                           "mgL","ugL","mgL","PerSat",
                           "ugL","ugL","ugL",
                           "ugL","mgL","mgL","mgL","mgL","mgL",
                           "mgL","mgL","NTU",
                           "mgL","mgL","ugL","ugL",
                           "cm","DegC","DegC","ms",
                           "SU"),
                   param=c("Al","Sb","As","NH4","DN",
                           "DON","TKN","TN",
                           "Ba","Cd","Ca","DOC",
                           "TOC","Cr","Co","SPC",
                           "Cu","Fe","Mg","Mn","Mo","Ni",
                           "NOx","SRP","DO","DO_PerSat",
                           "DOP","DP","PP",
                           "TP","Pb","K","Se","Ag","Na",
                           "TDS","Hard","Turb",
                           "Urea","Zn","Chla","Phyco",
                           "SDD","ATemp","WTemp","WindSp",
                           "pH"))
MDL.tab$group=NA
MDL.tab[MDL.tab$variable%in%field.param,"group"]="field"
MDL.tab[MDL.tab$variable%in%calc.param,"group"]="calc"
MDL.tab$group=with(MDL.tab,ifelse(is.na(group)==T,"lab",group))
# write.csv(MDL.tab,paste0(export.path,"metadata_MDLTable_62d.csv"),row.names = F)

### 
vars=names(dat)# [5:ncol(dat)]
dat.melt=melt(dat,id.vars = vars[1:4])
dat.melt=subset(dat.melt,is.na(value)==F)
dat.melt=merge(dat.melt,MDL.tab,"variable")

subset(dat.melt,value==0)
dat.melt$HalfMDL=with(dat.melt,ifelse(group=="field",value,ifelse(value<=MDL,MDL/2,value)))

plot(HalfMDL~value,subset(dat.melt,param=="TN"));abline(0,1)
### 
subset(MDL.tab,group=="calc")

dat.melt$param.unit=with(dat.melt,paste(param,Units,sep="."))
dat.xtab2=dcast(subset(dat.melt,!(group%in%c("calc"))),Date~param.unit,value.var = "HalfMDL",mean)

## Calcualted values
dat.xtab2$PP.ugL=with(dat.xtab2,TP.ugL-DP.ugL)
dat.xtab2$DOP.ugL=with(dat.xtab2,DP.ugL-SRP.ugL)

dat.xtab2$DIN.mgL=with(dat.xtab2,NOx.mgL+(NH4.ugL/1000))
dat.xtab2$TKN.mgL=with(dat.xtab2,TN.mgL+(NH4.ugL/1000))
dat.xtab2$DON.mgL=with(dat.xtab2,ifelse(is.na(Urea.mgL)==T,DN.mgL-DIN.mgL,DN.mgL-(DIN.mgL+Urea.mgL)))

# dat.xtab2$POC.mgL=with(dat.xtab2,TOC.mgL-DOC.mgL)

## Reversal screening
dat.xtab2$TPReversal=with(dat.xtab2,ifelse(is.na(SRP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(SRP.ugL>(TP.ugL*1.3),1,0)));
dat.xtab2$TNReversal=with(dat.xtab2,ifelse(is.na(DIN.mgL)==T|is.na(TN.mgL)==T,0,ifelse(DIN.mgL>(TN.mgL*1.3),1,0)));

sum(dat.xtab2$TPReversal); # should be zero
sum(dat.xtab2$TNReversal); # should be zero

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN.mgL~DIN.mgL,dat.xtab2,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP.ugL~SRP.ugL,dat.xtab2,ylab="TP (ug L\u207B\u00b9)",xlab="SRP (ug L\u207B\u00b9)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off()
# If reversal values not 0 then use script
# dat.xtab2$TP=with(dat.xtab2,ifelse(TPReversal==1,NA,TP))
# dat.xtab2$SRP=with(dat.xtab2,ifelse(TPReversal==1,NA,SRP))
# dat.xtab2$TN=with(dat.xtab2,ifelse(TNReversal==1,NA,TN))
# dat.xtab2$DIN=with(dat.xtab2,ifelse(TNReversal==1,NA,DIN))

## Stoichiometry
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107

dat.xtab2$TP.mM=with(dat.xtab2,(TP.ugL/1000)/P.mw)
dat.xtab2$TN.mM=with(dat.xtab2,TN.mgL/N.mw)
dat.xtab2$SRP.mM=with(dat.xtab2,(SRP.ugL/1000)/P.mw)
dat.xtab2$DIN.mM=with(dat.xtab2,DIN.mgL/N.mw)
dat.xtab2$TOC.mM=with(dat.xtab2,TOC.mgL/C.mw)
dat.xtab2$DOC.mM=with(dat.xtab2,DOC.mgL/C.mw)

dat.xtab2$TN_TP=with(dat.xtab2,TN.mM/TP.mM)
dat.xtab2$TOC_TP=with(dat.xtab2,TOC.mM/TP.mM)
dat.xtab2$TOC_TN=with(dat.xtab2,TOC.mM/TN.mM)
dat.xtab2$DIN_SRP=with(dat.xtab2,DIN.mM/SRP.mM)
dat.xtab2$DOC_SRP=with(dat.xtab2,DOC.mM/SRP.mM)
dat.xtab2$DOC_DIN=with(dat.xtab2,DOC.mM/DIN.mM)

consec.event=date.fun(c("2019-06-18","2019-08-19"))
diff(consec.event)

# Toxin data --------------------------------------------------------------
tox.mdl=read.xlsx(paste0(data.path,"LOD and LOQ toxins for Paul.xlsx"),sheet=2)

tox.dat=read.table(paste0(data.path,"PLSF_toxins_dailyandplus2019.txt"))
# tox.dat$Date=as.POSIXct(tox.dat$Date)
tox.dat$Date=date.fun(tox.dat$Date)

idvars=c('ID',"Date","Point","Site")
paramvars=names(tox.dat)[6:39]

tox.ls=strsplit(paramvars,"_")
tox.ls=data.frame(variable=paramvars,matrix=sapply(tox.ls,"[",1),tox=ifelse(sapply(tox.ls,"[",2)=="Cyn",
                                                                            sapply(tox.ls,"[",2),
                                                                            paste(sapply(tox.ls,"[",2),sapply(tox.ls,"[",3),sep="-")))
tox.ls=merge(tox.ls,tox.mdl,by.x=c('matrix','tox'),by.y=c('matrix','toxin'))

tox.dat.melt=melt(tox.dat,id.vars = idvars)
tox.dat.melt=merge(tox.dat.melt,tox.ls,"variable")

# if toxin detected value = 1 (removed for statistics)
tox.dat.melt$value=with(tox.dat.melt,ifelse(value==1,NA,value))
tox.dat.melt$HalfMDL=with(tox.dat.melt,ifelse(value<MDL|value==0,MDL/2,value))
unique(tox.dat.melt$Site)

tox.dat.xtab=dcast(tox.dat.melt,Date~variable,value.var = "HalfMDL",mean)

plot(extra_MC_LY_cell~Date,tox.dat.xtab)

plot(intra_MC_LR_cell~Date,tox.dat.xtab)
plot(intra_MC_LA_cell~Date,tox.dat.xtab)
plot(intra_MC_LF_cell~Date,tox.dat.xtab)
plot(intra_MC_LY_cell~Date,tox.dat.xtab)

MC.vars=c("Date",
          paste("intra_MC",c("HiIR","HtyR","LA","LF","LR","LW","LY","RR","WR","YR"),"cell",sep="_"),
          paste("extra_MC",c("HiIR","HtyR","LA","LF","LR","LW","LY","RR","WR","YR"),"cell",sep="_"))
MC.tox.dat.xtab=tox.dat.xtab[,MC.vars]
MC.tox.dat.xtab$intra_totalMC.ugL=rowSums(MC.tox.dat.xtab[,MC.vars[2:11]])*0.001
MC.tox.dat.xtab$extra_totalMC.ugL=rowSums(MC.tox.dat.xtab[,MC.vars[12:21]])*0.001

plot(intra_totalMC~Date,MC.tox.dat.xtab)
plot(extra_totalMC~Date,MC.tox.dat.xtab)
range(MC.tox.dat.xtab$intra_totalMC.ugL,na.rm=T)
range(MC.tox.dat.xtab$extra_totalMC.ugL,na.rm=T)

# dominate MC cong
names(sort(-rank(colMeans(MC.tox.dat.xtab[,MC.vars[2:11]],na.rm=T))))
names(sort(-rank(colMeans(MC.tox.dat.xtab[,MC.vars[12:21]],na.rm=T))))

plot(intra_MC_LR_cell~Date,MC.tox.dat.xtab)
plot(extra_MC_LR_cell~Date,MC.tox.dat.xtab)
# Genomic Data ------------------------------------------------------------
tax_tab <- read.table(paste0(data.path,"62Day_20221109/taxa_spe_phylo.csv"), header=T,row.names=1, check.names=F)
colnames(tax_tab) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus","Species")
tax_tab$sppID=row.names(tax_tab)

microbe<-read.table(paste0(data.path,"62Day_20221109/PLSF_species_daily_tax_phylo.csv"), row.names = 1, header=T)
count_phy=microbe
colnames(count_phy) <- sub("^X", "", colnames(count_phy))

count_phy$sppID=row.names(count_phy)
count_phy.melt=melt(count_phy,id.vars = "sppID")
count_phy.melt=merge(count_phy.melt,ID_date,by.x="variable",by.y="ID",all.x=T)
count_phy.melt$value=as.numeric(count_phy.melt$value)
count_phy1=dcast(count_phy.melt,variable+Date~sppID,value.var = "value",sum)

count_phy1$Sample_TotalSeqs <- rowSums(count_phy1[,3:ncol(count_phy1)])
count_phy1_spp=count_phy1[,3:(ncol(count_phy1)-1)]

seq_count=count_phy1[,c("variable","Sample_TotalSeqs")]
seq_count=seq_count[order(seq_count$Sample_TotalSeqs),]

# png(filename=paste0(plot.path,"PLSF_InLakeSeqCount.png"),width=8,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
ylim.val=c(0,6e7);by.y=1e7;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(seq_count$Sample_TotalSeqs,space=0,col="cornflowerblue",axes=F,ylim=ylim.val)
axis_fun(1,x,x,seq_count$variable,las=2,cex=0.5)
axis_fun(2,ymaj,ymin,ymaj/1e7);box(lwd=1)
mtext(side=1,line=2,"Sample ID")
mtext(side=2,line=2,"Number of Sequences per Sample (x10\u2077)")
dev.off()

## Based on Nico's code 
## (Dphyseq_filt_fin10 <- phyloseq::prune_taxa(phyloseq::taxa_sums(Dphyseq_meta) > 10, Dphyseq_meta))
(sum(colSums(count_phy1_spp)>10)/length(colSums(count_phy1_spp)))*100

# https://rdrr.io/rforge/vegan/man/rarefy.html
library(vegan)
S <- specnumber(count_phy1_spp) # observed number of species
(raremax <- min(rowSums(count_phy1_spp)))
Srare <- rarefy(count_phy1_spp, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(count_phy1_spp,  step=50000,sample = raremax, col = "blue", cex = 0.6)

count_phy1_spp_prune=count_phy1_spp[,colSums(count_phy1_spp)>10]
S2 <- specnumber(count_phy1_spp_prune) # observed number of species
(raremax2 <- min(rowSums(count_phy1_spp_prune)))
Srare2 <- rarefy(count_phy1_spp_prune, raremax)
plot(S2, Srare2, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(count_phy1_spp_prune, step=50000,sample = raremax2, col = "blue", cex = 0.6)

rare_slopes=rareslope(count_phy1_spp_prune, raremax)
plot(rare_slopes,S2)

count_phy1.prune=cbind(count_phy1[,1:2],count_phy1_spp_prune)
count_phy1.prune=rename(count_phy1.prune,c("variable"="sampleID"))
count_phy1.prune2=count_phy1.prune

## proportional abundance
x.val=apply(count_phy1.prune2[,3:ncol(count_phy1.prune2)],1,sum)
count_phy1.prune2[,3:ncol(count_phy1.prune2)]=sweep(count_phy1.prune2[,3:ncol(count_phy1.prune2)],1,x.val,"/")

## Diversity (alpha) Indices
# Shannon Index
shannon=-count_phy1.prune2[,3:ncol(count_phy1.prune2)]*log(count_phy1.prune2[,3:ncol(count_phy1.prune2)])
shannon_H=apply(shannon,1,sum,na.rm=T)

# Simpson
simpson_D=1-apply(count_phy1.prune2[,3:ncol(count_phy1.prune2)]^2,1,sum,na.rm=T)
simpson_invD=1/apply(count_phy1.prune2[,3:ncol(count_phy1.prune2)]^2,1,sum,na.rm=T)

plot(simpson_D)

#species richness
richness=specnumber(count_phy1.prune2[,3:ncol(count_phy1.prune2)])

# Pielou's evenness J' #https://www.rpubs.com/roalle/mres_2019
pielou_even=shannon_H/log(richness)



count_phy1.prune.melt=melt(count_phy1.prune2,id.vars = c("sampleID","Date"))
count_phy1.prune.melt.tax=merge(count_phy1.prune.melt,tax_tab,by.x="variable",by.y="sppID",all.x=T)

## top taxa plot like 
# top <- top_taxa(Dphyseq_filt_fin10, 
#                 tax_level = "Genus", 
#                 n_taxa = 10)
# plot_nested_bar(top$ps_obj, top_level = "Phylum", nested_level = "Genus")
tmp=dcast(count_phy1.prune.melt.tax,sampleID+Date~Genus,value.var = "value",sum)
top.names=names(sort(-rank(colSums(tmp[,3:ncol(tmp)])))[1:10])

tax_tab2=tax_tab
tax_tab2$group=with(tax_tab2,ifelse(Genus%in%top.names,Genus,"Other"))
tax_tab2$Phy.group=with(tax_tab2,ifelse(Genus%in%top.names,Phylum,"Other"))
tax_tab2$spp.group=with(tax_tab2,ifelse(Genus%in%top.names,Species,"Other"))
tax_tab2=tax_tab2[order(tax_tab2$Phylum,tax_tab2$Genus),]
unique(subset(tax_tab2,group!="Other")$Phylum)
unique(subset(tax_tab2,group!="Other")$Genus)

tmp2=dcast(subset(count_phy1.prune.melt.tax,Genus=="Microcystis"),
           sampleID+Date~Species,value.var = "value",sum)
MC.names=names(sort(-rank(colSums(tmp2[,3:ncol(tmp2)])))[1:3])
tax_tab2$spp.group2=with(tax_tab2,ifelse(is.na(Genus)==T,"Other",ifelse(Genus=="Microcystis",Species,"Other")))

genus.var=c("Anabaena","Dolichospermum","Microcystis","Synechococcus")
tax_tab2$genus.group3=with(tax_tab2,ifelse(is.na(Genus)==T,"Other",ifelse(Genus%in%genus.var,Genus,"Other")))

count_phy1.prune.melt.tax=merge(count_phy1.prune.melt,tax_tab2,by.x="variable",by.y="sppID",all.x=T)

top10_abund=dcast(count_phy1.prune.melt.tax,sampleID+Date~group,value.var = "value",sum)
top10_abund=top10_abund[,c("sampleID","Date","Other",unique(subset(tax_tab2,group!="Other")$Genus))]

MC_abund=dcast(count_phy1.prune.melt.tax,sampleID+Date~spp.group2,value.var = "value",sum)
MC_abund=MC_abund[,c("sampleID","Date","Other",unique(subset(tax_tab2,spp.group2!="Other")$spp.group2))]

fill.date=data.frame(Date=seq(min(top10_abund$Date),max(top10_abund$Date),"1 day"))
top10_abund=merge(top10_abund,fill.date,"Date",all.y=T)

top10_abund.wq=merge(top10_abund,dat.xtab2,'Date',all.x=T)
top10_abund.tox=merge(top10_abund,MC.tox.dat.xtab,'Date',all.x=T)

xlim.val=date.fun(c("2019-04-30","2019-10-29"));xmaj=seq(xlim.val[1],xlim.val[2],"4 weeks");xmin=seq(xlim.val[1],xlim.val[2],"1 week")
# png(filename=paste0(plot.path,"PLSF_62Day_top10.png"),width=10,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1.5,0.5,3.5),oma=c(1,2,0.75,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.5))

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=rev(c("grey80",
       colorRampPalette(c("dodgerblue4","dodgerblue"))(2),
       colorRampPalette(c("darkolivegreen4","darkolivegreen1"))(4),
       colorRampPalette(c("deeppink4","deeppink"))(4)))
x=barplot(t(top10_abund[,ncol(top10_abund):3]),beside=F,space=0,col=cols,border=cols,axes=F,yaxs="i",width=0.75,ylim=ylim.val)
# abline(v=x[which(fill.date$Date%in%consec.event)],col="black",lwd=2)
# axis_fun(1,x,x,format(top10_abund$Date,"%m-%d"),cex=0.75,las=2,line=-0.25)
axis_fun(1,x[which(fill.date$Date%in%xmaj)],x[which(fill.date$Date%in%xmin)],format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(3,x[which(fill.date$Date%in%consec.event)],x[which(fill.date$Date%in%consec.event)],c("Start",'End'),
         line=-0.75,maj.tcl = -0.3)
mtext(side=2,line=2,"Abundance")
mtext(side=1,line=2.5,'Date (M-D-2019)')

par(new=T)
ylim.val2=c(0,80);by.y2=20;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)
barplot(top10_abund.wq$DIN_SRP,ylim=ylim.val2,space=0,col=NA,border=NA,axes=F,yaxs="i",width=0.75)
pt_line(x[is.na(top10_abund.wq$DIN_SRP)==F],subset(top10_abund.wq,is.na(DIN_SRP)==F)$DIN_SRP,
        2,"red",1,21,"red")
pt_line(x[is.na(top10_abund.wq$TN_TP)==F],subset(top10_abund.wq,is.na(TN_TP)==F)$TN_TP,
        2,"black",1,21,"black")
axis_fun(4,ymaj2,ymin2,format(ymaj2));box(lwd=1)
mtext(side=4,line=2,"Molar Ratio")
leg.vals=ddply(tax_tab2,c("Phy.group","group"),summarise,N.val=N.obs(group))
leg.vals$Phy.group=factor(leg.vals$Phy.group,levels=c("Other",unique(subset(tax_tab2,group!="Other")$Phylum)))
leg.vals=leg.vals[order(leg.vals$Phy.group),]
leg.vals$group=gsub("_"," ",leg.vals$group)

par(mar=c(1,1.5,0.5,3.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.25,0.5,legend=paste(leg.vals$Phy.group,leg.vals$group,sep=" - "),
       pch=22,pt.bg=rev(cols),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Genus")
legend(0.75,0.5,legend=c("TN:TP","DIN:SRP"),
       pch=21,pt.bg=c("black","red"),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col="black",
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stoichiometry")
dev.off()

xlim.val=date.fun(c("2019-04-30","2019-10-29"));xmaj=seq(xlim.val[1],xlim.val[2],"4 weeks");xmin=seq(xlim.val[1],xlim.val[2],"1 week")
# png(filename=paste0(plot.path,"PLSF_62Day_top10_tox.png"),width=10,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1.5,0.5,4),oma=c(1,2,0.75,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.5))

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=rev(c("grey80",
           colorRampPalette(c("dodgerblue4","dodgerblue"))(2),
           colorRampPalette(c("darkolivegreen4","darkolivegreen1"))(4),
           colorRampPalette(c("deeppink4","deeppink"))(4)))
x=barplot(t(top10_abund[,ncol(top10_abund):3]),beside=F,space=0,col=cols,border=cols,axes=F,yaxs="i",width=0.75,ylim=ylim.val)
# abline(v=x[which(fill.date$Date%in%consec.event)],col="black",lwd=2)
# axis_fun(1,x,x,format(top10_abund$Date,"%m-%d"),cex=0.75,las=2,line=-0.25)
axis_fun(1,x[which(fill.date$Date%in%xmaj)],x[which(fill.date$Date%in%xmin)],format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(3,x[which(fill.date$Date%in%consec.event)],x[which(fill.date$Date%in%consec.event)],c("Start",'End'),
         line=-0.75,maj.tcl = -0.3)
mtext(side=2,line=2,"Abundance")
mtext(side=1,line=2.5,'Date (M-D-2019)')

par(new=T)
# ylim.val2=c(0.1,1e6);by.y2=20;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)
ylim.val2=c(0.01,200);ymaj2=log.scale.fun(ylim.val2,"major");ymin2=log.scale.fun(ylim.val2,"minor")
barplot(top10_abund.tox$extra_totalMC,ylim=ylim.val2,space=0,col=NA,border=NA,axes=F,yaxs="i",width=0.75,log="y")
pt_line(x[is.na(top10_abund.tox$extra_totalMC.ugL)==F],subset(top10_abund.tox,is.na(extra_totalMC.ugL)==F)$extra_totalMC.ugL,
        2,"red",1,21,"red")
pt_line(x[is.na(top10_abund.tox$intra_totalMC.ugL)==F],subset(top10_abund.tox,is.na(intra_totalMC.ugL)==F)$intra_totalMC.ugL,
        2,"black",1,21,"black")
axis_fun(4,ymaj2,ymin2,format(ymaj2,scientific = F));box(lwd=1)
mtext(side=4,line=3,"MC concentration (\u03BCg L\u207B\u00B9)")

leg.vals=ddply(tax_tab2,c("Phy.group","group"),summarise,N.val=N.obs(group))
leg.vals$Phy.group=factor(leg.vals$Phy.group,levels=c("Other",unique(subset(tax_tab2,group!="Other")$Phylum)))
leg.vals=leg.vals[order(leg.vals$Phy.group),]
leg.vals$group=gsub("_"," ",leg.vals$group)

par(mar=c(1,1.5,0.5,3.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.25,0.5,legend=paste(leg.vals$Phy.group,leg.vals$group,sep=" - "),
       pch=22,pt.bg=rev(cols),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Genus")
legend(0.75,0.5,legend=c("Total Intracelluar","Total Extracelluar"),
       pch=21,pt.bg=c("black","red"),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col="black",
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Microcystin Concentration")
dev.off()

MC_abund=merge(MC_abund,fill.date,"Date",all.y=T)
MC_abund.wq=merge(MC_abund,dat.xtab2,'Date',all.x=T)
MC_abund.tox=merge(MC_abund,MC.tox.dat.xtab,'Date',all.x=T)
# png(filename=paste0(plot.path,"PLSF_62Day_MC.png"),width=10,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1.5,0.5,3.5),oma=c(1,2,0.75,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.4))

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=rev(c("grey80",
           colorRampPalette(c("olivedrab","limegreen"))(3)))
x=barplot(t(MC_abund[,ncol(MC_abund):3]),beside=F,space=0,col=cols,border=cols,axes=F,yaxs="i",width=0.75,ylim=ylim.val)
# abline(v=x[which(fill.date$Date%in%consec.event)],col="black",lwd=2)
# axis_fun(1,x,x,format(MC_abund$Date,"%m-%d"),cex=0.75,las=2,line=-0.25)
axis_fun(1,x[which(fill.date$Date%in%xmaj)],x[which(fill.date$Date%in%xmin)],format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(3,x[which(fill.date$Date%in%consec.event)],x[which(fill.date$Date%in%consec.event)],c("Start",'End'),
         line=-0.75,maj.tcl = -0.3)
mtext(side=2,line=2,"Abundance")
mtext(side=1,line=2.5,'Date (M-D-2019)')

par(new=T)
ylim.val2=c(0,80);by.y2=20;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)
barplot(MC_abund.wq$DIN_SRP,ylim=ylim.val2,space=0,col=NA,border=NA,axes=F,yaxs="i",width=0.75)
pt_line(x[is.na(MC_abund.wq$DIN_SRP)==F],subset(MC_abund.wq,is.na(DIN_SRP)==F)$DIN_SRP,
        2,"red",1,21,"red")
pt_line(x[is.na(MC_abund.wq$TN_TP)==F],subset(MC_abund.wq,is.na(TN_TP)==F)$TN_TP,
        2,"black",1,21,"black")
axis_fun(4,ymaj2,ymin2,format(ymaj2));box(lwd=1)
mtext(side=4,line=2,"Molar Ratio")

leg.vals=c("Other", gsub("_"," ",unique(subset(tax_tab2,spp.group2!="Other")$spp.group2)))
par(mar=c(1,1.5,0.5,3.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.25,0.5,legend=c(leg.vals),
       pch=22,pt.bg=rev(cols),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Genus")
legend(0.75,0.5,legend=c("TN:TP","DIN:SRP"),
       pch=21,pt.bg=c("black","red"),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col="black",
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stoichiometry")
dev.off()

# png(filename=paste0(plot.path,"PLSF_62Day_MC_tox.png"),width=10,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1.5,0.5,3.5),oma=c(1,2,0.75,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.4))

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=rev(c("grey80",
           colorRampPalette(c("olivedrab","limegreen"))(3)))
x=barplot(t(MC_abund[,ncol(MC_abund):3]),beside=F,space=0,col=cols,border=cols,axes=F,yaxs="i",width=0.75,ylim=ylim.val)
# abline(v=x[which(fill.date$Date%in%consec.event)],col="black",lwd=2)
# axis_fun(1,x,x,format(MC_abund$Date,"%m-%d"),cex=0.75,las=2,line=-0.25)
axis_fun(1,x[which(fill.date$Date%in%xmaj)],x[which(fill.date$Date%in%xmin)],format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(3,x[which(fill.date$Date%in%consec.event)],x[which(fill.date$Date%in%consec.event)],c("Start",'End'),
         line=-0.75,maj.tcl = -0.3)
mtext(side=2,line=2,"Abundance")
mtext(side=1,line=2.5,'Date (M-D-2019)')

par(new=T)
# ylim.val2=c(0,80);by.y2=20;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)
ylim.val2=c(0.01,200);ymaj2=log.scale.fun(ylim.val2,"major");ymin2=log.scale.fun(ylim.val2,"minor")
barplot(MC_abund.tox$extra_totalMC,ylim=ylim.val2,space=0,col=NA,border=NA,axes=F,yaxs="i",width=0.75,log="y")
pt_line(x[is.na(MC_abund.tox$extra_totalMC.ugL)==F],subset(MC_abund.tox,is.na(extra_totalMC.ugL)==F)$extra_totalMC.ugL,
        2,"red",1,21,"red")
pt_line(x[is.na(MC_abund.tox$intra_totalMC.ugL)==F],subset(MC_abund.tox,is.na(intra_totalMC.ugL)==F)$intra_totalMC.ugL,
        2,"black",1,21,"black")
axis_fun(4,ymaj2,ymin2,format(ymaj2,scientific = F));box(lwd=1)
mtext(side=4,line=3,"MC concentration (\u03BCg L\u207B\u00B9)")

leg.vals=c("Other", gsub("_"," ",unique(subset(tax_tab2,spp.group2!="Other")$spp.group2)))
par(mar=c(1,1.5,0.5,3.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.25,0.5,legend=c(leg.vals),
       pch=22,pt.bg=rev(cols),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Genus")
legend(0.75,0.5,legend=c("Total Intracelluar","Total Extracelluar"),
       pch=21,pt.bg=c("black","red"),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col="black",
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Microcystin Concentration")
dev.off()



plot(Microcystis_aeruginosa~TN_TP,MC_abund.wq)
plot(Microcystis_aeruginosa~DIN_SRP,MC_abund.wq)

head(top10_abund.wq)
plot(Anabaena ~TN_TP,top10_abund.wq)
plot(Anabaena ~DIN_SRP,top10_abund.wq)

top10_abund.tox$log.intra_MC=log(top10_abund.tox$intra_totalMC.ugL)
tmp.mod=lm(Microcystis~log.intra_MC,top10_abund.tox)
gvlma::gvlma(tmp.mod)
layout(matrix(1:4,2,2));plot(tmp.mod)
dev.off()
plot(tmp.mod$model$Microcystis,tmp.mod$fitted.values);abline(0,1)

x.val=seq(min(top10_abund.tox$log.intra_MC,na.rm=T),max(top10_abund.tox$log.intra_MC,na.rm=T),length.out=100)
pred.mod=predict(tmp.mod,data.frame(log.intra_MC=x.val))
plot(Microcystis~intra_totalMC.ugL,top10_abund.tox)
lines(exp(x.val),pred.mod);# under predicts and loosely fits assumptions of the test

plot(Microcystis~extra_totalMC.ugL,top10_abund.tox)

plot(Microcystis_aeruginosa~intra_totalMC.ugL,MC_abund.tox)
plot(Microcystis_aeruginosa~extra_totalMC.ugL,MC_abund.tox)

plot(Microcystis_viridis~extra_totalMC.ugL,MC_abund.tox)


# RDA ---------------------------------------------------------------------
top_abund=dcast(count_phy1.prune.melt.tax,sampleID+Date~genus.group3,value.var = "value",sum)
top_abund=top_abund[,c("sampleID","Date","Other",unique(subset(tax_tab2,genus.group3!="Other")$genus.group3))]

barplot(t(top_abund[,ncol(top_abund):3]),beside=F,space=0)

top_abund2 <- top_abund# cbind(top_abund[,1:2],decostand(top_abund[,3:7], method='standardize'))

barplot(t(top_abund2[,ncol(top_abund2):3]),beside=F,space=0)

top_abund.wq=merge(top_abund,dat.xtab2,'Date',all.x=T)
plot(Microcystis~TN.mgL,top_abund.wq)
plot(Microcystis~DIN.mgL,top_abund.wq)
plot(Microcystis~Phyco.ugL,top_abund.wq)

plot(Phyco.ugL~TN.mgL,top_abund.wq)

## Data cleaning (omit NAs, 62 day period,etc )
wq.vars=c("SDD.cm",'ATemp.DegC',"DO_PerSat.PerSat","SPC.uScm","DN.mgL",'TN.mgL',"DP.ugL","TP.ugL",'SRP.ugL',"TN_TP")        
dat.xtab2[,c(wq.vars)]
top_abund2.wq=merge(top_abund2,dat.xtab2[,c("Date",wq.vars)],'Date',all.x=T)
top_abund2.wq=subset(top_abund2.wq,Date%in%seq(consec.event[1],consec.event[2],"1 days"))
top_abund2.wq=na.omit(top_abund2.wq)


## break data into env and spp data

env_st<-top_abund2.wq[,8:17]
abotu<-decostand(top_abund2.wq[,3:7], method='standardize')

## stepwise RDA
spe.rda <- rda(abotu~., data=env_st)
ordiR2step(rda(abotu~1, data=env_st), scope= formula(spe.rda), direction= "both", R2scope=TRUE, pstep=100)
vif.cca(spe.rda)


rda.step.vars=c("ATemp.DegC","SPC.uScm","SDD.cm","TN.mgL",'DP.ugL',"TN_TP")
env_st2=top_abund2.wq[,rda.step.vars]

spe.rda2 <- rda(abotu~., data=env_st2)
summary(spe.rda2, display=NULL)
vif.cca(spe.rda2)

anova.cca(spe.rda2, step=1000)
anova.cca(spe.rda2, by='axis', step=1000)
rslt.terms=anova.cca(spe.rda2, by='terms', step=1000)
(R2adj <- RsquareAdj(spe.rda2)$adj.r.squared)

rslt.terms=data.frame(rslt.terms)
rslt.terms$variables=rownames(rslt.terms)
rownames(rslt.terms)=1:nrow(rslt.terms)
rslt.terms=rslt.terms[,c(5,1:4)]
colnames(rslt.terms)=c("variable","df","var","fvalue","pvalue")

rslt.terms%>%
  flextable%>%
  colformat_double(j=3:4,digits=2,na_str = " ")%>%
  compose(j="pvalue",i=~pvalue<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="pvalue",i=~pvalue<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="pvalue",i=~pvalue<0.05)%>%
  compose(j="variable",i=~variable=="ATemp.DegC",value=as_paragraph('Temperature'))%>%
  compose(j="variable",i=~variable=="SPC.uScm",value=as_paragraph('Specific Conductance'))%>%
  compose(j="variable",i=~variable=="SDD.cm",value=as_paragraph('Secchi'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('Total Nitrogen'))%>%
  compose(j="variable",i=~variable=="DP.ugL",value=as_paragraph('Dissolved Phosphorus'))%>%
  compose(j="variable",i=~variable=="TN_TP",value=as_paragraph('TN:TP (molar rato)'))%>%
  set_header_labels("variable"="Parameter",
                    "df"="DF",
                    "var" = "Variance",
                    "fvalue" = "F",
                    "pvalue"="\u03C1-value")%>%
  width(width=c(1.5,0.5,0.75,0.75,0.75))%>%
  align(align="center",part="header")%>%
  align(j=2:5,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  footnote(j=1,ref_symbols = " ",part="header",
           value=as_paragraph("R\u00B2 Adj: ",format(round(R2adj,2),nsmall=2)))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=11,part="all")%>%
  fontsize(size=12,part="header") #%>%print("docx")


eig <- spe.rda2$CA$eig
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca

# png(filename=paste0(plot.path,"PLSF_62d_RDA_scree.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2,0.75,1),oma=c(2,1,0.25,0.5));

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2)
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n")
abline(h=ymaj,lty=3,col="grey")
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n",add=T)
abline(h=1,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.5,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
#mtext(side=1,line=1.5,"Principal Components")
mtext(side=2,line=1.5,"Eigenvalue")

ylim.val=c(0,120);by.y=25;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2);#set y limit and delineates the major and minor ticks
x=barplot(eig.pca$variance,ylim=ylim.val,col="white",border=0,yaxt="n")# inital plot to get the measurements
abline(h=ymaj,lty=3,col="grey")#makes vertical lines from y axis
x=barplot(eig.pca$variance,ylim=ylim.val,col="grey",yaxt="n",add=T)# the real plot that matters
lines(x,eig.pca$cumvariance,col="indianred1",lwd=2)# adds the cumulative variance for each factor
points(x,eig.pca$cumvariance,pch=21,bg="indianred1",cex=1.25)
abline(h=80,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.5,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
mtext(side=1,line=1.5,"Components")
mtext(side=2,line=1.75,"Percentage of Variances")
legend.text=c("Absolute","Cumulative");#helper vaiable for legend
pt.col=c("grey","indianred1")#helper vaiable for legend
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col=c("black",pt.col[2]),lty=c(0,1),lwd=1.5,pt.cex=1,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col="black",lty=0,lwd=0.5,pt.cex=1,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()

MC.dom=with(top_abund2.wq,ifelse(Microcystis>0.75,"MCDom","other"))
tmp=top_abund2.wq$Date# seq(consec.event[1],consec.event[2],"1 days")
i1=length(tmp)/3
# date.cat=split(tmp, rep(1:3, c(ceiling(i1), round(i1), floor(i1))))
date.cat=sort((seq_along(top_abund2.wq$Date)-1)%%3)
date.cat.labs=ddply(data.frame(Date=top_abund2.wq$Date,cat=date.cat),"cat",summarise,
                    min.val=min(Date),max.val=max(Date))

plot(spe.rda2)
ordiellipse(spe.rda2,MC.dom)

plot(spe.rda2)
ordiellipse(spe.rda2,date.cat,col=c("green","blue","red"),draw="polygon")

scrs<-scores(spe.rda2,display=c("sites","species"),choices=c(1,2,3));
scrs.arrows<-scores(spe.rda2,choices=c(1,2,3),"bp");

labs=rownames(scrs$species)
labs.arrows=rownames(scrs.arrows)
labs.arrows=c("Temp","SPC",'Secchi','TN',"DP","TN:TP")

dev.off()

time.cols=c("blue",'grey','firebrick1')
# png(filename=paste0(plot.path,"PLSF_62d_RDA1.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));
# layout(matrix(1:2,2,1),heights=c(1,0.25))

xlim.val=c(-2,2);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-1.5,1.5);by.y=0.5;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
x=ordiellipse(spe.rda2,date.cat,col=time.cols,draw="polygon",alpha=0.25*255,border="grey")
# points(scrs$sites[,c(1,2)],pch=21,bg="grey80",col="grey40",cex=1,lwd=0.5);
points(scrs$sites[,c(1,2)],pch=21,bg=with(top_abund2.wq,ifelse(Microcystis>0.5,"lightgreen","grey80")),col="grey40",cex=1,lwd=0.5);
arrows(0,0,scrs.arrows[,1],scrs.arrows[,2],length = 0.05, angle = 15, code = 2,col="indianred2",lwd=1.5);
text(scrs.arrows[,1],scrs.arrows[,2],labels=labs.arrows,cex=0.75,font=1,col="red")
with(scrs,text(species[,1],species[,2],labels=labs,cex=0.8,font=2,col="black"))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); 
mtext(side=1,line=1.5,paste0("RDA 1 (",round(eig.pca$variance[1],1),"%)"));
mtext(side=2,line=2.25,paste0("RDA 2 (",round(eig.pca$variance[2],1),"%)"));
box(lwd=1)

# par(mar=c(0.1,2,1,0.5))
# plot(0:1,0:1,axes=F,ann=F,type="n")
# date.cat.labs.leg=with(date.cat.labs,paste0("Period ",1:3," (",format(min.val,"%d %b")," - ",format(max.val,"%d %b"),')'))
date.cat.labs.leg=with(date.cat.labs,paste0(c("Beginning","Middle","End")," (",format(min.val,"%d %b")," - ",format(max.val,"%d %b"),')'))
legend("topleft",legend=c(date.cat.labs.leg,"Microcystin spp > 0.5"),
       pch=21,pt.bg=c(adjustcolor(time.cols,0.25),"lightgreen"),
       lty=NA,lwd=c(0.1),col="grey",
       pt.cex=c(2,2,2,1),ncol=1,cex=0.8,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)

dev.off()

# PCoA
# from https://archetypalecology.wordpress.com/2018/02/19/principal-coordinates-analysis-pcoa-in-r/
library(ecodist)

# using proportional data
top_abund.dist=vegdist(top_abund2.wq[,3:7],method="bray")
range(top_abund.dist)
pcoa.top_abund=pco(top_abund.dist,negvals = "zero",dround=0)

plot(pcoa.top_abund$vectors[,1], pcoa.top_abund$vectors[,2], type = "n", xlab = "PCoA1", ylab = "PCoA2",
     axes = TRUE, main = "PCoA (ecodist) on varespec data")
text(pcoa.top_abund$vectors[,1], pcoa.top_abund$vectors[,2], labels(top_abund.dist), 
     cex = 0.9, xpd = TRUE)

pcoa.top_abund$values # eigenvalue for each component. This is a measure of the variance explained by each dimension
pcoa.top_abund$vectors # eigenvectors. Each column contains the scores for that dimension.


## Using ape library
library(ape)

# Principal coordinate analysis and simple ordination plot
res=pcoa(top_abund.dist)
res$values

biplot(res)

# Project unstandardized and standardized species on the PCoA ordination plot
top_abund.st=apply(top_abund2.wq[,3:7],2,scale,center=T,scale=T)

biplot(res, top_abund.st)

## using Vegan package 
## https://fromthebottomoftheheap.net/slides/intro-vegan-webinar-2020/intro-to-vegan.html#56
# top_abund2.wq$days.event=((top_abund2.wq$Date-consec.event[1])/86400)+1
top_abund.dist=vegdist(top_abund2.wq[,3:7],method="bray")

pco1=wcmdscale(top_abund.dist,eig=T)
pco1
round(eigenvals(pco1),3)

pco2=wcmdscale(top_abund.dist,eig=T,add="lingoes")
pco2
round(eigenvals(pco2),3)

plot(pco2)
# ordiellipse(pco1,date.cat,col=time.cols,draw="polygon",alpha=0.25*255,border="grey")

#get PCoA scores
scrs=scores(pco1)
#weighted by abundance
spp_scrs=wascores(scrs,top_abund2.wq[,3:7],expand=F)

eig <- as.numeric(eigenvals(pco1))
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca

# png(filename=paste0(plot.path,"PLSF_62d_PCoA.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));
# layout(matrix(1:2,2,1),heights=c(1,0.25))

xlim.val=c(-0.6,0.6);by.x=0.2;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-0.4,0.4);by.y=0.2;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
x=ordiellipse(pco1,date.cat,col=time.cols,draw="polygon",alpha=0.25*255,border="grey")
# points(scrs$sites[,c(1,2)],pch=21,bg="grey80",col="grey40",cex=1,lwd=0.5);
points(scrs[,c(1,2)],pch=21,bg=with(top_abund2.wq,ifelse(Microcystis>0.5,"lightgreen","grey80")),col="grey40",cex=1,lwd=0.5);
text(spp_scrs,row.names(spp_scrs),cex=0.8,font=2,col="black")
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); 
mtext(side=1,line=1.5,"Dim 1");
mtext(side=2,line=2.25,"Dim 2");
box(lwd=1)

date.cat.labs.leg=with(date.cat.labs,paste0(c("Beginning","Middle","End")," (",format(min.val,"%d %b")," - ",format(max.val,"%d %b"),')'))
legend("topleft",legend=c(date.cat.labs.leg,"Microcystin spp > 50%"),
       pch=21,pt.bg=c(adjustcolor(time.cols,0.25),"lightgreen"),
       lty=NA,lwd=c(0.1),col="grey",
       pt.cex=c(2,2,2,1),ncol=1,cex=0.8,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()

## hierarchical clustering

top_abund.clust=hclust(top_abund.dist,method="average")
top_abund.clust$labels=c("begining","middle","end")[date.cat]
plot(top_abund.clust)


## Kmeans
dat.clr=compositions::clr(top_abund2.wq[,3:7])

# from kmeans help
cl=kmeans(dat.clr,5)
plot(dat.clr,col=cl$cluster)
##? no idea


## END --------------------------------------------------------------------
### 
MC_abund=na.omit(MC_abund)
barplot(t(MC_abund[,ncol(MC_abund):3]),beside=F,space=0)

MC_abund2 <- cbind(MC_abund[,1:2],decostand(MC_abund[,3:6], method='standardize'))
barplot(t(top_abund2[,ncol(top_abund2):3]),beside=F,space=0)

MC_abund.wq2=merge(MC_abund2,dat.xtab2,'Date',all.x=T)
plot(Microcystis_aeruginosa~TN.mgL,MC_abund.wq2)
plot(Microcystis_aeruginosa~DIN.mgL,MC_abund.wq2)
plot(Microcystis_aeruginosa~Phyco.ugL,MC_abund.wq2)

## Data cleaning (omit NAs, 62 day period,etc )
wq.vars=c("SDD.cm",'ATemp.DegC',"DO_PerSat.PerSat","SPC.uScm","DN.mgL",'TN.mgL',"DP.ugL","TP.ugL",'SRP.ugL',"TN_TP")        
dat.xtab2[,c(wq.vars)]
MC_abund.wq2=merge(MC_abund2,dat.xtab2[,c("Date",wq.vars)],'Date',all.x=T)
MC_abund.wq2=subset(MC_abund.wq2,Date%in%seq(consec.event[1],consec.event[2],"1 days"))
MC_abund.wq2=na.omit(MC_abund.wq2)

## break data into env and spp data
env_st<-MC_abund.wq2[,7:16]
abotu<-MC_abund.wq2[,3:6]

## stepwise RDA
spe.rda <- rda(abotu~., data=env_st)
ordiR2step(rda(abotu~1, data=env_st), scope= formula(spe.rda), direction= "forward", R2scope=TRUE, pstep=1000)

rda.step.vars=c("SPC.uScm","SDD.cm",'TN.mgL',"DP.ugL")
env_st2=MC_abund.wq2[,rda.step.vars]

spe.rda2 <- rda(abotu~., data=env_st2)
summary(spe.rda2, display=NULL)
vif.cca(spe.rda2)

anova.cca(spe.rda2, step=1000)
anova.cca(spe.rda2, by='axis', step=1000)
anova.cca(spe.rda2, by='terms', step=1000)
(R2adj <- RsquareAdj(spe.rda2)$adj.r.squared)
