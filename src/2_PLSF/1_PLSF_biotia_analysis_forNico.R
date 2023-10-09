## Title:      Petit-lac-Saint-François - biotia changes
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 03/20/2023
## Modified version of 1_PLSF_biotia_analysis_V2.R for publication. 
##  changes include reorganization and clean-up of commented out code/comments

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
library(ggplot2)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_Paper2/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

# WQ Data -----------------------------------------------------------------
MDL_func=function(data,MDL,rng.val=TRUE){
  tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
  tmp=ifelse(tmp<MDL,MDL/2,tmp)
  if(rng.val==TRUE){print(range(tmp,na.rm=T))}
  return(tmp)
}

dat=read.xlsx(paste0(data.path,"PLSF Database-12 Years (v2021-07-07).xlsx"))
dat$Date=date.fun(convertToDate(dat$Date))

# Water quality specific parameters
wq.dat=dat[,c(1:20,364:374,387)]
names(wq.dat)

# all WQ Vars
wq.vars=c("Date", "Site", "ENKI", "N_P", "TP.mgL", 
          "PP.calc.mgL", "DP.mgL","SRP.mgL", "DOP.calc.mgL", 
          "TN.mgL", "TKN.mgL", "NH4.mgL","NOx.mgL", "Urea.mgL", "DON.mgL", 
          "SolN.mgL", "SolOC.mgL", "TOC.mgL", 
          "pH", "Chla.ugL", "Cond", "DO.per", 
          "TDS.mgL", "Temp.C", "ORP.mV", "Sal",
          "Resistivity.ohm","Phyco.ugL", "TChl.ugL", 
          "Turb.NTU", "Colour_PCU","Secchi_cm")
colnames(wq.dat)=wq.vars

plot(SolN.mgL~Date,wq.dat)

## data handling
TP.MDL=0.7*0.001
SRP.MDL=TP.MDL
DP.MDL=TP.MDL
NOx.MDL=0.0004
NH4.MDL=0.0014
TN.MDL=0.004
Chla.MDL=0.1
Turb.MDL=2

wq.dat$TP.ugL=with(wq.dat,MDL_func(TP.mgL,TP.MDL)*1000)
wq.dat$SRP.ugL=with(wq.dat,MDL_func(SRP.mgL,SRP.MDL)*1000)
wq.dat$DP.ugL=with(wq.dat,MDL_func(DP.mgL,DP.MDL)*1000)

wq.dat$NOx.mgL=with(wq.dat,MDL_func(NOx.mgL,NOx.MDL))
wq.dat$NH4.mgL=with(wq.dat,MDL_func(NH4.mgL,NH4.MDL))
wq.dat$TN.mgL=with(wq.dat,MDL_func(TN.mgL,TN.MDL))
wq.dat$Chla.ugL=with(wq.dat,MDL_func(Chla.ugL,Chla.MDL))
wq.dat$Turb.NTU=with(wq.dat,MDL_func(Turb.NTU,Turb.MDL))

wq.dat$DIN.mgL=with(wq.dat,NOx.mgL+NH4.mgL)
wq.dat$DOP.ugL=with(wq.dat,ifelse(DP.ugL-SRP.ugL<0|DP.ugL-SRP.ugL==0,DP.MDL,DP.ugL-SRP.ugL))

## Finding high sediment outliers (as communicated by Barry)
plot(TP.ugL~Turb.NTU,wq.dat)
plot(TN.mgL~Turb.NTU,wq.dat)

# sampling error/outlier?
turb.outliers=subset(wq.dat,Turb.NTU>100)
High.TPTN=subset(wq.dat,TP.ugL>5000)

pre.out.screen=nrow(wq.dat)
wq.dat=subset(wq.dat,Turb.NTU<100|is.na(Turb.NTU)==T)
wq.dat=subset(wq.dat,TP.ugL<5000|is.na(TP.ugL)==T)

pre.out.screen-nrow(wq.dat)

# P reversal check
wq.dat$TPReversal=with(wq.dat,ifelse(is.na(SRP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(SRP.ugL>(TP.ugL*1.3),1,0)));
sum(wq.dat$TPReversal,na.rm=T)
subset(wq.dat,TPReversal==1)
plot(TP.ugL~SRP.ugL,wq.dat,ylab="TP (\u03BCg L\u207B\u00b9)",xlab="SRP (\u03BCg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

# removed TP values with reversal
wq.dat[wq.dat$TPReversal==1,c("TP.ugL","SRP.ugL","DP.ugL","DOP.ugL")]=NA

## TN Reversal Check
nrow(subset(wq.dat,is.na(NOx.mgL)==F&is.na(NH4.mgL)==F&is.na(Urea.mgL)==F))
nrow(subset(wq.dat,is.na(NOx.mgL)==F&is.na(NH4.mgL)==F|is.na(Urea.mgL)==T))

wq.dat$TNReversal=with(wq.dat,ifelse(is.na(DIN.mgL)==T|is.na(TN.mgL)==T,0,ifelse(DIN.mgL>(TN.mgL*1.3),1,0)));
sum(wq.dat$TNReversal,na.rm=T)
subset(wq.dat,TNReversal==1)
plot(TN.mgL~DIN.mgL,wq.dat,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TNReversal==1,"blue",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")
wq.dat[wq.dat$TNReversal==1,c("TN.mgL","DIN.mgL","NOx.mgL","NH4.mgL")]=NA

pre.out.screen-nrow(wq.dat)

# 
wq.dat$TON.mgL=with(wq.dat,ifelse(TN.mgL-DIN.mgL<TN.MDL,TN.MDL/2,TN.mgL-DIN.mgL))

## Stoichiometry
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107

wq.dat$TP.mM=with(wq.dat,(TP.ugL/1000)/P.mw)
wq.dat$TN.mM=with(wq.dat,TN.mgL/N.mw)
wq.dat$SRP.mM=with(wq.dat,(SRP.ugL/1000)/P.mw)
wq.dat$DIN.mM=with(wq.dat,DIN.mgL/N.mw)
wq.dat$TON.mM=with(wq.dat,TON.mgL/N.mw)
wq.dat$TOC.mM=with(wq.dat,TOC.mgL/C.mw)

wq.dat$TOC_TON=with(wq.dat,TOC.mM/TON.mM)
wq.dat$TN_TP=with(wq.dat,TN.mM/TP.mM)
wq.dat$DIN_SRP=with(wq.dat,DIN.mM/SRP.mM)

unique(wq.dat$Site)
vars=c("Date",
       "Temp.C","Cond","Turb.NTU","DO.per",
       "TP.ugL","SRP.ugL","DP.ugL","DOP.ugL",
       "TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL","TON.mgL",
       "TN_TP","DIN_SRP")
wq.dat=subset(wq.dat,Site=="Lake_Outlet")[,vars]

wq.dat.melt=melt(wq.dat,id.vars = "Date")
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)
wq.dat.melt$month=as.numeric(format(wq.dat.melt$Date,"%m"))
wq.dat.melt$CY=as.numeric(format(wq.dat.melt$Date,"%Y"))
wq.dat.melt$monCY.date=with(wq.dat.melt,paste(CY,month,"01",sep="-"))

wq.dat.month=dcast(wq.dat.melt,CY+month+monCY.date~variable,value.var = "value",mean)
na.omit(wq.dat.month)

head(wq.dat.month)
range(subset(wq.dat.month,is.na(TN.mgL)==F)$monCY.date)
range(subset(wq.dat.month,is.na(NOx.mgL)==F)$monCY.date)
range(subset(wq.dat.month,is.na(NH4.mgL)==F)$monCY.date)


# Biotic data -------------------------------------------------------------
plsf.phyto=read.csv(paste0(export.path,"PLSF_microscope_data_phyto.csv"))
plsf.phyto$date=date.fun(plsf.phyto$date)
plsf.phyto$month=as.numeric(format(plsf.phyto$date,"%m"))
plsf.phyto$CY=as.numeric(format(plsf.phyto$date,"%Y"))

## Just for summer season
plsf.phyto=subset(plsf.phyto,month%in%seq(5,10,1))

spp.class.order=read.csv(paste0(export.path,"20230317_GenusSpp_ClassOrder.csv"))
head(spp.class.order)

spp.class.order=rbind(spp.class.order,
                      data.frame(GenusSpp="Cryptomonas marsonii",
                                 genus="Cryptomonas",
                                 order="Cryptomonadales",
                                 class="Cryptophyceae"))


subset(spp.class.order,GenusSpp=="nostocalean filament sp.")
diazotroph.genera=c("Dolichospermum", "Anabaena", "Aphanizomenon", "Cuspidothrix", 
                    "Aphanocapsa", "Aphanothece", "Komvophoron", "Oscillatoria", 
                    "Phormidium", "Planktolyngbya", "Pseudanabaena", "Sphaerospermopsis", 
                    "Synechococcus", "Synechocystis")
# nostocalean filament sp. also a diazatrophic spp
nondiazotroph.genera=c("Chroococcus", "Limnococcus", "Eucapsis", "Coelomoron", "Coelosphaerium", 
  "Cyanocatena", "Cyanodictyon", "Cyanogranis", "Merismopedia", 
  "Microcystis", "Planktothrix", "Snowella", "Spirulina", "Woronichinia"
)
other.nondiaz.GenusSpp=c("Cyanophyte colony", "cyanophyte colony sp.", "cyanophyte cell pair spp.", 
                "cyanophyte filament sp.", "cyanophyte unicell, oval/rod  2.5-5um spp.", 
                "cyanophyte unicell, sphere  2.5-5um spp.", "cyanophyte unicelll, oval/rod  2.5-5um spp.", 
                "cyanophyte cell pair, oval/rod spp.", "cyanophyte tetrad spp."
)

spp.class.order$diaztroph=with(spp.class.order,ifelse(genus%in%diazotroph.genera|
                                                        GenusSpp=="nostocalean filament sp.",1,
                                                      ifelse(genus%in%nondiazotroph.genera|
                                                               GenusSpp%in%other.nondiaz.GenusSpp,0,NA)))
spp.class.order$diaztroph.fac=with(spp.class.order,ifelse(is.na(diaztroph)==T,"NoClass",
                                                          ifelse(diaztroph==1,"diaz",
                                                          ifelse(diaztroph==0,"nondiaz",NA))))

unique(spp.class.order$class)
spp.class.order$class[spp.class.order$class=="Cyanobacteriia"]="Cyanophyceae"

plsf.phyto$GenusSpp=trimws(plsf.phyto$GenusSpp)
plsf.phyto=merge(plsf.phyto,spp.class.order,"GenusSpp",all.x=T)
sum(is.na(plsf.phyto$diaztroph.fac))

plsf.phyto$diaztroph.fac_cyano=with(plsf.phyto,
                                         ifelse(class=="Cyanophyceae",
                                                paste0("cyano_",diaztroph.fac),
                                                paste0("Other_",diaztroph.fac)))

subset(plsf.phyto,CY==2017)
plsf.phyto2=ddply(plsf.phyto,c("CY","month","GenusSpp","genus","order","class",
                               "diaztroph.fac","diaztroph.fac_cyano"),
                  summarise,
                  mean.biovol.um3mL=mean(totbiovol.um3mL,na.rm=T),
                  mean.conc.cellsmL=mean(Conc.cellsmL,na.rm=T))


## rel abund vs biovol ----------------------------------------------------
phyto.dat.conc.xtab=dcast(plsf.phyto,date~GenusSpp,value.var = "Conc.cellsmL",sum)
x.val=apply(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)],1,sum)
phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)]=sweep(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)],1,x.val,"/")

## Diversity (alpha) Indices
# Shannon Index
shannon=-phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)]*log(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)])
shannon_H=apply(shannon,1,sum,na.rm=T)

# Simpson
simpson_D=1-apply(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)]^2,1,sum,na.rm=T)
simpson_invD=1/apply(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)]^2,1,sum,na.rm=T)

plot(simpson_D)

#species richness
richness=specnumber(phyto.dat.conc.xtab[,2:ncol(phyto.dat.conc.xtab)])
plot(richness)

# Pielou's evenness J' #https://www.rpubs.com/roalle/mres_2019
pielou_even=shannon_H/log(richness)

phyto.dat.conc.xtab=cbind(phyto.dat.conc.xtab,
                          shannon_H,simpson_D,simpson_invD,richness,pielou_even)
phyto.dat.diversity=phyto.dat.conc.xtab[,c("date","shannon_H","simpson_D","simpson_invD","richness","pielou_even")]

## Relative biovolume
phyto.dat.biovol.xtab=dcast(plsf.phyto,date~GenusSpp,value.var = "totbiovol.um3mL",sum,na.rm=T)
x.val=apply(phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)],1,sum)
phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)]=sweep(phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)],1,x.val,"/")

## Diversity (alpha) Indices
# Shannon Index
shannon=-phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)]*log(phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)])
shannon_H=apply(shannon,1,sum,na.rm=T)

# Simpson
simpson_D=1-apply(phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)]^2,1,sum,na.rm=T)
simpson_invD=1/apply(phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)]^2,1,sum,na.rm=T)

plot(simpson_D)

#species richness
richness=specnumber(phyto.dat.biovol.xtab[,2:ncol(phyto.dat.biovol.xtab)])
plot(richness)

# Pielou's evenness J' #https://www.rpubs.com/roalle/mres_2019
pielou_even=shannon_H/log(richness)

phyto.dat.biovol.xtab=cbind(phyto.dat.biovol.xtab,
                            shannon_H,simpson_D,simpson_invD,richness,pielou_even)

plot(phyto.dat.conc.xtab$shannon_H~phyto.dat.biovol.xtab$shannon_H);abline(0:1)
plot(phyto.dat.conc.xtab$simpson_D~phyto.dat.biovol.xtab$simpson_D);abline(0:1)
plot(phyto.dat.conc.xtab$simpson_invD~phyto.dat.biovol.xtab$simpson_invD);abline(0:1)
plot(phyto.dat.conc.xtab$richness~phyto.dat.biovol.xtab$richness);abline(0:1)
plot(phyto.dat.conc.xtab$pielou_even~phyto.dat.biovol.xtab$pielou_even);abline(0:1)

## Phytonplankton Turnover ------------------------------------------------
library(codyn)
dput(names(phyto.dat.conc.xtab))

vars=names(phyto.dat.conc.xtab)[!(names(phyto.dat.conc.xtab)%in%c("shannon_H","simpson_D","simpson_invD","richness","pielou_even"))]
phyto.dat.conc.melt=melt(phyto.dat.conc.xtab[,vars],id.vars="date")
phyto.dat.conc.melt$dec.date=lubridate::decimal_date(phyto.dat.conc.melt$date)

total.to=turnover(phyto.dat.conc.melt,
                  time.var = "dec.date",
                  species.var="variable",
                  abundance.var="value",
                  metric="total")
appear.to=turnover(phyto.dat.conc.melt,
                   time.var = "dec.date",
                   species.var="variable",
                   abundance.var="value",
                   metric="appearance")
disappear.to=turnover(phyto.dat.conc.melt,
                      time.var = "dec.date",
                      species.var="variable",
                      abundance.var="value",
                      metric="disappearance")

turnover.spp.conc=merge(total.to,appear.to,c("dec.date"))
turnover.spp.conc=merge(turnover.spp.conc,disappear.to,c("dec.date"))

plot(total~dec.date,turnover.spp.conc,type="b")
plot(appearance~dec.date,turnover.spp.conc,type="b",pch=21,bg="Red")
plot(disappearance~dec.date,turnover.spp.conc,type="b",pch=21,bg="lightgreen")

with(turnover.spp.conc,cor.test(total,dec.date,method="kendall"))
with(turnover.spp.conc,cor.test(appearance,dec.date,method="kendall"))
with(turnover.spp.conc,cor.test(disappearance,dec.date,method="kendall"))

## Biovolume
vars=names(phyto.dat.biovol.xtab)[!(names(phyto.dat.biovol.xtab)%in%c("shannon_H","simpson_D","simpson_invD","richness","pielou_even"))]
phyto.dat.biovol.melt=melt(phyto.dat.biovol.xtab[,vars],id.vars="date")
phyto.dat.biovol.melt$dec.date=lubridate::decimal_date(phyto.dat.biovol.melt$date)

total.to=turnover(phyto.dat.biovol.melt,
                  time.var = "dec.date",
                  species.var="variable",
                  abundance.var="value",
                  metric="total")
appear.to=turnover(phyto.dat.biovol.melt,
                   time.var = "dec.date",
                   species.var="variable",
                   abundance.var="value",
                   metric="appearance")
disappear.to=turnover(phyto.dat.biovol.melt,
                      time.var = "dec.date",
                      species.var="variable",
                      abundance.var="value",
                      metric="disappearance")

turnover.spp.biovol=merge(total.to,appear.to,c("dec.date"))
turnover.spp.biovol=merge(turnover.spp.biovol,disappear.to,c("dec.date"))

plot(total~dec.date,turnover.spp.biovol,type="b")
plot(appearance~dec.date,turnover.spp.biovol,type="b",pch=21,bg="Red")
plot(disappearance~dec.date,turnover.spp.biovol,type="b",pch=21,bg="lightgreen")

with(turnover.spp.biovol,cor.test(total,dec.date,method="kendall"))
with(turnover.spp.biovol,cor.test(appearance,dec.date,method="kendall"))
with(turnover.spp.biovol,cor.test(disappearance,dec.date,method="kendall"))
### At species level data too variable to detect trends

tmp=merge(turnover.spp.biovol,turnover.spp.conc,"dec.date")
head(tmp)
plot(total.x~total.y,tmp);abline(0,1)
plot(appearance.x~appearance.y,tmp);abline(0,1)
plot(disappearance.x~disappearance.y,tmp);abline(0,1)

## very little dfference between relative biovol and rel conc in turnover estimates

head(phyto.dat.biovol.melt)
phyto.dat.biovol.melt=subset(phyto.dat.biovol.melt,as.numeric(format(date,"%Y"))>2009)
rankshift.vals=rank_shift(phyto.dat.biovol.melt,
                          time.var = "dec.date",
                          species.var="variable",
                          abundance.var="value")
str.val=strsplit(as.character(rankshift.vals$year_pair),"-")
rankshift.vals$t2=lubridate::date_decimal(as.numeric(sapply(str.val,"[",2)))

plot(MRS~t2,rankshift.vals)

### Abundance - Class ------------------------------------------------------
plsf.phyto.class=ddply(plsf.phyto2,c("CY","month","class"),summarise,
                       sum.biovol.um3mL=sum(mean.biovol.um3mL,na.rm=T),
                       sum.conc.cellsmL=sum(mean.conc.cellsmL,na.rm=T))
plsf.phyto.class$log10.biovol=with(plsf.phyto.class,ifelse(sum.biovol.um3mL==0,0,log10(sum.biovol.um3mL)))

plsf.phyto.class.xtab=dcast(plsf.phyto.class,CY+month~class,value.var="sum.biovol.um3mL",mean,na.rm=T)
x.val=apply(plsf.phyto.class.xtab[,3:ncol(plsf.phyto.class.xtab)],1,sum,na.rm=T)
plsf.phyto.class.xtab.prop=plsf.phyto.class.xtab
plsf.phyto.class.xtab.prop[,3:ncol(plsf.phyto.class.xtab.prop)]=sweep(plsf.phyto.class.xtab.prop[,3:ncol(plsf.phyto.class.xtab.prop)],1,x.val,"/")

apply(plsf.phyto.class.xtab.prop[,3:ncol(plsf.phyto.class.xtab.prop)],2,mean,na.rm=T)
phyto.class.prop=melt(plsf.phyto.class.xtab.prop,id.vars = c("CY","month"))
phyto.class.prop.mean=ddply(phyto.class.prop,c("variable"),summarise,mean.val=mean(value,na.rm=T))

phyto.class.prop.mean=phyto.class.prop.mean[order(-phyto.class.prop.mean$mean.val),]

class.reclass=data.frame(class=c("Cyanophyceae", "Cryptophyceae", "Bacillariophyceae", "Dinophyceae", 
                                 "Chlorophyceae", "Chrysophyceae", "Euglenoidea", "Prasinophyceae", 
                                 "Trebouxiophyceae", "Prymnesiophyceae", "Zygnematophyceae", "Eurotiomycetes", 
                                 "unid", "Katablepharidea", "Euglenophyceae", "Xanthophyceae", 
                                 "Eustigmatophyceae", "Liliopsida", "Klebsormidiophyceae"),
                         class.reclass=c("Cyanophyceae", "Cryptophyceae", "Bacillariophyceae", "Dinophyceae", 
                                         "Chlorophyceae", rep("Other",14)))

phyto.class.prop=merge(phyto.class.prop,class.reclass,by.x="variable",by.y="class")
phyto.class.prop.sum=ddply(phyto.class.prop,c("CY","month","class.reclass"),summarise,Tprop=sum(value,na.rm=T))

phyto.class.prop=subset(phyto.class.prop,CY>2009)
phyto.class.prop.sum=dcast(phyto.class.prop,CY+month~class.reclass,value.var = "value",sum,na.rm=T)
fill.val=expand.grid(month=1:12,
                     CY=2011:2020)
fill.val=rbind(fill.val,data.frame(month=1,CY=2021))

phyto.class.prop.sum=merge(phyto.class.prop.sum,fill.val,c("month","CY"),all.y=T)
phyto.class.prop.sum=phyto.class.prop.sum[,c("month","CY","Bacillariophyceae","Dinophyceae",
                                             "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other")]
phyto.class.prop.sum=phyto.class.prop.sum[order(phyto.class.prop.sum$CY,phyto.class.prop.sum$month),]

cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
barplot(t(phyto.class.prop.sum[,3:ncol(phyto.class.prop.sum)]),
        col=cols)


plsf.phyto.class$biovol.mm3L=plsf.phyto.class$sum.biovol.um3mL*1e-6

phyto.class.biovol=merge(plsf.phyto.class,class.reclass,by.x="class",by.y="class")
phyto.class.biovol.sum=ddply(phyto.class.biovol,c("CY","month","class.reclass"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))
phyto.class.biovol.sum$Tbiovol.log10=with(phyto.class.biovol.sum,ifelse(Tbiovol==0,0,log10(Tbiovol)))

phyto.class.biovol.sum=dcast(phyto.class.biovol.sum,CY+month~class.reclass,value.var = "Tbiovol",sum,na.rm=T)
phyto.class.biovol.sum=subset(phyto.class.biovol.sum,CY>2009)
phyto.class.biovol.sum=merge(phyto.class.biovol.sum,fill.val,c("month","CY"),all.y=T)

phyto.class.biovol.sum=phyto.class.biovol.sum[order(phyto.class.biovol.sum$CY,phyto.class.biovol.sum$month),]

phyto.class.biovol.sum=phyto.class.biovol.sum[,c("month","CY","Bacillariophyceae","Dinophyceae",
                                                 "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other")]
range(rowSums(phyto.class.biovol.sum[,3:ncol(phyto.class.biovol.sum)],na.rm=T))


fill.val$date=with(fill.val,date.fun(paste(CY,month,"01",sep="-")))
xlim.val=date.fun(c("2011-01-01","2021-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 year")
# png(filename=paste0(plot.path,"PLSF_Class_biovol_abund_comb.png"),width=10,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,0.5,0.5),oma=c(2,2,0.5,0.5),lwd=0.25);
layout(matrix(c(1:2,3,3),2,2,byrow = F),widths=c(1,0.25))

ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
x=barplot(t(phyto.class.biovol.sum[,3:ncol(phyto.class.biovol.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.biovol.sum)))
axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.25,"Total Biovolume (mm\u00B3 L\u207B\u00b9)")


ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
x=barplot(t(phyto.class.prop.sum[,3:ncol(phyto.class.prop.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.prop.sum)))
axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Relative Biovolume (Proportion)")
mtext(side=1,line=2.5,'Date (Month-Year)')

par(mar=c(1,1.5,0.5,3.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("Bacillariophyceae","Dinophyceae",
                          "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other"),
       pch=22,pt.bg=cols,pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


fill.val$date=with(fill.val,date.fun(paste(CY,month,"01",sep="-")))
xlim.val=date.fun(c("2011-01-01","2021-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 year")
# png(filename=paste0(plot.path,"PLSF_Class_Abund.png"),width=10,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1.5,0.5,0.5),oma=c(1,2,0.75,0.5),lwd=0.25);
# layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.25))

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
x=barplot(t(phyto.class.prop.sum[,3:ncol(phyto.class.prop.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.prop.sum)))

axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Relative Abundance")
mtext(side=1,line=2.5,'Date (Month-Year)')

# 
# par(mar=c(1,1.5,0.5,3.5))
# plot(0:1,0:1,ann=F,axes=F,type="n")
legend("topleft",legend=c("Bacillariophyceae","Dinophyceae",
                          "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other"),
       pch=22,pt.bg=cols,pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

xlim.val=date.fun(c("2011-01-01","2021-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 year")
# png(filename=paste0(plot.path,"PLSF_Class_biovol.png"),width=10,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1.5,0.5,0.5),oma=c(1,2,0.75,0.5),lwd=0.25);
# layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.25))

ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
x=barplot(t(phyto.class.biovol.sum[,3:ncol(phyto.class.biovol.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.biovol.sum)))

axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.25,"Total Biovolume (mm\u00B3 L\u207B\u00b9)")
mtext(side=1,line=2.5,'Date (Month-Year)')

# 
# par(mar=c(1,1.5,0.5,3.5))
# plot(0:1,0:1,ann=F,axes=F,type="n")
legend("topleft",legend=c("Bacillariophyceae","Dinophyceae",
                          "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other"),
       pch=22,pt.bg=cols,pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


## Rank clock

phyto_class.dat=na.omit(phyto.class.prop.sum)
phyto_class.dat=phyto_class.dat[rowSums(phyto_class.dat[,3:ncol(phyto_class.dat)])==1,]

## On a class level
phyto_class.dat$MonCY.date=with(phyto_class.dat,date.fun(paste(CY,month,"01",sep="-")))

vars=c("MonCY.date", "Bacillariophyceae", "Dinophyceae", "Cryptophyceae", "Chlorophyceae", "Cyanophyceae", "Other")
phyto_class.dat.melt=melt(phyto_class.dat[,vars],id.vars =vars[1] )
phyto_class.dat.melt$MonCY.date=lubridate::decimal_date(phyto_class.dat.melt$MonCY.date)

phyto_class.dat.melt$year=as.numeric(format(lubridate::date_decimal(phyto_class.dat.melt$MonCY.date),"%Y"))
# aggdat <- aggregate(value ~ variable * year,  data = subset(phyto_class.dat.melt, variable=="Cyanophyceae"), FUN = mean)
aggdat <- aggregate(value ~ variable * year,  data = phyto_class.dat.melt, FUN = mean,na.rm=T)
aggdat$variable=factor(aggdat$variable,levels=c("Bacillariophyceae","Dinophyceae",
                                                "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other"))
library(ggplot2)
cbbPalette <-c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7")

rank_clock=ggplot(aggdat, aes(year, value,color=variable)) + 
  # plot species lines
  geom_line(size = 1.5,alpha=0.75) + 
  # faceted by species
  # facet_wrap(~replicate2) +
  # on polor coordinates
  coord_polar()+
  # label axes
  labs(x="Year", y="Annual Mean Relative Biovolume", color="Class") + 
  # format
  theme_bw() +
  theme(text = element_text(size = 14,family = "serif"),
        strip.text.x = element_text(size = 14,family = "serif"), 
        strip.background = element_blank(),
        panel.grid.major = element_line(size = 1)) + 
  theme(legend.position="bottom", 
        legend.text=element_text(face = "italic")) +
  # color-blind friendly palette
  scale_color_manual( values = cbbPalette) + 
  # add a line to indicate the start
  geom_segment(aes(x = 2011, y = 0, xend = 2011, yend = 1), color = "grey70",lty=2)
rank_clock

# ggsave(paste0(plot.path,"Class_rank_clock.png"),rank_clock, width = 6, height = 5, units = "in",dpi = 300)

### Turnover ----------------------------------------------------------------
# phyto_class.dat=na.omit(phyto.class.prop.sum)
# phyto_class.dat=phyto_class.dat[rowSums(phyto_class.dat[,3:ncol(phyto_class.dat)])==1,]
# 
# ## On a class level
# phyto_class.dat$MonCY.date=with(phyto_class.dat,date.fun(paste(CY,month,"01",sep="-")))
# 
# vars=c("MonCY.date", "Bacillariophyceae", "Dinophyceae", "Cryptophyceae", "Chlorophyceae", "Cyanophyceae", "Other")
# phyto_class.dat.melt=melt(phyto_class.dat[,vars],id.vars =vars[1] )
# phyto_class.dat.melt$MonCY.date=lubridate::decimal_date(phyto_class.dat.melt$MonCY.date)

total.to=turnover(phyto_class.dat.melt,
                  time.var = "MonCY.date",
                  species.var="variable",
                  abundance.var="value",
                  metric="total")
# total.to$Measure="total"
# total.to=rename(total.to,c("total"="value"))
appear.to=turnover(phyto_class.dat.melt,
                   time.var = "MonCY.date",
                   species.var="variable",
                   abundance.var="value",
                   metric="appearance")
# appear.to$Measure="appearance"
# appear.to=rename(appear.to,c("appearance"="value"))
disappear.to=turnover(phyto_class.dat.melt,
                      time.var = "MonCY.date",
                      species.var="variable",
                      abundance.var="value",
                      metric="disappearance")
# disappear.to$Measure="disappearance"
# disappear.to=rename(disappear.to,c("disappearance"="value"))

turnover.class=merge(total.to,appear.to,c("MonCY.date"))
turnover.class=merge(turnover.class,disappear.to,c("MonCY.date"))

plot(total~MonCY.date,turnover.class,type="b")
plot(appearance~MonCY.date,appear.to,type="b",pch=21,bg="Red")
plot(disappearance~MonCY.date,disappear.to,type="b",pch=21,bg="lightgreen")

with(turnover.class,cor.test(total,MonCY.date,method="kendall"))
with(turnover.class,cor.test(appearance,MonCY.date,method="kendall"))
with(turnover.class,cor.test(disappearance,MonCY.date,method="kendall"))
turnover.class$MonCY.date=date.fun(lubridate::date_decimal(turnover.class$MonCY.date))

# turnover.class$SER=with(turnover.class,(appearance-disappearance)/total)
# plot(SER~MonCY.date,turnover.class,type="l")


fill.val$date=with(fill.val,date.fun(paste(CY,month,"01",sep="-")))
xlim.val=date.fun(c("2011-01-01","2021-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 year")
# png(filename=paste0(plot.path,"PLSF_Class_biovol_abund_comb2.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.75,2,0.5,0.5),oma=c(2,3,0.5,0.5),lwd=0.25);
layout(matrix(c(1,1,1,6,
                2,2,2,6,
                3,4,5,7),3,4,byrow = T),widths=c(1,1,1,0.75))

ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
x=barplot(t(phyto.class.biovol.sum[,3:ncol(phyto.class.biovol.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.biovol.sum)))
axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.25,"Total Biovolume\n(mm\u00B3 L\u207B\u00b9)",cex=0.8)

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("burlywood1","goldenrod","mediumaquamarine","seagreen","paleturquoise2","grey")
x=barplot(t(phyto.class.prop.sum[,3:ncol(phyto.class.prop.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.prop.sum)))
axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.25,"Relative Biovolume\n(Proportion)",cex=0.8)

cols.turn=c("#009E73", "#e79f00", "#9ad0f3")
ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(total~MonCY.date,turnover.class,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="gray",lwd=0.5)
with(turnover.class,pt_line(MonCY.date,total,2,cols.turn[1],1.5,21,cols.turn[1],pt.lwd=0.01,cex=1.5))
axis_fun(1,xmaj,xmin,format(xmaj,"J-%Y"),line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.25,"Class Turnover",cex=0.8)
mtext(side=3,line=-1.25,adj=0," Total")
turnover.class$MonCY.date2=lubridate::decimal_date(turnover.class$MonCY.date)
mod=mblm::mblm(total~MonCY.date2,turnover.class,repeated=F)
# mod=lm(total~MonCY.date2,turnover.class)
x.val=seq(min(turnover.class$MonCY.date2),max(turnover.class$MonCY.date2),length.out=10)
mod.pred=predict(mod,data.frame(MonCY.date2=x.val),interval = "confidence")
x.val2=date.fun(lubridate::date_decimal(x.val))
lines(mod.pred[,1]~x.val2,lwd=2)
lines(mod.pred[,2]~x.val2,lwd=1,lty=2);lines(mod.pred[,3]~x.val2,lwd=1,lty=2)

plot(total~MonCY.date,turnover.class,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="gray",lwd=0.5)
with(turnover.class,pt_line(MonCY.date,appearance,2,cols.turn[2],1.5,21,cols.turn[2],pt.lwd=0.01,cex=1.5))
axis_fun(1,xmaj,xmin,format(xmaj,"J-%Y"),line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," Appearance")
mtext(side=1,line=2.5,'Date (Month-Year)')
# mod=mblm::mblm(appearance~MonCY.date2,turnover.class,repeated=F)
mod=lm(appearance~MonCY.date2,turnover.class)
x.val=seq(min(turnover.class$MonCY.date2),max(turnover.class$MonCY.date2),length.out=10)
mod.pred=predict(mod,data.frame(MonCY.date2=x.val),interval="confidence")
x.val2=date.fun(lubridate::date_decimal(x.val))
lines(mod.pred[,1]~x.val2,lwd=2)
lines(mod.pred[,2]~x.val2,lwd=1,lty=2);lines(mod.pred[,3]~x.val2,lwd=1,lty=2)

plot(total~MonCY.date,turnover.class,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="gray",lwd=0.5)
with(turnover.class,pt_line(MonCY.date,disappearance,2,cols.turn[3],1.5,21,cols.turn[3],pt.lwd=0.01,cex=1.5))
axis_fun(1,xmaj,xmin,format(xmaj,"J-%Y"),line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," Disappearance")
# mod=mblm::mblm(disappearance~MonCY.date2,turnover.class,repeated=F)
mod=lm(disappearance~MonCY.date2,turnover.class)
x.val=seq(min(turnover.class$MonCY.date2),max(turnover.class$MonCY.date2),length.out=10)
mod.pred=predict(mod,data.frame(MonCY.date2=x.val),interval="confidence")
x.val2=date.fun(lubridate::date_decimal(x.val))
lines(mod.pred[,1]~x.val2,lwd=2)
lines(mod.pred[,2]~x.val2,lwd=1,lty=2);lines(mod.pred[,3]~x.val2,lwd=1,lty=2)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("Bacillariophyceae","Dinophyceae",
                         "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other"),
       pch=22,pt.bg=cols,pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("Total","Appearance","Disappearance","Trend \u00B1 95% CI"),
       pch=NA,pt.bg=cols,pt.cex = 1.5,
       lty=c(1),lwd=c(2),col=c(cols.turn,"black"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

###


rankshift.vals=rank_shift(phyto_class.dat.melt,
           time.var = "MonCY.date",
           species.var="variable",
           abundance.var="value")
str.val=strsplit(as.character(rankshift.vals$year_pair),"-")
rankshift.vals$t2=as.numeric(sapply(str.val,"[",2))
plot(MRS~t2,rankshift.vals,type="b")
with(rankshift.vals,cor.test(MRS,t2,method="kendall"))


rate_change(phyto_class.dat.melt,
            time.var = "MonCY.date",
            species.var="variable",
            abundance.var="value")

comm.res.plsf=rate_change_interval(phyto_class.dat.melt,
                     time.var = "MonCY.date",
                     species.var="variable",
                     abundance.var="value")
plot(distance~interval,comm.res.plsf)


# phyto_class.dat.melt$Yr=as.numeric(format(lubridate::date_decimal(phyto_class.dat.melt$MonCY.date),"%Y"))
variance_ratio(phyto_class.dat.melt,
               time.var = "MonCY.date",
               species.var="variable",
               abundance.var="value",
               bootnumber = 500,
               average.replicates = F)

tmp=cyclic_shift(phyto_class.dat.melt,
             time.var = "MonCY.date",
             species.var="variable",
             abundance.var="value",
             FUN=cov,
             bootnumber = 10)
confint(tmp)

### PCoA -------------------------------------------------------------------
## https://fromthebottomoftheheap.net/slides/intro-vegan-webinar-2020/intro-to-vegan.html#56

# phyto_class.dat=na.omit(phyto.class.prop.sum)
# phyto_class.dat=phyto_class.dat[rowSums(phyto_class.dat[,3:ncol(phyto_class.dat)])==1,]

head(phyto_class.dat)

## DETERMINE THE OPTIMAL DISSIMILARITY MEASURE FOR DISSIMILARITY-BASED ANALYSES
# We used Principal coordinates analysis (PCoA) for this, a.k.a. Classical Multidimensional Scaling. Interpretation: The Canberra distance shows the best spread of points.

# Euclidean distance: from vegdist function d[jk] = sqrt(sum(x[ij]-x[ik])^2)
pco.eu <- cmdscale(vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)], method="euclidean"), eig=T) 
# Bray-Curtis dissimilarity: from vegdist function d[jk] = (sum abs(x[ij]-x[ik]))/(sum (x[ij]+x[ik]))
pco.bc <- cmdscale(vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)], method="bray"), eig=T) 
# Alternative Gower distance: from vegdist function d[jk] = (1/NZ) sum(abs(x[ij] - x[ik])), NZ is the number of non-zero columns excluding double-zeros (Anderson et al. 2006)
pco.ag <- cmdscale(vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)], method="altGower"), eig=T) 
# Manhattan dissimilarity: from vegdist function d[jk] = sum(abs(x[ij] - x[ik]))
pco.mh <- cmdscale(vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)], method="manhattan"), eig=T) 
# Chi-square distance: from dsvdis function (exp - obs) / sqrt{exp}
pco.cs <- cmdscale(vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)], "chisq"), eig=T)
# Canberra distance: from vegdist function d[jk] = (1/NZ) sum ((x[ij]-x[ik])/(x[ij]+x[ik]))
pco.ca <- cmdscale(vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)], "canberra"), eig=T) 

layout(matrix(1:6,3,2))
plot(pco.eu$points, main="PCoA Euclidean (i.e. PCA)")
plot(pco.bc$points, main="PCoA Bray-Curtis") 
plot(pco.ag$points, main="PCoA alt Gower") 
plot(pco.mh$points, main="PCoA Manhattan") 
plot(pco.cs$points, main="PCoA Chi-squared") 
plot(pco.ca$points, main="PCoA Canberra")

plot(pco.bc$points~pco.eu$points);abline(0,1)
plot(pco.bc$points~pco.ag$points);abline(0,1)
plot(pco.bc$points~pco.mh$points);abline(0,1)
plot(pco.bc$points~pco.cs$points);abline(0,1)
plot(pco.bc$points~pco.ca$points);abline(0,1)


## PCoA analysis
phyto_class.dat.dist=vegdist(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)],method="bray")

pco1=wcmdscale(phyto_class.dat.dist,eig=T)
pco1
round(eigenvals(pco1),3)

pco2=wcmdscale(phyto_class.dat.dist,eig=T,add="lingoes")
pco2
round(eigenvals(pco2),3)

pco3=wcmdscale(phyto_class.dat.dist,eig=T,add="cailliez")
pco3
round(eigenvals(pco3),3)

plot(pco2)
plot(pco3)

#get PCoA scores
scrs=scores(pco1)
#weighted by abundance
spp_scrs=wascores(scrs,phyto_class.dat[,3:ncol(phyto_class.dat)],expand=F)

eig <- as.numeric(eigenvals(pco1))
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca[1:4,]


#get PCoA scores
scrs=scores(pco1)
#weighted by abundance
spp_scrs=wascores(scrs,phyto_class.dat[,3:(ncol(phyto_class.dat)-1)],expand=F)


par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));
# layout(matrix(1:2,2,1),heights=c(1,0.25))

xlim.val=c(-0.4,0.8);by.x=0.2;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-0.4,0.8);by.y=0.2;ymaj=round(c(0,seq(ylim.val[1],ylim.val[2],by.y)),1);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
points(scrs[,c(1,2)],pch=21,bg=adjustcolor("dodgerblue1",0.5),col="grey40",cex=1,lwd=0.5);
text(spp_scrs,row.names(spp_scrs),cex=0.8,font=2,col="black")
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); 
mtext(side=1,line=1.5,"Dim 1");
mtext(side=2,line=2.25,"Dim 2");
box(lwd=1)

dev.off()

# https://rpubs.com/Bury/ClusteringOnPcaResults

hop.stat=hopkins::hopkins(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)])
hop.stat
n.val=nrow(phyto_class.dat[,3:(ncol(phyto_class.dat)-1)])
## Hopkin index indicate the data is clustered
### Calculated values 0-0.3 indicate regularly-spaced data. 
### Values around 0.5 indicate random data. Values 0.7-1 indicate clustered data.
hopkins::hopkins.pval(hop.stat,n.val)

pcoa_clust=factoextra::fviz_nbclust(scrs[,c(1,2)],FUNcluster=kmeans, k.max = 2)
plot(y~clusters,pcoa_clust$data,type="b")

eclust.rslt=factoextra::eclust(scrs[,c(1,2)], "kmeans", hc_metric="eucliden",k=2)
eclust.rslt$cluster

phyto_class.dat$PCOA1.clusts=eclust.rslt$cluster
plot(PCOA1.clusts~MonCY.date,phyto_class.dat)

clust.cols=c("lightgreen",'steelblue')
# png(filename=paste0(plot.path,"PLSF_Phyto_Class_PCoA_clusters.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));

xlim.val=c(-0.4,0.8);by.x=0.2;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-0.4,0.6);by.y=0.2;ymaj=round(c(0,seq(ylim.val[1],ylim.val[2],by.y)),1);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
x=ordiellipse(pco1,eclust.rslt$cluster,col=clust.cols,draw="polygon",alpha=0.25*255,border="grey")
points(scrs[,c(1,2)],pch=21,bg="grey",col="grey40",cex=1,lwd=0.5);
text(spp_scrs,row.names(spp_scrs),cex=0.8,font=2,col="black")
# text(scrs[,1],scrs[,2],consec.day.vals)
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); box(lwd=1)
mtext(side=1,line=1.5,paste0("Axis 1 (",round(eig.pca$variance[1],1)," %)"));
mtext(side=2,line=2.25,paste0("Axis 2 (",round(eig.pca$variance[2],1)," %)"));

legend("bottomleft",legend=c("Group 1","Group 2"),# legend=c("June 2011 - June 2016", "July 2016 - Sept 2020"),
       pch=21,pt.bg=c(adjustcolor(rev(clust.cols),0.25)),
       lty=NA,lwd=c(1),col=c(rep("grey",2)),
       pt.cex=c(2),ncol=1,cex=0.8,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title.adj=0,title=" K-means clusters")
dev.off()


## PERMANOVA

data(dune)
data(dune.env)
adonis(dune ~ Management*A1, data=dune.env, permutations=99)

vars1=c("Bacillariophyceae", "Dinophyceae", "Cryptophyceae", 
        "Chlorophyceae", "Cyanophyceae", "Other")
spp.dat=phyto_class.dat[,vars1]
other.dat=phyto_class.dat[,c("month","CY","PCOA1.clusts")]

tmp1=adonis2(spp.dat ~ CY*PCOA1.clusts, data=other.dat, permutations=999)
tmp1

### Regression tree  --------------------------------------------------------
# https://r.qcbs.ca/workshop10/book-en/
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/univariate-regression-trees/
library(mvpart)
library(labdsv)
phyto.class.prop.sum$MonCY.date=with(phyto.class.prop.sum,date.fun(paste(CY,month,"01",sep="-")))
phyto.class.prop.sum$MonCY.date.num=as.numeric(lubridate::decimal_date(phyto.class.prop.sum$MonCY.date))

wq.vars=c("CY","month","TP.ugL","TN.mgL")# ,"Cond","Temp.C")
bio.vars=c("month", "CY", "MonCY.date.num", "Bacillariophyceae", "Dinophyceae", "Cryptophyceae", 
           "Chlorophyceae", "Cyanophyceae", "Other")
phyto.class.prop.sum2=merge(phyto.class.prop.sum[,bio.vars],wq.dat.month[,wq.vars],c("CY","month"),all.x=T)

plsf.spp=na.omit(phyto.class.prop.sum2)[,bio.vars[4:length(bio.vars)]]
plsf.env=na.omit(phyto.class.prop.sum2)[,c("MonCY.date.num",wq.vars[3:length(wq.vars)])]

plsf.spp.hel=decostand(plsf.spp, method = "hellinger")
# Create multivariate regression tree
set.seed(123)
plsf.mrt <- mvpart(as.matrix(plsf.spp) ~ ., data = plsf.env,
                   xv = "1se", # interactively select best tree
                   xval = nrow(plsf.spp), # number of cross-validations
                   xvmult = 5, # number of multiple cross-validations
                   which = 4, # plot both node labels
                   legend = FALSE, margin = 0.01, cp = 0,size=17)

cptab=data.frame(plsf.mrt$cptable)
cptab$treesize=1:nrow(cptab)#cptab$nsplit+1
#plotcp(plsf.mrt)

# png(filename=paste0(plot.path,"PLSF_Phyto_mrt_errors.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));

xlim.val=c(1,17);by.x=1;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x);
ylim.val=c(0,1.5);by.y=0.2;ymaj=round(c(0,seq(ylim.val[1],ylim.val[2],by.y)),1);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);

plot(xerror~treesize,cptab,xlim=xlim.val,ylim=ylim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(cptab,pt_line(treesize,rel.error,1,"dodgerblue1",1,21,"dodgerblue1",cex=1.25,pt.lwd=0.01))
with(cptab,arrows(treesize,xerror-xstd,
                  treesize,xerror+xstd,col="indianred1",length=0,angle=90,code=3))
with(cptab,pt_line(treesize,xerror,2,"indianred1",1,21,"indianred1",cex=1.25,pt.lwd=0.01))
abline(h=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.5,"Relative Error")
mtext(side=1,line=2,"Size of Tree")

legend("bottomleft",legend=c("Relative Error","Cross-Validated Relative Error"),
       pch=21,pt.bg=c("dodgerblue1","indianred1"),
       lty=NA,lwd=c(1),col=c(rep("grey",2)),
       pt.cex=c(2),ncol=1,cex=0.8,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()

##
plsf.mrt2=mvpart(as.matrix(plsf.spp) ~ ., data = plsf.env,
       xv = "1se", # interactively select best tree
       xval = nrow(plsf.spp), # number of cross-validations
       xvmult = 5, # number of multiple cross-validations
       which = 4, # plot both node labels
       legend = FALSE, margin = 0.01, cp = 0,size=5)
plotcp(plsf.mrt2)
summary(plsf.mrt2)

library(ggdendro)
ddata <- dendro_data(plsf.mrt2)

ddata$labels$label=gsub("TN.mgL>=1.666","TN \u2265 1.67 mg L\u207B\u00B9",ddata$labels$label)
ddata$labels$label=gsub("TN.mgL>=1.263","TN \u2265 1.26 mg L\u207B\u00B9",ddata$labels$label)
ddata$labels$label=gsub("TP.ugL< 81.5","TP < 81 \u03BCg L\u207B\u00B9",ddata$labels$label)
ddata$labels$label=gsub("MonCY.date.num< 2017","Date < 2017",ddata$labels$label)
ddata$leaf_labels$label=round(as.numeric(ddata$leaf_labels$label),2)

dendro=ggplot() +
  geom_segment(data = ddata$segments,
               aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = ddata$labels,
            aes(x = x, y = y, label = label,family = "serif",fontface='bold'), size = 4, vjust = -0.5) +
  geom_text(data = ddata$leaf_labels,
            aes(x = x, y = y, label = label,family = "serif"), size = 4, vjust = 1) +
  theme_dendro();dendro
# ggsave(paste0(plot.path,"plsf_phyto_mrt.png"),dendro, width = 6, height = 5, units = "in",dpi = 300)

# Calculate indicator values (indval) for each species
doubs.mrt.indval <- indval(plsf.spp, plsf.mrt2$where)

# Extract the significant indicator species (and which node
# they represent)
doubs.mrt.indval$maxcls[which(doubs.mrt.indval$pval <= 0.05)]

# Extract their indicator values
doubs.mrt.indval$indcls[which(doubs.mrt.indval$pval <= 0.05)]

doubs.mrt.indval$pval[which(doubs.mrt.indval$pval <= 0.05)]



## Cyano  ------------------------------------------------------------------

head(plsf.phyto2)
unique(subset(plsf.phyto2,class=="Cyanophyceae")$genus)


phyto.class.prop.mean=ddply(phyto.class.prop,c("variable"),summarise,mean.val=mean(value,na.rm=T))
phyto.class.prop.mean=phyto.class.prop.mean[order(-phyto.class.prop.mean$mean.val),]

plsf.phyto2$cyano_genus=with(plsf.phyto2,ifelse(class!="Cyanophyceae"|is.na(genus)==T,"Other-phyto",genus))
plsf.phyto.cyano=ddply(plsf.phyto2,c("CY","month","cyano_genus"),summarise,
                       sum.biovol.um3mL=sum(mean.biovol.um3mL,na.rm=T),
                       sum.conc.cellsmL=sum(mean.conc.cellsmL,na.rm=T))
plsf.phyto.cyano.xtab1=dcast(plsf.phyto.cyano,CY+month~cyano_genus,value.var="sum.biovol.um3mL",mean,na.rm=T)
x.val=apply(plsf.phyto.cyano.xtab1[,3:ncol(plsf.phyto.cyano.xtab1)],1,sum,na.rm=T)
plsf.phyto.cyano.xtab.prop=plsf.phyto.cyano.xtab1
phyto.cyano.class.prop=melt()

phyto.cyano.class.prop.mean=ddply(melt(plsf.phyto.cyano.xtab.prop,id.vars = c("CY","month")),
                            c("variable"),summarise,sum.val=sum(value,na.rm=T))
phyto.cyano.class.prop.mean=phyto.cyano.class.prop.mean[order(-phyto.cyano.class.prop.mean$sum.val),]

top.cyanos=c("Dolichospermum", "Microcystis", "Aphanizomenon", "Planktothrix", "Woronichinia", "Aphanocapsa")
other.cyanos=as.character(subset(phyto.cyano.class.prop.mean,!(variable%in%c(top.cyanos,"Other-phyto")))$variable)

# other.cyanos=c("Anabaena", "Coelomoron", 
#                "Cyanocatena", "Cyanodictyon", "Cyanogranis", "Dactylococcopsis", 
#                "Phormidium", "Planktolyngbya", "Pseudanbaena", "Sphaerospermopsis", 
#                "Spirulina")

plsf.phyto2$cyano_genus_reclass=with(plsf.phyto2,ifelse(class=="Cyanophyceae"&genus%in%other.cyanos,"Other-Cyano",
                                                        ifelse(class=="Cyanophyceae"&genus%in%top.cyanos,top.cyanos,
                                                               ifelse(class=="Cyanophyceae"&is.na(genus)==T,"Other-Cyano",
                                                                      ifelse(class!="Cyanophycease","Other-Phyto",NA)))))
unique(plsf.phyto2$cyano_genus_reclass)
sum(is.na(plsf.phyto2$cyano_genus_reclass))
plsf.phyto.cyano=ddply(plsf.phyto2,c("CY","month","cyano_genus_reclass"),summarise,
                       sum.biovol.um3mL=sum(mean.biovol.um3mL,na.rm=T),
                       sum.conc.cellsmL=sum(mean.conc.cellsmL,na.rm=T))
plsf.phyto.cyano$log10.biovol=with(plsf.phyto.cyano,ifelse(sum.biovol.um3mL==0,0,log10(sum.biovol.um3mL)))

plsf.phyto.cyano.xtab=dcast(plsf.phyto.cyano,CY+month~cyano_genus_reclass,value.var="sum.biovol.um3mL",mean,na.rm=T)
x.val=apply(plsf.phyto.cyano.xtab[,3:ncol(plsf.phyto.cyano.xtab)],1,sum,na.rm=T)
plsf.phyto.cyano.xtab.prop=plsf.phyto.cyano.xtab
plsf.phyto.cyano.xtab.prop[,3:ncol(plsf.phyto.cyano.xtab.prop)]=sweep(plsf.phyto.cyano.xtab.prop[,3:ncol(plsf.phyto.cyano.xtab.prop)],1,x.val,"/")
rowSums(plsf.phyto.cyano.xtab.prop[,3:ncol(plsf.phyto.cyano.xtab.prop)],na.rm=T)


apply(plsf.phyto.cyano.xtab.prop[,3:ncol(plsf.phyto.cyano.xtab.prop)],2,mean,na.rm=T)
phyto.cyano.prop=melt(plsf.phyto.cyano.xtab.prop,id.vars = c("CY","month"))
phyto.cyano.prop.mean=ddply(phyto.cyano.prop,c("variable"),summarise,mean.val=sum(value,na.rm=T))

phyto.cyano.prop.mean=phyto.cyano.prop.mean[order(-phyto.cyano.prop.mean$mean.val),];phyto.cyano.prop.mean

phyto.cyano.prop.sum=ddply(phyto.cyano.prop,c("CY","month","variable"),summarise,Tprop=sum(value,na.rm=T))

phyto.cyano.prop=subset(phyto.cyano.prop,CY>2009)
phyto.cyano.prop.sum=dcast(phyto.cyano.prop,CY+month~variable,value.var = "value",sum,na.rm=T)
fill.val=expand.grid(month=1:12,
                     CY=2011:2020)
fill.val=rbind(fill.val,data.frame(month=1,CY=2021))

phyto.cyano.prop.sum=merge(phyto.cyano.prop.sum,fill.val,c("month","CY"),all.y=T)

# vars=c("month","CY",as.character(phyto.cyano.prop.mean$variable))
vars=c("month","CY",top.cyanos,"Other-Cyano","Other-Phyto")

phyto.cyano.prop.sum=phyto.cyano.prop.sum[order(phyto.cyano.prop.sum$CY,phyto.cyano.prop.sum$month),vars]
head(phyto.cyano.prop.sum)
tmp=rowSums(phyto.cyano.prop.sum[,3:ncol(phyto.cyano.prop.sum)])
phyto.cyano.prop.sum[tmp<0.99,]




fill.val$date=with(fill.val,date.fun(paste(CY,month,"01",sep="-")))
xlim.val=date.fun(c("2011-01-01","2021-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 year")
# png(filename=paste0(plot.path,"PLSF_Phyto_cyanos.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.75,2,0.5,0.5),oma=c(2,3,0.5,0.5),lwd=0.25);
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.5))

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

cols=c(hcl.colors(6,"BluGrn"),"darkturquoise","grey80")
x=barplot(t(phyto.cyano.prop.sum[,3:ncol(phyto.cyano.prop.sum)]),
          beside=F,space=0,col=cols,border="grey50",axes=F,yaxs="i",width=0.75,
          ylim=ylim.val,names=rep(NA,nrow(phyto.class.prop.sum)))
axis_fun(1,x[which(fill.val$date%in%xmaj)],x[which(fill.val$date%in%xmin)],format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.25,"Relative Biovolume\n(Proportion)",cex=0.8)
mtext(side=1,line=1.5,"Date")

par(mar=c(1.75,1,0.5,0.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c(top.cyanos,"Other-Cyano","Other-Phyto"),
       pch=22,pt.bg=cols,pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=rev(cols),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

### PCoA --------------------------------------------------------------------
phyto.cyano.prop.sum=na.omit(phyto.cyano.prop.sum)
phyto_cyano.dat.dist <- vegdist(phyto.cyano.prop.sum[,3:(ncol(phyto.cyano.prop.sum))], method="bray")

pco1=wcmdscale(phyto_cyano.dat.dist,eig=T)
pco1
round(eigenvals(pco1),3)

plot(pco1)

#get PCoA scores
scrs=scores(pco1)
#weighted by abundance
spp_scrs=wascores(scrs,phyto.cyano.prop.sum[,3:ncol(phyto.cyano.prop.sum)],expand=F)

eig <- as.numeric(eigenvals(pco1))
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca[1:4,]


par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));
# layout(matrix(1:2,2,1),heights=c(1,0.25))

xlim.val=c(-0.6,0.6);by.x=0.2;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-0.6,0.6);by.y=0.2;ymaj=round(c(0,seq(ylim.val[1],ylim.val[2],by.y)),1);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
points(scrs[,c(1,2)],pch=21,bg=adjustcolor("dodgerblue1",0.5),col="grey40",cex=1,lwd=0.5);
text(spp_scrs,row.names(spp_scrs),cex=0.8,font=2,col="black")
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); 
mtext(side=1,line=1.5,"Dim 1");
mtext(side=2,line=2.25,"Dim 2");
box(lwd=1)
dev.off()



hop.stat=hopkins::hopkins(phyto.cyano.prop.sum[,3:(ncol(phyto.cyano.prop.sum)-1)])
hop.stat
n.val=nrow(phyto.cyano.prop.sum[,3:(ncol(phyto.cyano.prop.sum)-1)])
## Hopkin index indicate the data is clustered
### Calculated values 0-0.3 indicate regularly-spaced data. 
### Values around 0.5 indicate random data. Values 0.7-1 indicate clustered data.
hopkins::hopkins.pval(hop.stat,n.val)

pcoa_clust=factoextra::fviz_nbclust(scrs[,c(1,2)],FUNcluster=kmeans, k.max = 3)
plot(y~clusters,pcoa_clust$data,type="b")

# eclust.rslt=factoextra::eclust(scrs[,c(1,2)], "hclust",k=3,plot=F)
# factoextra::fviz_dend(eclust.rslt)

eclust.rslt=factoextra::eclust(scrs[,c(1,2)], "kmeans", hc_metric="eucliden",k=3)
eclust.rslt$cluster

phyto.cyano.prop.sum$PCOA1.clusts=eclust.rslt$cluster
phyto.cyano.prop.sum$MonCY.date=with(phyto.cyano.prop.sum,date.fun(paste(CY,month,"01",sep="-")))
plot(PCOA1.clusts~MonCY.date,phyto.cyano.prop.sum)

clust.cols=c("orange","lightgreen",'steelblue')
# png(filename=paste0(plot.path,"PLSF_Phyto_Cyno_PCoA_clusters.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));

xlim.val=c(-0.6,0.8);by.x=0.2;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-0.6,0.6);by.y=0.2;ymaj=round(c(0,seq(ylim.val[1],ylim.val[2],by.y)),1);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
x=ordiellipse(pco1,eclust.rslt$cluster,col=clust.cols,draw="polygon",alpha=0.25*255,border="grey")
points(scrs[,c(1,2)],pch=21,bg="grey",col="grey40",cex=1,lwd=0.5);
text(spp_scrs,row.names(spp_scrs),cex=0.8,font=2,col="black")
# text(scrs[,1],scrs[,2],consec.day.vals)
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); box(lwd=1)
mtext(side=1,line=1.5,"Dim 1");
mtext(side=2,line=2.25,"Dim 2");

legend("topright",legend=c("Group 1","Group 2","Group 3"),# legend=c("June 2011 - June 2016", "July 2016 - Sept 2020"),
       pch=21,pt.bg=c(adjustcolor(clust.cols,0.25)),
       lty=NA,lwd=c(1),col=c(rep("grey",2)),
       pt.cex=c(2),ncol=1,cex=0.8,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title.adj=0,title=" K-means clusters")
dev.off()


grp1=spp_scrs[,2]>0
grp1=names(grp1[grp1==T])
paste(grp1,collapse=", ")

grp2=spp_scrs[,1]<0&spp_scrs[,2]<0
grp2=names(grp2[grp2==T])

grp3=spp_scrs[,1]>0&spp_scrs[,2]<0
grp3=names(grp3[grp3==T])
paste(grp3,collapse=", ")



### Cyano RDA ---------------------------------------------------------------
phyto.cyano.prop.sum2=merge(phyto.cyano.prop.sum,wq.dat.month,c("CY","month"),all.x=T)

nrow(phyto.cyano.prop.sum2)
nrow(na.omit(phyto.cyano.prop.sum2))

env_st<-na.omit(phyto.cyano.prop.sum2)[,14:ncol(phyto.cyano.prop.sum2)]
abotu<-na.omit(phyto.cyano.prop.sum2)[,3:10]

## stepwise RDA
spe.rda <- rda(abotu~., data=env_st)
tmp=ordiR2step(rda(abotu~1, data=env_st), scope= formula(spe.rda), direction= "both", R2scope=TRUE, pstep=100)
tmp
tmp$terms
vif.cca(spe.rda)


env.vars=c("SRP.ugL","TN.mgL","TN_TP")
spp.vars=c(top.cyanos,"Other-Cyano","Other-Phyto")
nrow(na.omit(phyto.cyano.prop.sum2[,c(spp.vars,env.vars)]))

env_st2<-na.omit(phyto.cyano.prop.sum2[,c(spp.vars,env.vars)])[,env.vars]
abotu<-na.omit(phyto.cyano.prop.sum2[,c(spp.vars,env.vars)])[,spp.vars]


spe.rda2 <- rda(abotu~., data=env_st2)
tmp=summary(spe.rda2, display=NULL);tmp
# tmp$cont$importance
vif.cca(spe.rda2)


anova.cca(spe.rda2, step=1000)
anova.cca(spe.rda2, by='axis', step=1000)
rslt.terms=anova.cca(spe.rda2, by='terms', step=1000);rslt.terms
(R2adj <- RsquareAdj(spe.rda2)$adj.r.squared)


eig <- spe.rda2$CA$eig
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca[1:3,]

plot(spe.rda2)

hop.stat=hopkins::hopkins(abotu)
hop.stat
n.val=nrow(abotu)
## Hopkin index indicate the data is clustered
### Calculated values 0-0.3 indicate regularly-spaced data. 
### Values around 0.5 indicate random data. Values 0.7-1 indicate clustered data.
hopkins::hopkins.pval(hop.stat,n.val)

scrs=scores(spe.rda2)
scrs2=scores(spe.rda2,scaling = "symmetric")
pcoa_clust=factoextra::fviz_nbclust(scrs$sites[,c(1,2)],FUNcluster=kmeans, k.max = 3)
plot(y~clusters,pcoa_clust$data,type="b")

eclust.rslt=factoextra::eclust(scrs$sites[,c(1,2)], "kmeans", hc_metric="eucliden",k=3)
eclust.rslt$cluster

scrs.arrows<-scores(spe.rda2,choices=c(1,2,3),scaling="symmetric");
labs.arrows=rownames(scrs.arrows)
labs.arrows=c("SRP","TN","TN:TP")


spp.labs=rownames(scrs2$species)
clust.cols=c("lightgreen",'steelblue',"pink")
# png(filename=paste0(plot.path,"PLSF_cyano_rda.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.25,0.5));

xlim.val=c(-1.5,2);by.x=0.5;xmaj=round(c(0,seq(xlim.val[1],xlim.val[2],by.x)),1);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-1.5,1.5);by.y=0.5;ymaj=round(c(0,seq(ylim.val[1],ylim.val[2],by.y)),1);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);plot(xlim.val,ylim.val,type="n",axes=F,ann=F);
abline(h=0,v=0,lty=3,col="grey");
x=ordiellipse(spe.rda2,eclust.rslt$cluster,col=clust.cols,draw="polygon",alpha=0.25*255,border="grey")
points(scrs$sites[,c(1,2)],pch=21,bg="grey",col="grey40",cex=1,lwd=0.5);
text(scrs2$species,spp.labs,cex=0.8,font=3,col="black")
arrows(0,0,scrs$biplot[,1],scrs$biplot[,2],length = 0.05, angle = 15, code = 2,col="indianred2",lwd=1.5);
text(scrs$biplot,labels=labs.arrows,cex=0.75,font=1,col="red",pos=ifelse(scrs$biplot[,1]<0,2,4))

# text(scrs[,1],scrs[,2],consec.day.vals)
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1);
axis_fun(2,ymaj,ymin,format(ymaj),1); box(lwd=1)
mtext(side=1,line=1.5,paste0(" RDA Axis 1 (",round(eig.pca$variance[1],1)," %)"));
mtext(side=2,line=2.25,paste0("RDA Axis 2 (",round(eig.pca$variance[2],1)," %)"));

legend("bottomleft",legend=c("Group 1","Group 2","Group 3"),
       pch=21,pt.bg=c(adjustcolor(rev(clust.cols),0.25)),
       lty=NA,lwd=c(1),col=c(rep("grey",2)),
       pt.cex=c(2),ncol=1,cex=0.8,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title.adj=0,title=" K-means clusters")
dev.off()




# END ---------------------------------------------------------------------
