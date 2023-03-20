## Title:      Petit-lac-Saint-François - biotia changes
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 03/20/2023

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

spp.class.order=read.csv(paste0(export.path,"20230317_GenusSpp_ClassOrder.csv"))
head(spp.class.order)

unique(spp.class.order$class)
spp.class.order$class[spp.class.order$class=="Cyanobacteriia"]="Cyanophyceae"

plsf.phyto$GenusSpp=trimws(plsf.phyto$GenusSpp)
plsf.phyto=merge(plsf.phyto,spp.class.order,"GenusSpp",all.x=T)

# is.na(plsf.phyto$class)
# unique(subset(plsf.phyto,is.na(class)==T)$GenusSpp)

plsf.phyto2=ddply(plsf.phyto,c("CY","month","GenusSpp","class"),summarise,mean.biovol.um3mL=mean(totbiovol.um3mL,na.rm=T))
plsf.phyto.class=ddply(plsf.phyto2,c("CY","month","class"),summarise,sum.biovol.um3mL=sum(mean.biovol.um3mL,na.rm=T))

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

phyto.class.prop.sum=dcast(phyto.class.prop,CY+month~class.reclass,value.var = "value",sum,na.rm=T)
fill.val=expand.grid(month=1:12,
                     CY=2009:2020)
phyto.class.prop.sum=merge(phyto.class.prop.sum,fill.val,c("month","CY"),all.y=T)
phyto.class.prop.sum=phyto.class.prop.sum[,c("month","CY","Bacillariophyceae","Dinophyceae",
                                             "Cryptophyceae","Chlorophyceae","Cyanophyceae","Other")]

cols=c("khaki","blue","green","forestgreen","darkolivegreen1","grey")
barplot(t(phyto.class.prop.sum[,3:ncol(phyto.class.prop.sum)]),
        col=cols)


plot(phyto.class.prop.sum$Bacillariophyceae)
plot(phyto.class.prop.sum$Dinophyceae)
plot(phyto.class.prop.sum$Cyanophyceae)
plot(phyto.class.prop.sum$Chlorophyceae)


# PCoA --------------------------------------------------------------------
## CcoA to find shifts in class? 
