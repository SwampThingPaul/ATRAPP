## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Sonde blue-green algae predictive
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 01/23/2023

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
paths=paste0(wd,c("/Plots/PLSF_BGPredict/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


# Sonde Data --------------------------------------------------------------
sonde.dat=read.xlsx(paste0(data.path,"PLSF_Sonde/PLSF Outlet Phycocyanin-Chlorophyll.xlsx"),
                    startRow = 3,cols=1:14,rows=1:55)
sonde.dat$Date=date.fun(convertToDate(sonde.dat$Date))
colnames(sonde.dat)=c("Date","Week",paste0("Phy.",c(2008,2016:2020)),paste0("Chl.",c(2008,2016:2020)))
sonde.dat$month=as.numeric(format(sonde.dat$Date,"%m"))
sonde.dat$day=as.numeric(format(sonde.dat$Date,"%d"))

sonde.dat.melt=melt(sonde.dat,id.vars = c("Date","Week","month","day"))
sonde.dat.melt$variable=as.character(sonde.dat.melt$variable)
sonde.dat.melt$year=sapply(strsplit(sonde.dat.melt$variable,"\\."),"[",2)
sonde.dat.melt$variable=sapply(strsplit(sonde.dat.melt$variable,"\\."),"[",1)
sonde.dat.melt$date2=with(sonde.dat.melt,date.fun(paste(year,month,day,sep="-")))
sonde.dat.melt$value=as.numeric(sonde.dat.melt$value)
range(sonde.dat.melt$value,na.rm=T)

sonde.dat=dcast(subset(sonde.dat.melt,year!=2008&is.na(value)==F),date2~variable,value.var = "value",mean)
sonde.dat$Chl_Phy_ratio=with(sonde.dat,Chl/Phy)
sonde.dat$Phy_Chl_ratio=with(sonde.dat,Phy/Chl)

plot(Phy_Chl_ratio~date2,sonde.dat)
plot(Chl_Phy_ratio~date2,sonde.dat)

##quantmod analysis (ie peak analysis)


# Microscope data ---------------------------------------------------------
## From "Phyto-Cyano-Zoop-Proto" PLSF database
bio.dat.head=read.xlsx(paste0(data.path,"PLSF Database Phyto-Cyano-Zoop-Proto.xlsx"))[1:2,]
bio.dat.head=t(bio.dat.head)
bio.dat.head=cbind(data.frame(spp=rownames(bio.dat.head)),bio.dat.head)
colnames(bio.dat.head)=c("spp","col1","col2")
unique(bio.dat.head$col1)
bio.dat.head$col1=with(bio.dat.head,ifelse(col1=="Concentration","Conc",
                                           ifelse(col1=="Total biovolume","biovol",
                                                  ifelse(col1=="Total biomass","biomass",NA))))
unique(bio.dat.head$col2)
unit.xwalk=data.frame(col2=c(NA, "cells/ml", "µm3/mL", "#/L", "µg/L"),
                      col2a=c(NA,"cells.mL","um3.mL","num.L","ug.L"))
bio.dat.head$col2=with(bio.dat.head,ifelse(col2=="cells/ml","cells.mL",
                                           ifelse(col2=="µm3/mL","um3.mL",
                                                  ifelse(col2=="#/L","num.L",
                                                         ifelse(col2== "µg/L","ug.L",NA)))))
# bio.dat.head=merge(bio.dat.head,unit.xwalk,"col2",all.x=T,sort=F)
bio.dat.head$head.val=with(bio.dat.head,ifelse(spp=="Date","Date",paste(spp,col1,col2,sep="_")))

spp.check=strsplit(bio.dat.head$spp,"\\.")
# write.csv(data.frame(val=unique(sapply(spp.check,"[",1))),paste0(export.path,"PLSF_botia_genera.csv"),row.names = F)

bio.dat=read.xlsx(paste0(data.path,"PLSF Database Phyto-Cyano-Zoop-Proto.xlsx"),startRow = 4,colNames=F)
colnames(bio.dat)=bio.dat.head$head.val
bio.dat$Date=date.fun(convertToDate(bio.dat$Date))
bio.dat$Site="Lake_Outlet"

vars=c('Date',
       "Phytoplankton.total_biovol_um3.mL",
       "Phytoplankton.total_Conc_cells.mL",
       "Cyano.N-Fixing.Species_biovol_um3.mL",
       "Cyano.N-Fixing.Species_Conc_cells.mL",
       "Cyano.Non-N-Fixing.Species_biovol_um3.mL",
       "Cyano.Non-N-Fixing.Species_Conc_cells.mL"
       )
bio.dat2=bio.dat[,vars]
cyano.biovol.vars=c("Cyano.N-Fixing.Species_biovol_um3.mL","Cyano.Non-N-Fixing.Species_biovol_um3.mL")
bio.dat2$Cyano.biovol=rowSums(bio.dat2[,cyano.biovol.vars],na.rm=T)

cyano.cells.vars=c("Cyano.N-Fixing.Species_Conc_cells.mL","Cyano.Non-N-Fixing.Species_Conc_cells.mL")
bio.dat2$Cyano.cells.mL=rowSums(bio.dat2[,cyano.cells.vars],na.rm=T)


sonde.dat2=merge(sonde.dat,bio.dat2,by.x="date2",by.y="Date",all.x=T)
sonde.dat2$log10.Cyano.cells.mL=with(sonde.dat2,ifelse(Cyano.cells.mL==0,0,log10(Cyano.cells.mL)))
sonde.dat2$log10.phyto.cells.mL=with(sonde.dat2,ifelse(Phytoplankton.total_Conc_cells.mL==0,0,log10(Phytoplankton.total_Conc_cells.mL)))

plot(Chl~Phy,sonde.dat2,log="yx")

plot(log10.Cyano.cells.mL~Phy,sonde.dat2,log="x")
plot(log10.phyto.cells.mL~Phy,sonde.dat2,log="x")

plot(log10.phyto.cells.mL~Chl_Phy_ratio,sonde.dat2,log="x")

plot(Cyano.cells.mL~Phy,sonde.dat2,log="xy")
plot(Cyano.biovol~Phy,sonde.dat2,log="y")
plot(Phy~Phytoplankton.total_biovol_um3.mL,sonde.dat2,log="xy");
abline(v=18*1e6)

sonde.dat2$log.Phy=log(sonde.dat2$Phy)
sonde.dat2$log.Tphyto=log(sonde.dat2$Phytoplankton.total_biovol_um3.mL)

Phy.Phytobiovol.mod=lm(log.Phy~log.Tphyto,sonde.dat2)
summary(Phy.Phytobiovol.mod)
gvlma::gvlma(Phy.Phytobiovol.mod)
x.val=with(sonde.dat2,seq(min(log.Tphyto,na.rm=T),max(log.Tphyto,na.rm=T),length.out=50))
pred.mod=predict(Phy.Phytobiovol.mod,data.frame(log.Tphyto=x.val))

plot(Phy~Phytoplankton.total_biovol_um3.mL,sonde.dat2);
lines(exp(x.val),exp(pred.mod))

library(segmented)
seg.mod=segmented(Phy.Phytobiovol.mod,~log.Tphyto)
abline(v=exp(seg.mod$psi[2]))

exp(predict(Phy.Phytobiovol.mod,data.frame(log.Tphyto=seg.mod$psi[2])))

## How to identify bloom conditions? - is all data from bloom conditions?

plot(Phy_Chl_ratio~Phytoplankton.total_biovol_um3.mL,sonde.dat2);
plot(Chl_Phy_ratio~Phytoplankton.total_biovol_um3.mL,sonde.dat2);
