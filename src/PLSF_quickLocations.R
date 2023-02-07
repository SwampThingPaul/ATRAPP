## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Version 3 after explore and streamlining process
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 05/11/2022

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(openxlsx)
library(plyr)
library(reshape2)
library(dssrip)
library(zoo)
library(classInt)
#
library(magrittr)
library(flextable)
library(ggplot2)

# GIS libraries 
# library(sp)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_1/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


wgs84=CRS("+proj=longlat +datum=WGS84")


### 

PLSF_sites=data.frame(SITE=c("Inlet","Outlet","In_Lake"),
                      lat=c(45.5357,45.53154,45.5373),
                      long=c(-72.0415,-72.0297,-72.0432))
PLSF_sites=SpatialPointsDataFrame(PLSF_sites[,c("long","lat")],
                                  PLSF_sites,
                            proj4string = wgs84 )
bbox(PLSF_sites)

###
ogrListLayers(paste0(GIS.path,"/CA_NHN/StLawrence/NHN_02OF000_4_1.gdb"))
ogrListLayers(paste0(GIS.path,"/QC_multiscale_Watersheds/CE_bassin_multi.gdb"))

lakes=spTransform(readOGR(paste0(GIS.path,"/CA_NHN/StLawrence/NHN_02OF000_4_1.gdb"),"NHN_HD_WATERBODY_2"),wgs84)
flowLines=spTransform(readOGR(paste0(GIS.path,"/CA_NHN/StLawrence/NHN_02OF000_4_1.gdb"),"NHN_HD_SLWATER_1"),wgs84)
watershed=spTransform(readOGR(paste0(GIS.path,"/QC_multiscale_Watersheds/CE_bassin_multi.gdb"),"bassin_multi"),wgs84)

# PLSF_watershed=subset(watershed,Nom.bassin=="Tomecod, Ruisseau")
# subset(watershed,NO_COURS_DEAU=="03020471"); # FRAPPIER
# subset(watershed,NO_COURS_DEAU=="03020489"); # CHABOT
# subset(watershed,NO_COURS_DEAU=="03020194"); # TOMECOD
PLSF_watershed=subset(watershed,NO_COURS_DEAU=="03020194")

PLSF=subset(lakes,lakeName1==unique(lakes$lakeName1)[43])
bbox(PLSF)


plot(PLSF)

par(mar=c(0,0,0,0),oma=c(0.1,0.1,0.1,0.1))
plot(PLSF_watershed)
plot(crop(flowLines,PLSF_watershed),add=T,col="lightblue",lwd=2)
plot(PLSF,add=T,col="lightblue")
