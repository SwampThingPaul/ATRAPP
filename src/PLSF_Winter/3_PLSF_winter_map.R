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

library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_winter/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


wgs84=CRS("+proj=longlat +datum=WGS84")
utm18=CRS("+init=epsg:26918")

# -------------------------------------------------------------------------
# as.data.frame(wx.sites[,c("station_name","lat","lon")])
wx.sites2=data.frame(station_name = c("Bonsecours", "Bromptonville"), 
                     lat = c(45.4, 45.48), 
                     lon = c(-72.27, -71.95)
                     )
wx.sites2.shp=SpatialPointsDataFrame(wx.sites2[,c("lon","lat")],
                                     wx.sites2,
                         proj4string = wgs84 )

wx.sites2.shp=spTransform(wx.sites2.shp,utm18)

world <- ne_countries(scale = 50, returnclass = "sp")

ca.shp=subset(countries110,sovereignt=='Canada')
ca.shp=spTransform(ca.shp,utm18)
ca.states=spTransform(ne_states(country="Canada",returnclass = "sp"),utm18)

plot(ca.shp,lwd=0.5)
plot(ca.states,lwd=0.1,border="grey50")
plot(subset(ca.states,name_de=="Québec"),col="grey",add=T,lwd=0.5)


### 

PLSF_sites=data.frame(SITE=c("Inlet","Outlet","In_Lake"),
                      lat=c(45.5357,45.53154,45.5373),
                      long=c(-72.0415,-72.0297,-72.0432))
PLSF_sites=SpatialPointsDataFrame(PLSF_sites[,c("long","lat")],
                                  PLSF_sites,
                            proj4string = wgs84 )
bbox(PLSF_sites)

PLSF_sites=spTransform(PLSF_sites,utm18)

###
ogrListLayers(paste0(GIS.path,"/CA_NHN/StLawrence/NHN_02OF000_4_1.gdb"))
ogrListLayers(paste0(GIS.path,"/QC_multiscale_Watersheds/CE_bassin_multi.gdb"))

lakes=spTransform(readOGR(paste0(GIS.path,"/CA_NHN/StLawrence/NHN_02OF000_4_1.gdb"),"NHN_HD_WATERBODY_2"),wgs84)
flowLines=spTransform(readOGR(paste0(GIS.path,"/CA_NHN/StLawrence/NHN_02OF000_4_1.gdb"),"NHN_HD_SLWATER_1"),wgs84)
watershed=spTransform(readOGR(paste0(GIS.path,"/QC_multiscale_Watersheds/CE_bassin_multi.gdb"),"bassin_multi"),wgs84)

head(watershed@data)

watershed_lvl1=subset(watershed,NIVEAU_BASSIN==1)
watershed_lvl1=spTransform(watershed_lvl1,utm18)
# PLSF_watershed=subset(watershed,Nom.bassin=="Tomecod, Ruisseau")
frap=spTransform(subset(watershed,NO_COURS_DEAU=="03020471"),utm18); # FRAPPIER
chab=spTransform(subset(watershed,NO_COURS_DEAU=="03020489"),utm18); # CHABOT
# subset(watershed,NO_COURS_DEAU=="03020194"); # TOMECOD
PLSF_watershed=subset(watershed,NO_COURS_DEAU=="03020194")

PLSF=subset(lakes,lakeName1==unique(lakes$lakeName1)[43])
# bbox(PLSF)
# plot(PLSF)
# par(mar=c(0,0,0,0),oma=c(0.1,0.1,0.1,0.1))
# plot(PLSF_watershed)
# plot(crop(flowLines,PLSF_watershed),add=T,col="lightblue",lwd=2)
# plot(PLSF,add=T,col="lightblue")


PLSF_flow=spTransform(crop(flowLines,PLSF_watershed),utm18)
PLSF_watershed=spTransform(PLSF_watershed,utm18)
PLSF=spTransform(PLSF,utm18)

lakes2=spTransform(lakes,utm18)

AOI=raster::extent(gBuffer(PLSF_watershed,width=10000))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm18

# png(filename=paste0(plot.path,"/Fig1_PLSFMap.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0,0,0,0),oma=c(0.1,0.1,0.1,0.1))
# layout(matrix(c(1,2,3,3,4,5),2,3))
layout(matrix(c(1,2,2,3,3,3,4,4,5),3,3),heights=c(0.5,0.4,0.3),widths=c(0.5,0.75,1))

plot(ca.states,lwd=0.1,border="grey50")
plot(subset(ca.states,name_de=="Québec"),col="grey",add=T,lwd=0.5,border="grey50")
mtext(side=3,line=-2,'Canada')
# mapmisc::scaleBar(utm18,"bottom",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

plot(subset(ca.states,name_de=="Québec"),lwd=0.1,border="grey50")
# text(subset(ca.states,name_de=="Québec"),"name_de",cex=1.5,halo=T)
# plot(PLSF_watershed,add=T)
plot(watershed_lvl1,border="grey30",add=T,lwd=0.1,col="white")
plot(AOI.poly,add=T,border=adjustcolor("red",0.5),lwd=2,lty=1)
mtext(side=3,line=-2,'Québec')
# mapmisc::scaleBar(utm18,"bottom",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

bbox.lims=bbox(PLSF_watershed)
# bbox.lims=bbox(bind(wx.sites2.shp,PLSF_sites))
plot(PLSF_flow,col="lightblue",lwd=1.5,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PLSF,add=T,col="lightblue",border="grey50",lwd=0.5)
plot(crop(lakes2,PLSF_watershed),add=T,col="lightblue",border="grey50",lwd=0.5)
plot(PLSF_watershed,lwd=1.25,add=T)
plot(wx.sites2.shp,add=T,pch=21,bg="dodgerblue1")
mtext(side=3,line=-3,"Petit-lac-Saint-François\nWatershed")
mapmisc::scaleBar(utm18,"bottom",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

bbox.lims=bbox(gBuffer(PLSF,width=500))
plot(PLSF_flow,col="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(PLSF,add=T,col="lightblue",border="grey50")
plot(subset(PLSF_sites,SITE!="In_Lake"),pch=19,cex=2,add=T)
text(subset(PLSF_sites,SITE=="Inlet"),c("PLSF\nInlet"),pos=2)
text(subset(PLSF_sites,SITE=="Outlet"),c("PLSF\nOutlet"),pos=4)
box(lwd=1)
mtext(side=3,line=-2,"Petit-lac-Saint-François")
mapmisc::scaleBar(utm18,"bottom",bty="n",cex=1,seg.len=4,outer=F)


plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",
       legend=c("Area of Interest",
                "Monitoring Locations",
                "Lakes",
                "Rivers, streams and creeks",
                "Watershed"),
       pch=c(22,19,22,NA,22),pt.bg=c(adjustcolor("red",0.5),"black","lightblue",NA,"white"),
       lty=c(0,0,0,1,0),lwd=c(1,0.5,0.5,1,1),col=c("red","black","grey50","lightblue","black"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
box(lwd=1)
dev.off()