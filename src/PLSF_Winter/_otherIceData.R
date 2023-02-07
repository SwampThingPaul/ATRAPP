## Ice data from SITES
# https://www.fieldsites.se/2023/01/16/en-GB/january-data-in-focus-long-term-ice-cover-record-from%C2%A0lake-erken%C2%A0%C2%A0-46281380
# https://meta.fieldsites.se/objects/QLqXw2wkqxXrr0ahsxeQaWbp

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
paths=paste0(wd,c("/Plots/PLSF_winter/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

dat=read.csv(paste0(data.path,"IceData_otherareas/SITES_ICE_ERK_MAI_1941-2022_L2_annual.csv"),skip=27)
dat$TIME_ICE_ON=as.Date(dat$TIME_ICE_ON)
dat$TIME_ICE_OFF=as.Date(dat$TIME_ICE_OFF)
dat$CY=as.numeric(format(dat$TIME_ICE_ON,"%Y"))


dat$DOY.Ice_On=as.numeric(format(dat$TIME_ICE_ON,"%j"))
dat$DOY.Ice_Off=as.numeric(format(dat$TIME_ICE_OFF,"%j"))
ice.dat=dat


xlim.val=date.fun(c("2000-02-01","2000-05-15"));xmaj=seq(xlim.val[1],xlim.val[2],"15 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
ylim.val=date.fun(c("2000-01-01","2000-12-31"));ymaj=seq(ylim.val[1],ylim.val[2],"15 days");ymin=seq(ylim.val[1],ylim.val[2],"1 days")
DOY.fun=function(x){as.numeric(format(x,"%j"))}
ice.dat$year.f=as.factor(ice.dat$CY)

cols=viridis::viridis(length(ice.dat$year.f),alpha=0.75)
par(family="serif",mar=c(1,1,1.5,0.25),oma=c(2,3.5,0.25,0.5));
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.25))

plot(DOY.Ice_On~DOY.Ice_Off,ice.dat,type="n",axes=F,
     ylim=DOY.fun(ylim.val),xlim=DOY.fun(xlim.val))
abline(h=DOY.fun(ymaj),v=DOY.fun(xmaj),lty=3,col="grey",lwd=0.75)
points(DOY.Ice_On~DOY.Ice_Off,ice.dat,pch=21,bg=cols[ice.dat$year.f],cex=1.25,lwd=0.01)
with(ice.dat,text(DOY.Ice_Off,DOY.Ice_On,CY,pos=2,cex=0.75))
# with(subset(ice.dat,year%in%c(1975,2010,2015,2020)),text(DOY.Ice_Off,DOY.Ice_On,year,pos=2,cex=0.75))
axis_fun(2,DOY.fun(ymaj),DOY.fun(ymin),format(as.Date(ymaj,origin="2000-01-01"),"%b-%d"))
axis_fun(1,DOY.fun(xmaj),DOY.fun(xmin),format(as.Date(xmaj,origin="2000-01-01"),"%b-%d"),line=-0.5)
box(lwd=1)
mtext(side=2,line=3.5,"Date of Ice On")
mtext(side=1,line=2,"Date of Ice Off")

plot(0:1,0:1,ann=F,axes=F,type="n")
l.b=length(ice.dat$year.f)
labs=c(min(ice.dat$year),max(ice.dat$year))
# n.bks=length(b2)-1
top.val=0.8
bot.val=0.2
mid.v.val=bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
text(x=x.max, y = c(bot.val,top.val), labels = format(labs),cex=0.75,adj=0,pos=4,offset=0.5)
text(x=mid.val,y=top.val,"Year",adj=0,cex=1,pos=3,xpd=NA)