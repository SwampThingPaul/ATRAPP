## Title:      Petit-lac-Saint-François Ice on/off
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 02/14/2022

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
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


# -------------------------------------------------------------------------

ice.dat=read.xlsx(paste0(data.path,"PLSF Ice-On Ice-Off Dates.xlsx"),
                  cols=1:3,rows=1:46)
colnames(ice.dat)=c("Year","Ice_On","Ice_Off")
ice.dat$Ice_On=date.fun(convertToDate(ice.dat$Ice_On))
ice.dat$Ice_Off=date.fun(convertToDate(ice.dat$Ice_Off))
ice.dat$duration=with(ice.dat,as.numeric(Ice_Off - Ice_On))
ice.dat$year=as.numeric(substr(ice.dat$Year,1,4))
# edit
subset(ice.dat,Year=="2015-16")
ice.dat[ice.dat$Year=="2015-16","Ice_On"]=date.fun("2015-12-31")

ice.dat$DOY.Ice_On=as.numeric(format(ice.dat$Ice_On,"%j"))
ice.dat$CY.Ice_On=as.numeric(format(ice.dat$Ice_On,"%Y"))

ice.dat$DOY.Ice_Off=as.numeric(format(ice.dat$Ice_Off,"%j"))
ice.dat$CY.Ice_Off=as.numeric(format(ice.dat$Ice_Off,"%Y"))



ylim.val=c(10,500);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(2008,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

plot(DOY.Ice_On~CY.Ice_On,ice.dat,ylim=c(0,366))
points(DOY.Ice_Off~CY.Ice_Off,ice.dat)


pts.grid2 <- expand.grid(x=seq.Date(from = as.Date('2001-03-01'),to = as.Date('2001-06-01'),by = '10 days'),
                         y=seq.Date(from = as.Date('2000-11-01'),to = as.Date('2001-12-31'),by = '10 days'))
pts.grid2$z <- pts.grid2$x - pts.grid2$y
pts.grid2=subset(pts.grid2,x== as.Date('2001-03-01') | y == as.Date('2000-11-01'))

pts.grid2$x2=as.numeric(format(pts.grid2$x,"%j"))
pts.grid2$y2=as.numeric(format(pts.grid2$y,"%j"))
pts.grid2=subset(pts.grid2,z%in%seq(100, 200, by = 10) )

test=data.frame(x=seq(300,365,10),
           y=seq(90,150,10))
test$z=with(test,x-y)

xlim.val=date.fun(c("2000-04-01","2000-05-15"));xmaj=seq(xlim.val[1],xlim.val[2],"15 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
ylim.val=date.fun(c("2000-11-01","2000-12-31"));ymaj=seq(ylim.val[1],ylim.val[2],"15 days");ymin=seq(ylim.val[1],ylim.val[2],"1 days")
DOY.fun=function(x){as.numeric(format(x,"%j"))}
ice.dat$year.f=as.factor(ice.dat$year)

cols=viridis::viridis(length(ice.dat$year.f),alpha=0.75)
# png(filename=paste0(plot.path,"PLSF_IceCover.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,1.5,0.25),oma=c(2,3.5,0.25,0.5));
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.25))

plot(DOY.Ice_On~DOY.Ice_Off,ice.dat,type="n",axes=F,
     ylim=DOY.fun(ylim.val),xlim=DOY.fun(xlim.val))
abline(h=DOY.fun(ymaj),v=DOY.fun(xmaj),lty=3,col="grey",lwd=0.75)
points(DOY.Ice_On~DOY.Ice_Off,ice.dat,pch=21,bg=cols[ice.dat$year.f],cex=1.25,lwd=0.01)
with(subset(ice.dat,year%in%c(1975,2010,2015,2020)),text(DOY.Ice_Off,DOY.Ice_On,year,pos=2,cex=0.75))
axis_fun(2,DOY.fun(ymaj),DOY.fun(ymin),format(as.Date(ymaj,origin="2000-01-01"),"%b-%d"))
axis_fun(1,DOY.fun(xmaj),DOY.fun(xmin),format(as.Date(xmaj,origin="2000-01-01"),"%b-%d"),line=-0.5)
box(lwd=1)
mtext(side=2,line=3.5,"Date of Ice On")
mtext(side=1,line=2,"Date of Ice Off")
mtext(side=3,adj=0,line=0.75,"Petit-lac-Saint-François Ice Cover")
mtext(side=3,adj=0,"(1975 - 1977, 1982, 2010 - 2020)",font=3,col="grey50",cex=0.75)

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
dev.off()


# See https://github.com/hdugan/MendotaIce/blob/78daf8c2fac6a1f5b5c70f6d10a05fe3d7a6db87/mendotaIce.gif
## see https://raw.githubusercontent.com/hdugan/MendotaIce/master/mendotaIce.R
