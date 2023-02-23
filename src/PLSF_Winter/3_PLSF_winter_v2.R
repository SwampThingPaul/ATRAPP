## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Winter limno
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
library(EnvStats)

library(flextable)
library(magrittr)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_winter/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

# function 
label_wrap_fun=function(x,width){
  unlist(lapply(strwrap(x, width = width, simplify = FALSE), paste0, collapse = "\n"))
}
MDL_func=function(data,MDL,rng.val=TRUE){
  tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
  tmp=ifelse(tmp<MDL,MDL/2,tmp)
  if(rng.val==TRUE){print(range(tmp,na.rm=T))}
  return(tmp)
}


consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}
leg.fun=function(b,pal,leg.title,
                 top.val=0.8,bot.val=0.2,mid.v.val=NULL,
                 x.max=0.3,x.min=0.1,mid.val=NULL,
                 txt.offset.val=-0.01,txt.y=NULL,leg.txt=NULL,
                 txt.cex=0.75,txt.adj=0,txt.pos=4,txt.offset=0.5,
                 title.cex=0.8,title.pos=3,title.adj=0,
                 title.x=NULL,title.y=NULL,
                 leg.type=c("continuous","categorical"), ...){
  l.b=length(b)
  labs=c(paste0("< ",b[2]),paste(b[2:(l.b-2)],b[3:(l.b-1)],sep=" - "),paste(paste0(">",b[(l.b-1)])))
  n.bks=length(b)-1
  mid.v.val=if(is.null(mid.v.val)==T){bot.val+(top.val-bot.val)/2}else{mid.v.val}
  
  mid.val=if(is.null(mid.val)==T){x.min+(x.max-x.min)/2}else{mid.val}
  if(leg.type=="continuous"){
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    rasterImage(legend_image,x.min,bot.val,x.max,top.val)
    txt.y=if(is.null(txt.y)==T){c(bot.val,top.val)}else(txt.y)
    leg.txt=if(is.null(leg.txt)==T){format(c(min(b),max(b)))}else(leg.txt)
    text(x=x.max, y = txt.y, labels =leg.txt,cex=txt.cex,adj=txt.adj,pos=txt.pos,offset=txt.offset, ...)
  }
  if(leg.type=="categorical"){
    bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
    rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal),lty=0)
    text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, 
         labels = rev(labs),cex=txt.cex,xpd=NA,pos=txt.pos,adj=txt.adj)
  }
  
  title.x=if(is.null(title.x)==T){mid.val}else{title.x}
  title.y=if(is.null(title.y)==T){top.val}else{title.y}
  text(x=title.x,y=title.y,leg.title,adj=title.adj,cex=title.cex,pos=title.pos,xpd=NA)
}
# Weather -----------------------------------------------------------------
library(weathercan)
# stations_dl()
# example
# weather_dl(station_ids=42203,start = "2017-01-06", end = "2017-01-10")

# dates=date.fun(c("2010-10-01","2021-09-30"))
dates=date.fun(c("1978-10-01","2021-09-30"))

subset(stations(),station_name=="BROMPTONVILLE")
subset(stations(),station_id%in%c(5327,5322))

wx.sites=subset(stations(),station_id%in%c(5327,5322)&interval=="day")

# subset(stations(),station_name=="BONSECOURS")
# subset(stations(),climate_id==7024440)
weathercan:::get_html(station_id=5327,interval="day")

wx.dat=data.frame(weather_dl(station_ids=c(5327,5322),start = dates[1], end = dates[2],interval="day",trim=F))
wx.dat$date=date.fun(wx.dat$date)
wx.dat$CY=as.numeric(format(wx.dat$date,"%Y"))
wx.dat$WY=WY(wx.dat$date,WY.type="Fed")

plot(max_temp~date,wx.dat)
plot(mean_temp~date,wx.dat)
plot(total_precip~date,wx.dat)
plot( snow_grnd~date,wx.dat)

plot(snow_grnd~date,subset(wx.dat,station_name=="BROMPTONVILLE"&WY==2000))
plot(snow_grnd~date,subset(wx.dat,station_name=="BONSECOURS"&WY==2000))

# Spatially averaged
# wx.dat.mean=ddply(wx.dat,c("date"),summarise,mean.min_temp=mean(min_temp,na.rm=T))
# wx.dat.mean$inter.mean.min_temp=zoo::na.approx(wx.dat.mean$mean.min_temp)
# plot(mean.min_temp~date,wx.dat.mean)

wx.dat.da=ddply(wx.dat,c("date","CY"),summarise,
                mean.min_temp=mean(min_temp,na.rm=T),
                mean.total_precip=mean(total_precip,na.rm=T),
                mean.total_rain=mean(total_rain,na.rm=T),
                mean.total_snow=mean(total_snow,na.rm=T),
                mean.snow_gnd=mean(snow_grnd,na.rm=T))
plot(mean.snow_gnd~date,wx.dat.da)
# wx.dat.mean=ddply(subset(wx.dat,station_id==5327),c("CY"),summarise,
#                   mean.min_temp=mean(min_temp,na.rm=T),
#                   sum.total_precip=sum(Tprecip,na.rm=T)*0.1,
#                   RF.nas=sum(is.na(total_precip)))
# wx.dat.mean=ddply(wx.dat.da,c("CY"),summarise,
#                   mean.min_temp=mean(mean.min_temp,na.rm=T),
#                   sum.total_precip=sum(mean.total_precip,na.rm=T)*0.1,
#                   prep.nas=sum(is.na(mean.total_precip)),
#                   sum.RF=sum(mean.total_rain,na.rm=T)*0.1,
#                   sum.SF=sum(mean.total_snow,na.rm=T)*0.1)

wx.dat.mean=wx.dat.da
wx.dat.mean$inter.mean.min_temp=zoo::na.approx(wx.dat.mean$mean.min_temp)

wx.dat.mean$WY=WY(wx.dat.mean$date,WY.type="Fed")
wx.dat.mean$CY=as.numeric(format(wx.dat.mean$date,"%Y"))
wx.dat.mean$month=as.numeric(format(wx.dat.mean$date,"%m"))
wx.dat.mean$winter=with(wx.dat.mean,ifelse(month%in%c(12,1:2),1,0))
wx.dat.mean$freeze.day=with(wx.dat.mean,ifelse(mean.min_temp<0,1,0))
wx.dat.mean$freeze.day2=with(wx.dat.mean,ifelse(inter.mean.min_temp<0,1,0))
wx.dat.mean$WY.cumsnow=with(wx.dat.mean,ave(ifelse(is.na(mean.total_snow),0,mean.total_snow),WY,FUN = function(x)cumsum(x)))
wx.dat.mean$WY.cumrain=with(wx.dat.mean,ave(ifelse(is.na(mean.total_rain),0,mean.total_rain),WY,FUN = function(x)cumsum(x)))
wx.dat.mean$WY.cumFDD=with(wx.dat.mean,ave(ifelse(inter.mean.min_temp<0,inter.mean.min_temp,0),WY,FUN = function(x)cumsum(x)))
wx.dat.mean$WY.DOY=hydro.day(wx.dat.mean$date,WY.type="Fed")

plot(WY.cumFDD~WY.DOY,wx.dat.mean)

WYmax=aggregate(WY.cumsnow~WY,wx.dat.mean,max)
colnames(WYmax)=c("WY","MaxSnow")

wx.dat.mean=merge(wx.dat.mean,WYmax,"WY")
wx.dat.mean$WY.cumsnow2=with(wx.dat.mean,ifelse(WY.cumsnow==MaxSnow,NA,WY.cumsnow))


# plot(mean.min_temp~month,wx.dat.mean)
fdd.month=ddply(wx.dat.mean,c("CY","month","WY","winter"),summarise,
                FDD=sum(ifelse(mean.min_temp<0,abs(mean.min_temp),NA),na.rm=T),
                Tprep=sum(mean.total_precip,na.rm=T))
fdd.winter=ddply(subset(wx.dat.mean,winter==1),c("WY"),summarise,
                 # FDD=sum(ifelse(mean.min_temp<0,(mean.min_temp*-1),NA),na.rm=T),
                 FDD=sum(ifelse(mean.min_temp<0,(mean.min_temp),NA),na.rm=T),
                 mean.temp=mean(inter.mean.min_temp,na.rm=T),
                 Tprep=sum(mean.total_precip,na.rm=T))
ann.precip=ddply(wx.dat.mean,c("WY"),summarise,
                 sum.total_precip=sum(mean.total_precip,na.rm=T)*0.1,
                 prep.nas=sum(is.na(mean.total_precip)),
                 sum.RF=sum(mean.total_rain,na.rm=T)*0.1,
                 sum.SF=sum(mean.total_snow,na.rm=T)*0.1)
plot(sum.total_precip~WY,ann.precip)


range(fdd.winter$mean.temp)
range(fdd.winter$FDD)

# png(filename=paste0(plot.path,"PLSF_climate.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(3,3,0.5,0.25));
layout(matrix(1:2,2,1))

xlim.val=c(1979,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(-20,-5);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
bks=seq(-20,-7,0.5)
cols=viridis::plasma(length(bks))
col.vals=findInterval(fdd.winter$mean.temp,bks)

plot(mean.temp~WY,fdd.winter,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(fdd.winter,pt_line(WY,mean.temp,2,"grey40",1,21,cols[col.vals],1.25,0.01))
mod=mblm::mblm(mean.temp~WY,fdd.winter)
x.vals=data.frame(WY=seq(min(fdd.winter$WY),max(fdd.winter$WY),length.out=100))
mod.pred=predict(mod,x.vals,interval="confidence")
lines(x.vals$WY,mod.pred[,1],col="indianred1",lwd=2)
lines(x.vals$WY,mod.pred[,2],lty=2,col="indianred1")
lines(x.vals$WY,mod.pred[,3],lty=2,col="indianred1")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,'Mean Winter Air Temp (\u2103)')


ylim.val=c(-1700,-700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
bks=seq(-1700,-700,100)
cols=viridis::plasma(length(bks),direction=-1)
col.vals=findInterval(fdd.winter$FDD,bks)

plot(FDD~WY,fdd.winter,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(fdd.winter,pt_line(WY,FDD,2,"grey40",1,21,cols[col.vals],1.25,0.01))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mod=mblm::mblm(FDD~WY,fdd.winter)
x.vals=data.frame(WY=seq(min(fdd.winter$WY),max(fdd.winter$WY),length.out=100))
mod.pred=predict(mod,x.vals,interval="confidence")
lines(x.vals$WY,mod.pred[,1],col="indianred1",lwd=2)
lines(x.vals$WY,mod.pred[,2],lty=2,col="indianred1")
lines(x.vals$WY,mod.pred[,3],lty=2,col="indianred1")

# mtext(side=2,line=2.5,label_wrap_fun('Winter Cumulative Freezing Degree Days (\u2103)',30))
mtext(side=2,line=2.75,'Winter Cumulative\nFreezing Degree Days (\u2103)')
mtext(side=1,line=2.5,'Water Year\n(Oct - Sept)')
dev.off()

# png(filename=paste0(plot.path,"PLSF_cumsnow.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,3,0.5,0.25));
layout(matrix(1:2,1,2),width=c(1,0.4))
xlim.val=c(0,365);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

WYs=unique(wx.dat.mean$WY)
cols=adjustcolor(colorRampPalette(c("blue","dodgerblue1","indianred1","red"))(length(WYs)),0.5)#viridisLite::turbo(length(WYs),alpha=0.5)
plot(WY.cumsnow~WY.DOY,wx.dat.mean,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:length(WYs)){
  lines(WY.cumsnow~WY.DOY,subset(wx.dat.mean,WY==WYs[i]),col=cols[i],lwd=1.75)
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,'Days of Water Year')
mtext(side=2,line=2.5,'Cumulative Snowfall (mm)')

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(range(WYs),cols,leg.title="Water Year (Oct - Sept)",leg.type = "continuous")
dev.off()

# png(filename=paste0(plot.path,"PLSF_cumPrep.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,3,0.5,0.25));
layout(matrix(c(1:3,3),2,2),width=c(1,0.4))
xlim.val=c(0,365);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

WYs=unique(wx.dat.mean$WY)
cols=adjustcolor(colorRampPalette(c("blue","dodgerblue1","indianred1","red"))(length(WYs)),0.5)#viridisLite::turbo(length(WYs),alpha=0.5)
plot(WY.cumsnow~WY.DOY,wx.dat.mean,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:length(WYs)){
  lines(WY.cumsnow~WY.DOY,subset(wx.dat.mean,WY==WYs[i]),col=cols[i],lwd=1.75)
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=1.5,'Days of Water Year')
mtext(side=2,line=2.5,'Cumulative Snowfall (mm)')

ylim.val=c(0,1500);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(WY.cumrain~WY.DOY,wx.dat.mean,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:length(WYs)){
  lines(WY.cumrain~WY.DOY,subset(wx.dat.mean,WY==WYs[i]),col=cols[i],lwd=1.75)
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,'Days of Water Year')
mtext(side=2,line=2.5,'Cumulative Rainfall (mm)')

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(range(WYs),cols,leg.title="Water Year (Oct - Sept)",leg.type = "continuous")
dev.off()


##

# unlist(lapply(strwrap("Winter Cumulative Freezing Degree Days", width = 20, simplify = FALSE), paste0, collapse = "\n"))

plot(FDD~WY,subset(fdd.month,winter==1))
subset(wx.dat.mean,WY==2015&month==2)
# wx.dat2=data.frame(weather_dl(station_ids=5322,start = dates[1], end = dates[2],interval="day"))
# wx.dat2$date=date.fun(wx.dat$date)
# 
# plot(max_temp~date,wx.dat2)
# plot(total_precip~date,wx.dat2)

with(fdd.winter,cor.test(WY,mean.temp,method="kendall"))
mblm(mean.temp~WY,fdd.winter)
pt.rslt=trend::pettitt.test(fdd.winter$mean.temp)
pt.rslt
fdd.winter[pt.rslt$estimate,]
plot(ecdf(fdd.winter$mean.temp))

with(subset(fdd.winter,WY%in%seq(2010,2020,1)),cor.test(WY,mean.temp,method="kendall"))
range(subset(fdd.winter,WY%in%seq(2010,2020,1))$mean.temp)


with(fdd.winter,cor.test(WY,FDD,method="kendall"))
mblm(FDD~WY,fdd.winter)
pt.rslt=trend::pettitt.test(fdd.winter$FDD)
pt.rslt
fdd.winter[pt.rslt$estimate,]
plot(ecdf(fdd.winter$FDD))

with(subset(fdd.winter,WY%in%seq(2010,2020,1)),cor.test(WY,FDD,method="kendall"))
range(subset(fdd.winter,WY%in%seq(2010,2020,1))$FDD)
range(fdd.winter$FDD)
# winter precip
with(fdd.winter,cor.test(WY,Tprep,method="kendall"))
mblm(FDD~WY,fdd.winter)
with(subset(fdd.winter,WY%in%seq(2010,2020,1)),cor.test(WY,Tprep,method="kendall"))


with(ann.precip,cor.test(WY,sum.total_precip,method="kendall"))
with(subset(ann.precip,WY>=2010),cor.test(WY,sum.total_precip,method="kendall"))
mblm(sum.total_precip~WY,subset(ann.precip,WY>=2010))

## Climate index 
## check out rsoi package https://cran.r-project.org/web/packages/rsoi/index.html
# Multivariate ENSO -------------------------------------------------------
# https://psl.noaa.gov/enso/mei/
# row.count=length(seq(1979,2022,1))
# var.names=paste0(c("D",substring(month.name,1,1)),substring(month.name,1,1))[1:12]
# ENSO.dat=read.table("https://psl.noaa.gov/enso/mei/data/meiv2.data",
#                     header=F,skip=1,sep="",na.string="-999.00",
#                     nrows=row.count)
# colnames(ENSO.dat)=c("YR",var.names)
# 
# ENSO.dat.melt=melt(ENSO.dat,id.vars = "YR")
# ENSO.dat.melt$WY=with(ENSO.dat.melt,ifelse(variable%in%c("ON",'ND'),YR+1,YR))
# 
# ENSO.winter=ddply(subset(ENSO.dat.melt,variable%in%c("DJ","JF","FM")),"WY",summarise,ENSO.mean=mean(value,na.rm=T))
# plot(ENSO.mean~WY,ENSO.winter,type="b")
# 
# with(ENSO.winter,cor.test(WY,ENSO.mean,method="kendall"))
# with(subset(ENSO.winter,WY>=2000),cor.test(WY,ENSO.mean,method="kendall"))

mei_link = "https://www.esrl.noaa.gov/psd/enso/mei/data/meiv2.data"

years = strsplit(readLines(mei_link, n = 1), "     ", 2)[[1]]
rows = diff(as.numeric(years)) + 1
months = c("DJ", "JF", "FM", "MA", "AM", "MJ", "JJ", "JA", "AS", "SO", "ON", "ND")

mei =  read.csv(mei_link,    # reset connection
                header = FALSE, 
                col.names = c("Year", months),
                nrows = rows,
                skip = 1, 
                na.strings = "-999.00",
                sep = "",
                stringsAsFactors = FALSE)

ENSO.dat = expand.grid(Year = mei$Year, Month = months)
ENSO.dat$MEI = c(as.matrix(mei[, months]))
ENSO.dat$Month = factor(ENSO.dat$Month, levels = months)
ENSO.dat$Date = as.Date(paste0(ENSO.dat$Year, "-", as.numeric(ENSO.dat$Month) ,"-01"), "%Y-%m-%d")
ENSO.dat$Phase = factor(ifelse(ENSO.dat$MEI <= -0.5,"Cool Phase/La Nina",
                           ifelse(ENSO.dat$MEI >= 0.5, "Warm Phase/El Nino", 
                                  "Neutral Phase")))
ENSO.dat$Phase = factor(ENSO.dat$Phase, levels = c("Cool Phase/La Nina", 
                                           "Neutral Phase",
                                           "Warm Phase/El Nino"), ordered = TRUE)
ENSO.dat$month.num=as.numeric(format(ENSO.dat$Date,"%m"))

ENSO.dat$WY=with(ENSO.dat,ifelse(Month%in%c("ON",'ND'),Year+1,Year))

ENSO.winter=ddply(subset(ENSO.dat,Month%in%c("DJ","JF","FM")),"WY",summarise,ENSO.mean=mean(MEI,na.rm=T))
plot(ENSO.mean~WY,ENSO.winter,type="b")

# AMO ---------------------------------------------------------------------
## AMO https://psl.noaa.gov/data/timeseries/AMO/

vars=c('year',month.abb)
row.count=length(seq(1856,2022,1))
noaa.amo.path="https://psl.noaa.gov/data/correlation/amon.us.long.data"

# AMO.dat=read.table("https://psl.noaa.gov/data/correlation/amon.us.long.data",header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
AMO.dat=read.table("https://psl.noaa.gov/data/correlation/amon.us.long.data",
                   header=F,skip=1,sep="\t",na.string="-99.990",
                   nrows=row.count)
spl=strsplit(AMO.dat$V1,split="   ")
spl[167]
AMO.dat=data.frame(year=sapply(spl,"[",1),
                   Jan=sapply(spl,"[",2),
                   Feb=sapply(spl,"[",3),
                   Mar=sapply(spl,"[",4),
                   Apr=sapply(spl,"[",5),
                   May=sapply(spl,"[",6),
                   Jun=sapply(spl,"[",7),
                   Jul=sapply(spl,"[",8),
                   Aug=sapply(spl,"[",9),
                   Sep=sapply(spl,"[",10),
                   Oct=sapply(spl,"[",11),
                   Nov=sapply(spl,"[",12),
                   Dec=sapply(spl,"[",13)
)
AMO.dat[167,]$Feb=sapply(strsplit(AMO.dat[167,]$Feb,split="  "),"[",1)
AMO.dat[,2:13]=sapply(AMO.dat[,2:13],as.numeric)

AMO.dat.melt=melt(AMO.dat,id.vars="year")
AMO.dat.melt=merge(AMO.dat.melt,data.frame(variable=month.abb,month=1:12))
AMO.dat.melt$Date.mon=with(AMO.dat.melt,date.fun(paste(year,month,"01",sep="-")))
AMO.dat.melt=AMO.dat.melt[order(AMO.dat.melt$Date.mon),c("Date.mon","month","year","value")]
AMO.dat.melt$year=as.numeric(AMO.dat.melt$year)
AMO.dat.melt$month=as.numeric(AMO.dat.melt$month)
# AMO.dat.melt$warm=with(AMO.dat.melt,ifelse(value>0,value,0))
# AMO.dat.melt$dry=with(AMO.dat.melt,ifelse(value<0,value,0))
# AMO.dat.melt$ma=zoo::rollmean(AMO.dat.melt$value,k=120,align="center",na.pad=T);# consistent with NOAA smoothing
# # with(AMO.dat.melt,c(rep(NA,120),zoo::rollapply(value,width=121,FUN=function(x)mean(x,na.rm=T))))
# AMO.dat.melt$ma.warm=with(AMO.dat.melt,ifelse(ma>0,ma,0))
# AMO.dat.melt$ma.cool=with(AMO.dat.melt,ifelse(ma<0,ma,0))
# AMO.dat.melt$WY=WY(AMO.dat.melt$Date.mon)
# head(AMO.dat.melt)
# tail(AMO.dat.melt)

plot(value~Date.mon,AMO.dat.melt)
# plot(ma~Date.mon,AMO.dat.melt)


# PDO ---------------------------------------------------------------------
# # https://www.ncdc.noaa.gov/teleconnections/pdo/
pdo=read.delim("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
               skip=1,header=T,sep="",na.strings="-99.99")
nrow.val=length(seq(1854,2022,1))
head.val=c("yr",month.abb)

#pdo=read.table(paste0(data.path,"NOAA/PDO/data.txt"),skip=2,header=F,col.names=head.val,nrows = nrow.val-1)
head(pdo);tail(pdo)
# pdo[169,]$Feb=substr(pdo[169,]$Feb,1,5)
# pdo$Feb=as.numeric(pdo$Feb)

pdo$Year=as.numeric(pdo$Year)
pdo=melt(pdo,id.var="Year")
pdo$month.num=with(pdo,as.numeric(match(variable,month.abb)))
pdo$winter=with(pdo,ifelse(as.numeric(month.num)%in%c(12,1:2),1,0))
pdo$date.val=with(pdo,date.fun(paste(Year,month.num,"01",sep="-")))
pdo$WY=with(pdo,WY(date.val,'Fed'))
pdo=pdo[order(pdo$date.val),]
pdo.winter.mean=ddply(subset(pdo,winter==1&WY<2023),c("WY","winter"),summarise,mean.val=mean(value,na.rm=T))
pdo.mean=ddply(subset(pdo,WY<2023),c("WY"),summarise,mean.val=mean(value,na.rm=T))

plot(mean.val~WY,pdo.winter.mean,type="l")
plot(mean.val~WY,subset(pdo.winter.mean,WY%in%seq(2009,2020,1)),type="l")

plot(mean.val~WY,subset(pdo.mean,WY%in%seq(2009,2020,1)),type="l")
plot(value~date.val,subset(pdo,WY%in%seq(2009,2020,1)),type="l")

pdo$cool=with(pdo,ifelse(value<0,value,0))
pdo$warm=with(pdo,ifelse(value>0,value,0))

# png(filename=paste0(plot.path,"xx_pdo.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(2,2,1,0.25));

ylim.val=c(-3,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2009-10-01","2020-09-30"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

plot(value~date.val,pdo,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(subset(pdo,WY%in%seq(2009,2020,1)),segments(date.val,rep(0,length(date.val)),date.val,value,
                  lwd=2,col=adjustcolor(ifelse(value<0,"dodgerblue1","indianred1"),0.5),lmitre=1))
with(subset(pdo,WY%in%seq(2009,2020,1)),lines(value~date.val))
abline(h=0)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=1.5,"PDO Index")
mtext(side=1,line=1.5,"Date (Month-Year)")
dev.off()


# SOI ---------------------------------------------------------------------
# # https://www.ncei.noaa.gov/access/monitoring/enso/soi
# soi=read.delim("https://www.cpc.ncep.noaa.gov/data/indices/soi",
#                skip=3,header=T,sep="",na.strings="-999.9")

soi=readLines("https://www.cpc.ncep.noaa.gov/data/indices/soi")
soi <- soi[grep("STANDARDIZED", soi):length(soi)]
soi <- soi[grep("YEAR", soi):length(soi)]

fwf_file <- tempfile(fileext = ".fwf")
writeLines(soi, fwf_file)

soi <- utils::read.fwf(fwf_file, skip = 1, 
                       widths = c(4, rep(6, 12)), 
                       col.names = c("YEAR", month.abb), 
                       na.strings = "-999.9")

soi$Year=as.numeric(soi$YEAR)
soi=melt(soi,id.var="YEAR")
soi$month.num=with(soi,as.numeric(match(variable,month.abb)))
soi$winter=with(soi,ifelse(as.numeric(month.num)%in%c(12,1:2),1,0))
soi$date.val=with(soi,date.fun(paste(YEAR,month.num,"01",sep="-")))
soi$WY=with(soi,WY(date.val,'Fed'))
soi=soi[order(soi$date.val),]
soi.winter.mean=ddply(subset(soi,winter==1&WY<2023),c("WY","winter"),summarise,mean.val=mean(value,na.rm=T))
soi.mean=ddply(subset(soi,WY<2023),c("WY"),summarise,mean.val=mean(value,na.rm=T))

plot(mean.val~WY,soi.winter.mean,type="l")
plot(mean.val~WY,subset(soi.winter.mean,WY%in%seq(2009,2020,1)),type="l")

plot(mean.val~WY,subset(soi.winter.mean,WY%in%seq(2009,2020,1)),type="l")
plot(value~date.val,subset(soi,WY%in%seq(2009,2020,1)),type="l")


# nao ---------------------------------------------------------------------
nao_link="https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table"
nao.dat = read.fwf(nao_link, 
               widths = c(4, rep(7, 12)),
               header = FALSE, 
               col.names = c("Year", month.abb))

nao.dat$Year=as.numeric(nao.dat$Year)
nao.dat=melt(nao.dat,id.var="Year")
nao.dat=subset(nao.dat,is.na(value)==F)
nao.dat$month.num=with(nao.dat,as.numeric(match(variable,month.abb)))
nao.dat$winter=with(nao.dat,ifelse(as.numeric(month.num)%in%c(12,1:2),1,0))
nao.dat$date.val=with(nao.dat,date.fun(paste(Year,month.num,"01",sep="-")))
nao.dat$WY=with(nao.dat,WY(date.val,'Fed'))
nao.dat=subset(nao.dat,is.na(Year)==F)
nao.dat=nao.dat[order(nao.dat$date.val),]
nao.dat$value=as.numeric(nao.dat$value)
nao.dat.winter.mean=ddply(subset(nao.dat,winter==1&WY<2023),c("WY","winter"),summarise,mean.val=mean(value ,na.rm=T))
nao.dat.mean=ddply(subset(nao.dat,WY<2023),c("WY"),summarise,mean.val=mean(value,na.rm=T))

plot(mean.val~WY,subset(nao.dat.winter.mean,WY%in%seq(2009,2020,1)),type="l")
plot(value~date.val,subset(nao.dat,WY%in%seq(2009,2020,1)),type="l")


# ONI ---------------------------------------------------------------------
## https://www.climate.gov/news-features/understanding-climate/climate-variability-oceanic-ni%C3%B1o-index
## https://climatedataguide.ucar.edu/climate-data/nino-sst-indices-nino-12-3-34-4-oni-and-tni

oni_link ="http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt"
oni = read.table(oni_link, 
                 col.names = c("Year","Month","TOTAL","ClimAdjust","dSST3.4"),
                 skip = 1,
                 stringsAsFactors = FALSE)

oni = oni[,c("Year","Month","dSST3.4")]
oni$Date = date.fun(paste0(oni$Year,"-",oni$Month,"-01"), form="%Y-%m-%d")

##Month label to collapse
oni$Month = format(oni$Date,'%b')
## Create 3 month average window. Each row is a month
oni$ONI = as.numeric(stats::filter(oni$dSST3.4,rep(1/3,3), sides=2))

oni$ONI_month_window <- sapply(1:nrow(oni), function(x) paste(substr(oni$Month[x-1],1,1),
                                                              substr(oni$Month[x],1,1),
                                                              substr(oni$Month[x+1],1,1),
                                                              sep=""))
oni$ONI_month_window[c(1, nrow(oni))] <- NA
oni$phase = factor(ifelse(oni$ONI <= -0.5,"Cool Phase/La Nina",
                          ifelse(oni$ONI >= 0.5, "Warm Phase/El Nino", "Neutral Phase")))
oni$month.num=as.numeric(format(oni$Date,"%m"))

strong.elNino=ddply(oni[which(oni$ONI>1.5),],"Year",summarise,
                    max.date.val=Date[which.max(ONI)],
                    max.ONI.val=ONI[which.max(ONI)])
strong.elNino$date_diff=c(NA,diff(strong.elNino$max.date.val))

strong.laNina=ddply(oni[which(oni$ONI<(-1.5)),],"Year",summarise,
                    max.date.val=Date[which.max(ONI)],
                    max.ONI.val=ONI[which.max(ONI)])


# https://ggweather.com/enso/oni.htm
# png(filename=paste0(plot.path,"X_ONI_index.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,1),oma=c(0.5,3,0.5,0.25));
layout(matrix(1:2,2,1),heights=c(1,0.2))
xlim.val=date.fun(c("1978-05-01","2022-05-01"));by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ylim.val=c(-2.0,2.8);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(ONI~Date,oni,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
xx=date.fun(c("2009-10-01","2020-10-01","2020-10-01","2009-10-01"))
yy=c(-3,-3,3,3)
polygon(xx,yy,col=adjustcolor("grey",0.5),border=NA)
with(subset(oni,is.na(ONI)==F),
     shaded.range(Date,ifelse(ONI<0,ONI,0),ifelse(ONI>0,ONI,0),
                  bg="lightblue",col="black",lty=1,col.adj = 0.5,lwd=0.5))
# abline(h=seq(0.5,2.5,0.5),col="red",lty=2,lwd=0.5)
# abline(h=seq(-2.5,-0.5,0.5),col="blue",lty=2,lwd=0.5)
abline(h=c(0.5,2.5),col="red",lty=2,lwd=0.5)
abline(h=c(-2.5,-0.5),col="blue",lty=2,lwd=0.5)
points(max.ONI.val~max.date.val,strong.elNino,pch=21,bg="red",cex=0.75)
# text(strong.elNino$max.date.val,strong.elNino$max.ONI.val,strong.elNino$Year)
points(max.ONI.val~max.date.val,strong.laNina,pch=21,bg="blue",cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1))
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.5)
box(lwd=1)
mtext(side=2,line=2.25,"Oceanic Ni\u00F1o Index")
mtext(side=1,line=1.5,"Date (Month - Year)")
par(mar=c(0.1,1,1,1))
plot(0:1,0:1,type="n",ann=F,axes=F)
legend("center",legend=c("Strong El Ni\u00F1o","Strong La Ni\u00F1a"),
       pch=c(21),pt.bg=c("red","blue"),pt.cex=c(1),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)
dev.off()

# Iceon/off ---------------------------------------------------------------
ice.dat=read.xlsx(paste0(data.path,"PLSF Ice-On Ice-Off Dates.xlsx"),
                  cols=1:3,rows=1:46)
colnames(ice.dat)=c("Year","Ice_On","Ice_Off")
ice.dat$Ice_On=date.fun(convertToDate(ice.dat$Ice_On))
ice.dat$Ice_Off=date.fun(convertToDate(ice.dat$Ice_Off))
ice.dat$duration=with(ice.dat,as.numeric(Ice_Off - Ice_On))
ice.dat$year=as.numeric(substr(ice.dat$Year,1,4))


ice.dat2=subset(ice.dat,Ice_On>date.fun("2010-12-01"))
ice_period=data.frame()
for(i in 1:nrow(ice.dat2)){
  tmp=seq(ice.dat2$Ice_On[i],ice.dat2$Ice_Off[i],"1 days")
  ice_period=rbind(ice_period,data.frame(Date=tmp,ice=1))
  print(i)
}

ice_period=merge(data.frame(Date=seq(date.fun("2010-10-01"),date.fun("2021-09-29"),"1 days")),
                 ice_period,all.x=T)
ice_period$ice=with(ice_period,ifelse(is.na(ice),0,ice))
ice_period$WY=WY(ice_period$Date,"Fed")

ice_period$Ice_cum=with(ice_period,ave(ice,WY,FUN=function(x)cumsum(x)))
ice_period$Ice_cum=with(ice_period,ifelse(ice==0,0,Ice_cum))
plot(Ice_cum~Date,ice_period)

# iceon_period=consec.startend(ice_period$ice>0)
# ice_period$sum.ice=0
# for(i in 1:length(iceon_period$ends)){
#   ice_period[iceon_period$ends[i],]$sum.ice=with(ice_period[c(iceon_period$starts[i]:iceon_period$ends[i]),],sum(ice,na.rm=T))
# }

plot(duration~year,ice.dat2)
with(ice.dat2,cor.test(year, duration,method="kendall" ))

ice.dat$DOY.Ice_On=as.numeric(format(ice.dat$Ice_On,"%j"))
ice.dat$CY.Ice_On=as.numeric(format(ice.dat$Ice_On,"%Y"))

ice.dat$DOY.Ice_Off=as.numeric(format(ice.dat$Ice_Off,"%j"))
ice.dat$CY.Ice_Off=as.numeric(format(ice.dat$Ice_Off,"%Y"))
ice.dat$periods=with(ice.dat,ifelse(year%in%c(1975:1981),"ref","curr"))
ice.dat$periods=factor(ice.dat$periods,levels=c("ref","curr"))

kruskal.test(duration~periods,ice.dat)

# png(filename=paste0(plot.path,"PLSF_IceCover_dur.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.25),oma=c(2,3.5,0.25,0.5));

ylim.val=c(100,180);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=viridis::viridis(2,alpha=0.5)
boxplot(duration~periods,ice.dat,ylim=ylim.val,ann=F,axes=F,col=cols)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
axis_fun(1,1:2,1:2,c("Reference Period\n(1975 - 1977, 1982)","Current\n(2010 - 2021)"),line=0.5)
mtext(side=2,line=3,"Duration of Ice Cover (Days)")
dev.off()


xlim.val=date.fun(c("2000-04-01","2000-05-15"));xmaj=seq(xlim.val[1],xlim.val[2],"15 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
ylim.val=date.fun(c("2000-11-01","2000-12-31"));ymaj=seq(ylim.val[1],ylim.val[2],"15 days");ymin=seq(ylim.val[1],ylim.val[2],"1 days")
DOY.fun=function(x){as.numeric(format(x,"%j"))}
ice.dat$year.f=as.factor(ice.dat$year)

cols=viridis::viridis(length(ice.dat$year.f),alpha=0.75)
# png(filename=paste0(plot.path,"PLSF_IceCover.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,1.5,0.25),oma=c(2,3.5,0.25,0.5));
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.25))

plot(DOY.Ice_On~DOY.Ice_Off,ice.dat,type="n",axes=F,
     ylim=DOY.fun(ylim.val),xlim=DOY.fun(xlim.val))
abline(h=DOY.fun(ymaj),v=DOY.fun(xmaj),lty=3,col="grey",lwd=0.75)
points(DOY.Ice_On~DOY.Ice_Off,ice.dat,pch=21,bg=cols[ice.dat$year.f],cex=1.25,lwd=0.01)
with(ice.dat,text(DOY.Ice_Off,DOY.Ice_On,year,pos=2,cex=0.75))
# with(subset(ice.dat,year%in%c(1975,2010,2015,2020)),text(DOY.Ice_Off,DOY.Ice_On,year,pos=2,cex=0.75))
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

# WQ Data -----------------------------------------------------------------
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


## From Cavaliere and Baulch (2021)
wq.dat$diff.temp=with(wq.dat,ave(Temp.C,Site,FUN=function(x) c(NA,diff(x))))
wq.dat$diff.day=with(wq.dat,ave(as.numeric(Date),Site,FUN=function(x) c(NA,diff(x*1/60*1/60*1/24))))
wq.dat$TCI=with(wq.dat,diff.temp/diff.day)

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


plot(TOC.mgL~Date,subset(wq.dat,Site=="Lake_Outlet"&is.na(TOC.mM)==F))
N.obs(subset(wq.dat,Site=="Lake_Outlet")$TOC.mgL)
N.obs(subset(wq.dat,Site=="Lake_Outlet")$SolOC.mgL)

range(subset(wq.dat,Site=="Lake_Outlet"&is.na(TOC.mM)==F)$Date)

ylim.val=c(10,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2019-11-01","2020-05-01"));xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")

plot(TOC_TON~Date,subset(wq.dat,Site=="Lake_Outlet"&is.na(TOC.mM)==F),xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(wq.dat,Site=="Lake_Outlet"&is.na(TOC.mM)==F),pt_line(Date,TOC_TON,2,"dodgerblue1",1,21,"dodgerblue1"))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TOC:TON (molar ratio)")
mtext(side=1,line=2,"Date (Month-Year)")

## Explore TCI 
plot(DIN.mgL~TCI,subset(wq.dat,Site=="Lake_Outlet"))
mod=lm(DIN.mgL~TCI,subset(wq.dat,Site=="Lake_Outlet"))
library(segmented)
seg.mod=segmented(mod,~TCI)
plot(seg.mod,add=T)
plot(SRP.ugL~TCI,subset(wq.dat,Site=="Lake_Outlet"),log="y")

plot(DO.per~TCI,subset(wq.dat,Site=="Lake_Outlet"))

wq.dat$WY.may=WY(wq.dat$Date)
wq.dat$hydro.doy=hydro.day(wq.dat$Date)
wq.dat$winter=with(wq.dat,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),1,0))

plot(DO.per~hydro.doy,subset(wq.dat,winter==1&Site=="Lake_Outlet"))

mod=lm(DO.per~hydro.doy,subset(wq.dat,winter==1&Site=="Lake_Outlet"))
seg.mod=segmented(mod,~hydro.doy)
plot(seg.mod,add=T)



###

idvars=c("Date","Site")
paramvars=c(paste(c("TP","SRP","DP","DOP"),"ugL",sep="."),
            paste(c("TN","NOx",'NH4',"DIN","TON"),"mgL",sep="."),
            "Chla.ugL","Phyco.ugL", "TChl.ugL",
            "Temp.C","DO.per","TCI")

wq.dat.melt=melt(wq.dat[,c(idvars,paramvars)],id.vars = idvars)
wq.dat.melt$month=as.numeric(format(wq.dat.melt$Date,"%m"))
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)


# Ice period compare ------------------------------------------------------

wq.dat.melt$ice.period=with(wq.dat.melt,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:4),"Ice","NoIce"))
wq.dat.melt$ice.period=factor(wq.dat.melt$ice.period,level=c("NoIce","Ice"))
wq.dat.melt$WY=WY(wq.dat.melt$Date,WY.type = "FL")
wq.dat.melt$WY.oct=WY(wq.dat.melt$Date,WY.type = "Fed")

sites.vals=c("Godbout","Lake_Outlet")
## "In_Lake" not sampled during winter
ice_period.mean=ddply(subset(wq.dat.melt,Site%in%sites.vals),c("Site","WY","ice.period","variable"),summarise,
                  mean.val=mean(value,na.rm=T),
                  GM.val=exp(mean(log(value),na.rm=T)),
                  N.val=N.obs(value))



boxplot(mean.val~ice.period,subset(ice_period.mean,Site==sites.vals[1]&variable=="TP.ugL"),outline=F,loog="y")
rslt=kruskal.test(mean.val~ice.period,subset(ice_period.mean,Site==sites.vals[1]&variable=="TP.ugL"))
if(rslt$p.value<0.05){text(ylim.val[2],1.5,"*")}

rslt$p.value
rslt$statistic

rslt2=ddply(ice_period.mean,c("Site","variable"),summarise,
      stat=kruskal.test(mean.val~ice.period)$statistic,
      pval=round(kruskal.test(mean.val~ice.period)$p.value,3))
subset(rslt2,pval<0.05)

param.vars=c("TP.ugL",'SRP.ugL',"TN.mgL","DIN.mgL","NOx.mgL","NH4.mgL")
ddply(subset(ice_period.mean,variable%in%param.vars),c("Site","variable","ice.period"),summarise,
      mean.val2=mean(mean.val),
      se.val=SE(mean.val))

tmp.mean=dcast(subset(ice_period.mean,variable%in%param.vars),Site+variable~ice.period,value.var="mean.val",mean)
colnames(tmp.mean)=c("Site", "variable", "NoIce.mean", "Ice.mean")
tmp.SE=dcast(subset(ice_period.mean,variable%in%param.vars),Site+variable~ice.period,value.var="mean.val",SE)
colnames(tmp.SE)=c("Site", "variable", "NoIce.SE", "Ice.SE")

tmp=merge(tmp.mean,tmp.SE,c("Site","variable"))
tmp=merge(tmp,rslt2,c("Site","variable"),all.x=T)
tmp$variable=factor(tmp$variable,levels=param.vars)
tmp=tmp[order(tmp$Site,tmp$variable),]
tmp=tmp[,c("Site", "variable", "NoIce.mean","NoIce.SE", "Ice.mean", "Ice.SE", 
       "stat", "pval")]
tmp[tmp$variable=="NH4.mgL",3:6]=tmp[tmp$variable=="NH4.mgL",3:6]*1000;# changed NH4 units

vars1=c("TP.ugL", "SRP.ugL","NH4.mgL")
vars2=c("TN.mgL","DIN.mgL","NOx.mgL")
tmp%>%
  flextable()%>%
  colformat_double(j=3:6,i=~variable%in%vars1,digits=1,na_str = "---")%>%
  colformat_double(j=3:6,i=~variable%in%vars2,digits=2,na_str = "---")%>%
  colformat_double(j=7:8,digits=2,na_str = "---")%>%
  compose(j="pval",i=~pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="pval",i=~pval<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="pval",i=~pval<0.05)%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  merge_v(j="Site")%>%
  compose(j="variable",i=~variable=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('TN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DIN.mgL",value=as_paragraph('DIN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="NOx.mgL",value=as_paragraph('NO\u2093 (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="NH4.mgL",value=as_paragraph('NH\u2084 (\u03BCg L\u207B\u00B9)'))%>%
  set_header_labels("variable"="Parameter",
                    "Site"="Site",
                    "NoIce.mean" = "Mean",
                    "NoIce.SE" = "SE",
                    "Ice.mean" = "Mean",
                    "Ice.SE" = "SE",
                    "stat" = "\u03C7\u00B2",
                    "pval"="\u03C1-value")%>%
  add_header("NoIce.mean" = "No Ice",
             "NoIce.SE" = "No Ice",
             "Ice.mean" = "Ice",
             "Ice.SE" = "Ice",
             "stat" = "Kruskal-Wallis Statistic",
             "pval"="Kruskal-Wallis Statistic")%>%
  merge_h(part="header")%>%align(align="center",part="header")%>%
  width(width=c(1.25,1,0.75,0.75,0.75,0.75,0.75,0.75))%>%
  align(align="center",part="header")%>%
  align(j=3:8,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  hline(i=6)%>%
  vline(j=c(2,4,6,8),border=officer::fp_border(color="grey"))%>%
  fix_border_issues()%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=11,part="all")%>%
  fontsize(size=12,part="header")# %>%print("docx")
  


# ice_period.mean2=ddply(subset(wq.dat.melt,Site%in%sites.vals),c("Site","WY.oct","ice.period","variable"),summarise,
#                       mean.val=mean(value,na.rm=T),
#                       GM.val=exp(mean(log(value),na.rm=T)),
#                       N.val=N.obs(value))
# rslt3=ddply(ice_period.mean2,c("Site","variable"),summarise,
#             stat=kruskal.test(mean.val~ice.period)$statistic,
#             pval=round(kruskal.test(mean.val~ice.period)$p.value,3))
# subset(rslt2,pval<0.05)

# png(filename=paste0(plot.path,"PLSF_SeaComp_all.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:12,6,2,byrow = T),widths=c(1,1))
xaxs.cex=0.8
ylab.cex=0.9

ylim.val=c(9,300);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="TP.ugL"
  axes.lab="TP (\u03BCg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}
ylim.val=c(2,100);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="SRP.ugL"
  axes.lab="SRP (\u03BCg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)}
}
ylim.val=c(0.2,5);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="TN.mgL"
  axes.lab="TN (mg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)}
}
ylim.val=c(0.1,2);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="DIN.mgL"
  axes.lab="DIN (mg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)}
}
ylim.val=c(0.01,2);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="NOx.mgL"
  axes.lab="NO\u2093 (mg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)}
}
ylim.val=c(0.01,0.5);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="NH4.mgL"
  axes.lab="NH\u2084 (mg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)}
}
mtext(side=1,line=1,outer=T,"Season")
dev.off()

dcast(ice_period.mean,Site+ice.period~variable,value.var="mean.val",N.obs)

# png(filename=paste0(plot.path,"PLSF_SeaComp_all_v2.png"),width=4.75,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:6,6,1,byrow = T),widths=c(1,1))

sum.stats.mean=dcast(ice_period.mean,Site+ice.period~variable,value.var="mean.val",mean)
sum.stats.se=dcast(ice_period.mean,Site+ice.period~variable,value.var="mean.val",fun.aggregate = function(x) SE(x))
yaxs.cex=1
ylab.cex=0.85
ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);#ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");
param.val="TP.ugL"
axes.lab="TP (\u03BCg L\u207B\u00B9)"
x=barplot(sum.stats.mean[,param.val],space=c(0,0,0.5,0),ylim=ylim.val,ann=F,axes=F,col=rep(c("white","sky blue"),2))
errorbars(x,sum.stats.mean[,param.val],sum.stats.se[,param.val],col="black",length=0.05)
with(subset(rslt2,variable==param.val),text(c(1.0,3.5),rep(ylim.val[2],2),ifelse(pval<0.05,"*",NA),pos=1,cex=2))
axis_fun(1,x,x,NA)
axis_fun(2,ymaj,ymin,ymaj,cex=yaxs.cex);box(lwd=1)
axis_fun(3,c(1.0,3.5),c(1.0,3.5),c("Lake Inlet","Lake Outlet"),maj.tcl=0,min.tcl=0)
mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)

ylim.val=c(0,30);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);#ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");
param.val="SRP.ugL"
axes.lab="SRP (\u03BCg L\u207B\u00B9)"
x=barplot(sum.stats.mean[,param.val],space=c(0,0,0.5,0),ylim=ylim.val,ann=F,axes=F,col=rep(c("white","sky blue"),2))
errorbars(x,sum.stats.mean[,param.val],sum.stats.se[,param.val],col="black",length=0.05)
with(subset(rslt2,variable==param.val),text(c(1.0,3.5),rep(ylim.val[2],2),ifelse(pval<0.05,"*",NA),pos=1,cex=2))
axis_fun(1,x,x,NA)
axis_fun(2,ymaj,ymin,ymaj,cex=yaxs.cex);box(lwd=1)
mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);#ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");
param.val="TN.mgL"
axes.lab="TN (mg L\u207B\u00B9)"
x=barplot(sum.stats.mean[,param.val],space=c(0,0,0.5,0),ylim=ylim.val,ann=F,axes=F,col=rep(c("white","sky blue"),2))
errorbars(x,sum.stats.mean[,param.val],sum.stats.se[,param.val],col="black",length=0.05)
with(subset(rslt2,variable==param.val),text(c(1.0,3.5),rep(ylim.val[2],2),ifelse(pval<0.05,"*",NA),pos=1,cex=2))
axis_fun(1,x,x,NA)
axis_fun(2,ymaj,ymin,ymaj,cex=yaxs.cex);box(lwd=1)
mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)

ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);#ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");
param.val="DIN.mgL"
axes.lab="DIN (mg L\u207B\u00B9)"
x=barplot(sum.stats.mean[,param.val],space=c(0,0,0.5,0),ylim=ylim.val,ann=F,axes=F,col=rep(c("white","sky blue"),2))
errorbars(x,sum.stats.mean[,param.val],sum.stats.se[,param.val],col="black",length=0.05)
with(subset(rslt2,variable==param.val),text(c(1.0,3.5),rep(ylim.val[2],2),ifelse(pval<0.05,"*",NA),pos=1,cex=2))
axis_fun(1,x,x,NA)
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)

ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);#ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");
param.val="NOx.mgL"
axes.lab="NO\u2093 (mg L\u207B\u00B9)"
x=barplot(sum.stats.mean[,param.val],space=c(0,0,0.5,0),ylim=ylim.val,ann=F,axes=F,col=rep(c("white","sky blue"),2))
errorbars(x,sum.stats.mean[,param.val],sum.stats.se[,param.val],col="black",length=0.05)
with(subset(rslt2,variable==param.val),text(c(1.0,3.5),rep(ylim.val[2],2),ifelse(pval<0.05,"*",NA),pos=1,cex=2))
axis_fun(1,x,x,NA)
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)

ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);#ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");
param.val="NH4.mgL"
axes.lab="NH\u2084 (\u03BCg L\u207B\u00B9)"
x=barplot(sum.stats.mean[,param.val]*1000,space=c(0,0,0.5,0),ylim=ylim.val,ann=F,axes=F,col=rep(c("white","sky blue"),2))
errorbars(x,sum.stats.mean[,param.val]*1000,sum.stats.se[,param.val]*1000,col="black",length=0.05)
with(subset(rslt2,variable==param.val),text(c(1.0,3.5),rep(ylim.val[2],2),ifelse(pval<0.05,"*",NA),pos=1,cex=2))
axis_fun(1,x,x,rep(c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),2),padj=1,line=-1)
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=2,line=2.5,axes.lab,cex=ylab.cex)
mtext(side=1,line=1.5,outer=T,"Season")
dev.off()

## add summary stats table for paper
ddply(ice_period.mean,c("Site","ice.period","variable"),summarise,
      mean.val2=mean(mean.val,na.rm=T),
      sd.val=sd(mean.val,na.rm=T),
      N.val=N.obs(mean.val))

# WQ Climate analysis -----------------------------------------------------
wq.dat.melt$WY=WY(wq.dat.melt$Date,WY.type = "Fed")
wq.dat.melt$winter=with(wq.dat.melt,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),1,0))
wq.dat.melt$winter.txt=with(wq.dat.melt,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),"winter","other"))

sites.vals=c("Godbout","Lake_Outlet")
## "In_Lake" not sampled during winter
winter.mean=ddply(subset(wq.dat.melt,winter==1&Site%in%sites.vals),c("Site","WY","winter","variable"),summarise,
                  mean.val=mean(value,na.rm=T),
                  GM.val=exp(mean(log(value),na.rm=T)),
                  N.val=N.obs(value))

plot(mean.val~WY,subset(winter.mean,Site==sites.vals[1]&variable=="TP.ugL"&WY!=2011))

### ENSO  -----------------------------------------------------------------
winter.mean=merge(winter.mean,ENSO.winter,"WY")

plot(mean.val~ENSO.mean,subset(winter.mean,Site%in%sites.vals[1]&variable=="TN.mgL"))
plot(mean.val~ENSO.mean,subset(winter.mean,Site%in%sites.vals[1]&variable=="DIN.mgL"))
plot(mean.val~ENSO.mean,subset(winter.mean,Site%in%sites.vals[1]&variable=="TP.ugL"&ENSO.mean>(-1)))
plot(mean.val~ENSO.mean,subset(winter.mean,Site%in%sites.vals[1]&variable=="SRP.ugL"))

ddply(subset(winter.mean,Site%in%sites.vals&variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")),
      c("Site","variable"),summarise,
      r.val=cor.test(mean.val,ENSO.mean,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,ENSO.mean,method="spearman")$p.value,3))

with(subset(winter.mean,Site%in%sites.vals[1]&variable=="TP.ugL"&ENSO.mean>(-1)),
     cor.test(mean.val,ENSO.mean,method="spearman"))
## not really correlated (too much variability)

month.period=paste0(substr(month.abb,1,1)[c(12,seq(1,11,1))],substr(month.abb,1,1)[seq(1,12,1)])

wq.dat.melt$CY=with(wq.dat.melt,as.numeric(format(Date,"%Y")))
wq.dat.melt$MEI.yr=with(wq.dat.melt,ifelse(month==12,CY+1,CY))

wq.dat.melt.bimonth=data.frame()
vals.1=c(12,seq(1,11,1))
vals.2=seq(1,12,1)
for(i in 1:12){
  tmp=subset(wq.dat.melt,month%in%c(vals.1[i],vals.2[i]))
  tmp2=ddply(tmp,c("MEI.yr",'Site',"variable"),summarise,
             mean.val=mean(value,na.rm=T),
             sd.val=sd(value,na.rm=T),
             N.val=N.obs(value))
  tmp2$month.period=month.period[i]
  wq.dat.melt.bimonth=rbind(tmp2,wq.dat.melt.bimonth)
}
wq.dat.melt.bimonth$month.period=factor(wq.dat.melt.bimonth$month.period,level=month.period)

dcast(subset(wq.dat.melt.bimonth,variable=="TP.ugL"),MEI.yr~month.period,value.var = "mean.val",mean)

wq.dat.melt.bimonth=merge(wq.dat.melt.bimonth,ENSO.dat[,c('Year',"Month","MEI","month.num")],by.x=c("MEI.yr","month.period"),by.y=c("Year","Month"))
head(wq.dat.melt.bimonth)

subset(wq.dat.melt.bimonth,variable=="TP.ugL"&mean.val>200)
subset(wq.dat.melt.bimonth,N.val==1)

wq.dat.melt.bimonth$mean.val=with(wq.dat.melt.bimonth,ifelse(N.val==1,NA,mean.val))

plot(mean.val~MEI,subset(wq.dat.melt.bimonth,variable=="TP.ugL"&Site%in%sites.vals[1]),log="y")
plot(mean.val~MEI,subset(wq.dat.melt.bimonth,variable=="TP.ugL"&month.period%in%c("SO","ON",'ND')&Site%in%sites.vals[1]),log="y")

plot(mean.val~MEI,subset(wq.dat.melt.bimonth,variable=="TN.mgL"&Site%in%sites.vals[1]))
plot(mean.val~MEI,subset(wq.dat.melt.bimonth,variable=="TN.mgL"&month.period%in%c("ON",'ND')&Site%in%sites.vals[2]))


ddply(subset(wq.dat.melt.bimonth,Site%in%sites.vals&variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")),
      c("Site","variable"),summarise,
      r.val=cor.test(mean.val,MEI,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,MEI,method="spearman")$p.value,3))


### AMO -------------------------------------------------------------------
head(wq.dat.melt);str(wq.dat.melt)
head(AMO.dat.melt);str(AMO.dat.melt)
AMO.dat.melt=rename(AMO.dat.melt,c("value"="AMO"))

wq.dat.month=ddply(tmp,c('Site',"month","CY","winter","variable"),summarise,
                   mean.val=mean(value,na.rm=T),
                   sd.val=sd(value,na.rm=T),
                   N.val=N.obs(value))

wq.dat.melt2=merge(wq.dat.melt,AMO.dat.melt[,c('month',"year","AMO")],
                   by.x=c("month","CY"),by.y=c('month',"year"))

plot(value~AMO,subset(wq.dat.melt2,variable=="TN.mgL"&Site%in%sites.vals[1]))

ddply(subset(wq.dat.melt2,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&
               winter==1),
      c("Site","variable"),summarise,
      r.val=cor.test(value,AMO,method="spearman")$estimate,
      p.val=round(cor.test(value,AMO,method="spearman")$p.value,3))

ddply(subset(wq.dat.melt2,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")),
      c("Site","variable"),summarise,
      r.val=cor.test(value,AMO,method="spearman")$estimate,
      p.val=round(cor.test(value,AMO,method="spearman")$p.value,3))

winter.amo.wq=ddply(subset(wq.dat.melt2,
                           Site%in%sites.vals&
                             variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
                    c("Site","variable"),summarise,
                    r.val=cor.test(value,AMO,method="spearman")$estimate,
                    p.val=round(cor.test(value,AMO,method="spearman")$p.value,3))

### PDO -------------------------------------------------------------------
head(wq.dat.melt);str(wq.dat.melt)
head(pdo);str(pdo)

pdo=rename(pdo,c("value"="PDO"))

wq.dat.melt3=merge(wq.dat.melt,pdo[,c('month.num',"Year","PDO")],
                   by.x=c("month","CY"),by.y=c('month.num',"Year"))

plot(value~PDO,subset(wq.dat.melt3,variable=="TN.mgL"&Site%in%sites.vals[1]&winter==0))
plot(value~PDO,subset(wq.dat.melt3,variable=="TN.mgL"&Site%in%sites.vals[2]&winter==1))
winter.pdo.wq=ddply(subset(wq.dat.melt3,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,
      r.val=cor.test(value,PDO,method="spearman")$estimate,
      p.val=round(cor.test(value,PDO,method="spearman")$p.value,3))
# write.csv(winter.pdo.wq,paste0(export.path,"winter_pdo_cor.csv"),row.names = F)

winter.pdo.wq%>%
  flextable()%>%
  colformat_double(j=3:4,digits=2,na_str = "---")%>%
  compose(j="variable",i=~variable=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('TN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DIN.mgL",value=as_paragraph('DIN (mg L\u207B\u00B9)'))%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  merge_v(j="Site")%>%
  compose(j="p.val",i=~p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="p.val",i=~p.val<0.01,value=as_paragraph('< 0.01'))%>%
  set_header_labels("Site"="Site",
                    "variable" = "Parameter",
                    "r.val" = "r",
                    "p.val"="\u03C1-value")%>%
  width(width=c(1,1,0.75,0.75))%>%
  #align(align="center",part="header")%>%
  align(j=1:2,align="left",part="all")%>%
  valign(j=1,valign="top")%>%
  align(j=3:4,align="center",part="all")%>%
  hline(i=4)%>%
  padding(padding=1.5,part="all")%>%
  fix_border_issues()%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")# %>%print("docx")

cols=adjustcolor(wesanderson::wes_palette("Zissou1",2,"continuous"),0.5)
# png(filename=paste0(plot.path,"PLSF_PDOWQ_winter.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:8,4,2,byrow = T),widths=c(1,1))
ylab.ln=2.5
xlim.val=c(-2.5,2);by.x=1;xmaj=seq(min(c(-3,xlim.val[1])),xlim.val[2],by.x);xmin=seq(min(c(-3,xlim.val[1])),xlim.val[2],by.x/2)

ylim.val=c(10,2000);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
tmp.dat=subset(wq.dat.melt3,variable=="TP.ugL"&Site%in%sites.vals[i]&winter==1)
plot(value~PDO,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(value~PDO,tmp.dat,pch=21,bg=cols[i],lwd=0.1,col=adjustcolor("black",0.5))
mod=mblm::mblm(value~PDO,tmp.dat,repeated = F)
x.val=with(tmp.dat,seq(min(PDO,na.rm=T),max(PDO,na.rm=T),length.out=20))
mod.pred=predict(mod,data.frame(PDO=x.val),interval="confidence")
lines(x.val,mod.pred[,1]);lines(x.val,mod.pred[,2],lty=2);lines(x.val,mod.pred[,3],lty=2)
axis_fun(1,xmaj,xmin,NA)
if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
if(i==1){mtext(side=2,line=ylab.ln,"TP (\u03BCg L\u207B\u00B9)")}
}

ylim.val=c(1,100);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  tmp.dat=subset(wq.dat.melt3,variable=="SRP.ugL"&Site%in%sites.vals[i]&winter==1)
  plot(value~PDO,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  points(value~PDO,tmp.dat,pch=21,bg=cols[i],lwd=0.1,col=adjustcolor("black",0.5))
  mod=mblm::mblm(value~PDO,tmp.dat,repeated = F)
  x.val=with(tmp.dat,seq(min(PDO,na.rm=T),max(PDO,na.rm=T),length.out=20))
  mod.pred=predict(mod,data.frame(PDO=x.val),interval="confidence")
  lines(x.val,mod.pred[,1]);lines(x.val,mod.pred[,2],lty=2);lines(x.val,mod.pred[,3],lty=2)
  axis_fun(1,xmaj,xmin,NA)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==1){mtext(side=2,line=ylab.ln,"SRP (\u03BCg L\u207B\u00B9)")}
}

ylim.val=c(0.5,3);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  tmp.dat=subset(wq.dat.melt3,variable=="TN.mgL"&Site%in%sites.vals[i]&winter==1)
  plot(value~PDO,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  points(value~PDO,tmp.dat,pch=21,bg=cols[i],lwd=0.1,col=adjustcolor("black",0.5))
  mod=mblm::mblm(value~PDO,tmp.dat,repeated = F)
  x.val=with(tmp.dat,seq(min(PDO,na.rm=T),max(PDO,na.rm=T),length.out=20))
  mod.pred=predict(mod,data.frame(PDO=x.val),interval="confidence")
  lines(x.val,mod.pred[,1]);lines(x.val,mod.pred[,2],lty=2);lines(x.val,mod.pred[,3],lty=2)
  axis_fun(1,xmaj,xmin,NA)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==1){mtext(side=2,line=ylab.ln,"TN (mg L\u207B\u00B9)")}
}

ylim.val=c(0.3,3);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  tmp.dat=subset(wq.dat.melt3,variable=="DIN.mgL"&Site%in%sites.vals[i]&winter==1)
  plot(value~PDO,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  points(value~PDO,tmp.dat,pch=21,bg=cols[i],lwd=0.1,col=adjustcolor("black",0.5))
  mod=mblm::mblm(value~PDO,tmp.dat,repeated = F)
  x.val=with(tmp.dat,seq(min(PDO,na.rm=T),max(PDO,na.rm=T),length.out=20))
  mod.pred=predict(mod,data.frame(PDO=x.val),interval="confidence")
  lines(x.val,mod.pred[,1]);lines(x.val,mod.pred[,2],lty=2);lines(x.val,mod.pred[,3],lty=2)
  axis_fun(1,xmaj,xmin,xmaj)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==1){mtext(side=2,line=ylab.ln,"DIN (mg L\u207B\u00B9)")}
}
mtext(side=1,outer=T,line=1,"Winter PDO Index")
dev.off()

# wq.dat.month2=merge(wq.dat.month,pdo[,c('month.num',"Year","PDO")],
#                     by.x=c("month","CY"),by.y=c('month.num',"Year"))
# 
# plot(mean.val~PDO,subset(wq.dat.month2,variable=="TN.mgL"&Site%in%sites.vals[1]))
# plot(mean.val~PDO,subset(wq.dat.month2,variable=="TP.ugL"&Site%in%sites.vals[1]))
# 
# ddply(subset(wq.dat.month2,
#              Site%in%sites.vals&
#                variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")),
#       c("Site","variable"),summarise,
#       r.val=cor.test(mean.val,PDO,method="spearman")$estimate,
#       p.val=round(cor.test(mean.val,PDO,method="spearman")$p.value,3))

## attempt to detrend data...
month.wq=dcast(wq.dat.melt,Site+CY+month~variable,value.var = "value",mean,na.rm=T)
month.wq$monCY.date=with(month.wq,date.fun(paste(CY,month,"01",sep="-")))
month.wq$dec.yr=lubridate::decimal_date(month.wq$monCY.date)

tmp=subset(month.wq,Site=="Lake_Outlet")
month.trend.NOX.out=mblm(NOx.mgL~dec.yr,subset(tmp,is.na(NOx.mgL)==F))
# month.trend.NOX.out=lm(NOx.mgL~dec.yr,subset(tmp,is.na(NOx.mgL)==F))

tmp$NOx.pred=predict(month.trend.NOX.out,tmp)
tmp$NOx.detrend=with(tmp,NOx.mgL-NOx.pred)
tmp$NOx.detrend2=pracma::detrend(tmp$NOx.mgL)

plot(NOx.mgL~dec.yr,tmp,type="b")
plot(NOx.detrend~dec.yr,tmp,type="b")
plot(NOx.detrend2~dec.yr,tmp,type="b")

plot(NOx.detrend2~NOx.detrend,tmp);abline(0,1)


### SOI -------------------------------------------------------------------
head(wq.dat.melt);str(wq.dat.melt)
head(soi);str(soi)

soi=rename(soi,c("value"="SOI"))

wq.dat.melt4=merge(wq.dat.melt,soi[,c('month.num',"YEAR","SOI")],
                   by.x=c("month","CY"),by.y=c('month.num',"YEAR"))


plot(value~SOI,subset(wq.dat.melt4,variable=="TN.mgL"&Site%in%sites.vals[1]&winter==0))
plot(value~SOI,subset(wq.dat.melt4,variable=="TN.mgL"&Site%in%sites.vals[2]&winter==1))
winter.soi.wq=ddply(subset(wq.dat.melt4,
                           Site%in%sites.vals&
                             variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
                    c("Site","variable"),summarise,
                    r.val=cor.test(value,SOI,method="spearman")$estimate,
                    p.val=round(cor.test(value,SOI,method="spearman")$p.value,3))


### NAO -------------------------------------------------------------------
nao.dat=rename(nao.dat,c("value"="NAO"))

wq.dat.melt5=merge(wq.dat.melt,nao.dat[,c('month.num',"Year","NAO")],
                   by.x=c("month","CY"),by.y=c('month.num',"Year"))

plot(value~NAO,subset(wq.dat.melt5,variable=="TN.mgL"&Site%in%sites.vals[1]&winter==0))
plot(value~NAO,subset(wq.dat.melt5,variable=="TN.mgL"&Site%in%sites.vals[2]&winter==1))
winter.nao.wq=ddply(subset(wq.dat.melt5,
                           Site%in%sites.vals&
                             variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
                    c("Site","variable"),summarise,
                    r.val=cor.test(value,NAO,method="spearman")$estimate,
                    p.val=round(cor.test(value,NAO,method="spearman")$p.value,3))
winter.nao.wq
tmp=ddply(subset(wq.dat.melt5,winter==1),c("WY","variable"),summarise,
          mean.val=mean(value,na.rm=T),
          mean.noa=mean(NAO,na.rm=T))

# teleconnections ---------------------------------------------------------
wq.dat.melt6=merge(wq.dat.melt,AMO.dat.melt[,c('month',"year","AMO")],
                   by.x=c("month","CY"),by.y=c('month',"year"))
wq.dat.melt6=merge(wq.dat.melt6,nao.dat[,c('month.num',"Year","NAO")],
                   by.x=c("month","CY"),by.y=c('month.num',"Year"))
wq.dat.melt6=merge(wq.dat.melt6,pdo[,c('month.num',"Year","PDO")],
                   by.x=c("month","CY"),by.y=c('month.num',"Year"))
wq.dat.melt6=merge(wq.dat.melt6,soi[,c('month.num',"YEAR","SOI")],
                   by.x=c("month","CY"),by.y=c('month.num',"YEAR"))
wq.dat.melt6=merge(wq.dat.melt6,oni[,c('month.num',"Year","ONI")],
                   by.x=c("month","CY"),by.y=c('month.num',"Year"))
wq.dat.melt6=merge(wq.dat.melt6,ENSO.dat[,c('month.num',"Year","MEI")],
                   by.x=c("month","CY"),by.y=c('month.num',"Year"))

subset(wq.dat.melt6,variable=="TP.ugL"&value>1000)

tmp=ddply(wq.dat.melt6,c("Site","Date","WY","variable","month","winter",'ice.period'),summarise,
          mean.val=mean(value,na.rm=T),N.val=N.obs(value),
          mean.amo=mean(AMO,na.rm=T),
          mean.nao=mean(NAO,na.rm=T),
          mean.pdo=mean(PDO,na.rm=T),
          mean.soi=mean(SOI,na.rm=T),
          mean.oni=mean(ONI,na.rm=T),
          mean.enso=mean(MEI,na.rm=T))

tmp2=ddply(wq.dat.melt6,c("Site","WY","month","variable","winter"),summarise,
          mean.val=mean(value,na.rm=T),N.val=N.obs(value),
          mean.amo=mean(AMO,na.rm=T),
          mean.nao=mean(NAO,na.rm=T),
          mean.pdo=mean(PDO,na.rm=T),
          mean.soi=mean(SOI,na.rm=T),
          mean.oni=mean(ONI,na.rm=T),
          mean.enso=mean(MEI,na.rm=T))

with(subset(tmp,Site==sites.vals[1]&winter==1&WY!=2011),
     cor.test(mean.val,mean.enso,method="spearman"))
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1&mean.val<2000),
      c("Site","variable"),summarise,Index="ENSO",
      r.val=cor.test(mean.val,mean.enso,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.enso,method="spearman")$p.value,3))


## Synchrony  -------------------------------------------------------------
library(synchrony)

test.sync=with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[1]),phase.sync(mean.val,mean.pdo))
# Distribution of phase difference
hist(test.sync$deltaphase$mod_phase_diff_2pi)

# Compute concordant peaks
p=with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[1]&winter==1),peaks(mean.val, mean.pdo, nrands=100))
# Find proportion of time steps where both time series peak together
p$peaks
# Plot (null) distribution of proportion of time steps where both time
# series peak together
hist(p$rand)
# p-value of observed value
p$pval

var.vals1=c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")
clim.ind=paste("mean",c("amo","nao","pdo","soi","oni","enso"),sep=".")
sync.rslt=data.frame()
for(i in 1:length(sites.vals)){
  tmp.dat=subset(tmp2,Site==sites.vals[i]&winter==1)
  for(j in 1:length(var.vals1)){
    for(k in 1:length(clim.ind)){
    p.concord=peaks(tmp.dat[tmp.dat$variable==var.vals1[j],"mean.val"],
                    tmp.dat[tmp.dat$variable==var.vals1[j],clim.ind[k]], nrands=100,type=1,quiet=T)
    rslt=data.frame(Site=sites.vals[i],
                    variable=var.vals1[j],
                    climate.index=clim.ind[k],
                    obs=p.concord$obs,
                    p.val=p.concord$pval)
    sync.rslt=rbind(rslt,sync.rslt)
    
    
    }
  }
}
sync.rslt
subset(sync.rslt,p.val<0.05)

plot(mean.val~mean.nao,subset(tmp2,Site==sites.vals[2]&variable=="SRP.ugL"))
plot(mean.val~mean.nao,subset(tmp2,Site==sites.vals[2]&variable=="DIN.mgL"))

kendall.w(subset(tmp2,Site==sites.vals[2]&variable=="DIN.mgL")[,c("mean.val",clim.ind)],nrand=100)

w.val=kendall.w(subset(tmp2,Site==sites.vals[2]&variable=="TN.mgL")[,c("mean.val",clim.ind)],nrand=100)



# time lagged cross correlation
## https://online.stat.psu.edu/stat510/lesson/8/8.2
with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2]),ccf(mean.amo,mean.val))
with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[1]),ccf(mean.nao,mean.val))
with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2]),ccf(mean.pdo,mean.val))
with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2]),ccf(mean.soi,mean.val))
with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2]),ccf(mean.oni,mean.val))
with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2]),ccf(mean.enso,mean.val))


##

tele.wq=tmp
tele.wq.winter=rbind(
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,Index="AMO",
      r.val=cor.test(mean.val,mean.amo,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.amo,method="spearman")$p.value,3)),
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,Index="NAO",
      r.val=cor.test(mean.val,mean.nao,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.nao,method="spearman")$p.value,3)),
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,Index="PDO",
      r.val=cor.test(mean.val,mean.pdo,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.pdo,method="spearman")$p.value,3)),
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,Index="SOI",
      r.val=cor.test(mean.val,mean.soi,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.soi,method="spearman")$p.value,3)),
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,Index="ONI",
      r.val=cor.test(mean.val,mean.oni,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.oni,method="spearman")$p.value,3)),
ddply(subset(tmp,
             Site%in%sites.vals&
               variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")&winter==1),
      c("Site","variable"),summarise,Index="ENSO",
      r.val=cor.test(mean.val,mean.enso,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,mean.enso,method="spearman")$p.value,3))
)

rval.bks=seq(-1,1,0.2)
cols=colorRampPalette(c("blue","grey","red"))(length(rval.bks))
var.plot=data.frame(variable=rev(c("TP.ugL", "SRP.ugL","TN.mgL","DIN.mgL")),
                    varplot=c(1:4))
varlab=rev(c("TP","SRP","TN","DIN"))

tele.wq.winter=merge(tele.wq.winter,var.plot,"variable")
tele.wq.winter$variable=factor(tele.wq.winter$variable,levels=var.plot$variable)
# tele.wq.winter=tele.wq.winter[order(tele.wq.winter$varplot),]
tele.wq.winter$col.vals=findInterval(tele.wq.winter$r.val,rval.bks)

ylim.val=c(0.5,4.5);ymaj=1:4
xlim.val=c(0.1,1.3);xmaj=seq(0.2,1.2,0.2)
pos1=data.frame(index=c("AMO","NAO","PDO","SOI","ONI","ENSO"),
                x.val=c(0.2,0.4,0.6,0.8,1,1.2))

# png(filename=paste0(plot.path,"PLSF_winter_teleconnect.png"),width=5.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,2,1.5,0.25));
layout(matrix(c(1,2,3,3),2,2,byrow = F),widths=c(1,0.5))

for(j in 1:2){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,type="n",yaxs="i",xaxs="i",ann=F,axes=F)
  abline(h=seq(0.5,4.5,1),v=seq(0.1,1.1,0.2),col="grey")
for(i in 1:6){
  tmp.dat1=subset(tele.wq.winter,Site==sites.vals[j]&Index==pos1$index[i])
  with(tmp.dat1,
       points(rep(pos1$x.val[i],length(variable)),varplot,
                  pch=21,bg=ifelse(p.val>0.05,NA,cols[col.vals]),
                  col=ifelse(p.val<0.05,"grey",cols[col.vals]),
                  lwd=ifelse(p.val<0.05,0.05,3),cex=5))
}
  axis_fun(2,ymaj,ymaj,varlab)
  if(j==1){axis_fun(3,xmaj,xmaj,c("AMO","NAO","PDO","SOI","ONI","MEI"),line=-0.75,maj.tcl=0,min.tcl=0)}
  box(lwd=1)
  mtext(side=4,las=3,c("Lake Inlet",'Lake Outlet')[j])
}
mtext(side=2,line=0.5,"Variable",outer=T)
mtext(side=1,line=1,"Climate Index")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(rval.bks,cols,"Correlation\nr-value",
        x.max=0.6,x.min=0.2,
        leg.type="continuous",leg.txt=c("1.0","0.0","-1.0"),txt.y=c(0.8,0.5,0.2))
legend(0.5,0.2,legend=c("\u03C1-value < 0.05","\u03C1-value > 0.05"),
       pch=c(21),pt.bg=c("grey",NA),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01,2),col=c("black","grey"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)
dev.off()


tele.wq.winter[order(tele.wq.winter$Site,-tele.wq.winter$varplot,tele.wq.winter$Index),]
range(tele.wq.winter$r.val)
range(subset(tele.wq.winter,p.val<=0.05)$r.val)

tmp.amo=subset(tele.wq.winter,Index=='AMO')
tmp.amo=tmp.amo[order(tmp.amo$Site,-tmp.amo$varplot),c('Site',"variable","r.val","p.val")]
colnames(tmp.amo)=c('Site',"variable","AMO.r.val","AMO.p.val")

tmp.nao=subset(tele.wq.winter,Index=='NAO')
tmp.nao=tmp.nao[order(tmp.nao$Site,-tmp.nao$varplot),c('Site',"variable","r.val","p.val")]
colnames(tmp.nao)=c('Site',"variable","NAO.r.val","NAO.p.val")

tmp.pdo=subset(tele.wq.winter,Index=='PDO')
tmp.pdo=tmp.pdo[order(tmp.pdo$Site,-tmp.pdo$varplot),c('Site',"variable","r.val","p.val")]
colnames(tmp.pdo)=c('Site',"variable","PDO.r.val","PDO.p.val")

tmp.soi=subset(tele.wq.winter,Index=='SOI')
tmp.soi=tmp.soi[order(tmp.soi$Site,-tmp.soi$varplot),c('Site',"variable","r.val","p.val")]
colnames(tmp.soi)=c('Site',"variable","SOI.r.val","SOI.p.val")

tmp.oni=subset(tele.wq.winter,Index=='ONI')
tmp.oni=tmp.oni[order(tmp.oni$Site,-tmp.oni$varplot),c('Site',"variable","r.val","p.val")]
colnames(tmp.oni)=c('Site',"variable","ONI.r.val","ONI.p.val")

tmp.enso=subset(tele.wq.winter,Index=='ENSO')
tmp.enso=tmp.enso[order(tmp.enso$Site,-tmp.enso$varplot),c('Site',"variable","r.val","p.val")]
colnames(tmp.enso)=c('Site',"variable","ENSO.r.val","ENSO.p.val")


cbind(
  tmp.amo,
  tmp.nao[,c("NAO.r.val","NAO.p.val")],
  tmp.pdo[,c("PDO.r.val","PDO.p.val")],
  tmp.soi[,c("SOI.r.val","SOI.p.val")],
  tmp.oni[,c("ONI.r.val","ONI.p.val")],
  tmp.enso[,c("ENSO.r.val","ENSO.p.val")]
)%>%
  flextable()%>%
  colformat_double(j=3:14,digits=2,na_str = "---")%>%
  compose(j="AMO.p.val",i=~AMO.p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="AMO.p.val",i=~AMO.p.val<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="AMO.p.val",i=~AMO.p.val<0.05)%>%
  compose(j="NAO.p.val",i=~NAO.p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="NAO.p.val",i=~NAO.p.val<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="NAO.p.val",i=~NAO.p.val<0.05)%>%
  compose(j="PDO.p.val",i=~PDO.p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="PDO.p.val",i=~PDO.p.val<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="PDO.p.val",i=~PDO.p.val<0.05)%>%
  compose(j="SOI.p.val",i=~SOI.p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="SOI.p.val",i=~SOI.p.val<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="SOI.p.val",i=~SOI.p.val<0.05)%>%
  compose(j="ONI.p.val",i=~ONI.p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="ONI.p.val",i=~ONI.p.val<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="ONI.p.val",i=~ONI.p.val<0.05)%>%
  compose(j="ENSO.p.val",i=~ENSO.p.val<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="ENSO.p.val",i=~ENSO.p.val<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="ENSO.p.val",i=~ENSO.p.val<0.05)%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  merge_v(j="Site")%>%
  compose(j="variable",i=~variable=="TP.ugL",value=as_paragraph('TP'))%>%
  compose(j="variable",i=~variable=="SRP.ugL",value=as_paragraph('SRP'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('TN'))%>%
  compose(j="variable",i=~variable=="DIN.mgL",value=as_paragraph('DIN'))%>%
  set_header_labels("variable"="Parameter",
                    "Site"="Site",
                    "AMO.r.val" = "r-value",
                    "AMO.p.val" = "\u03C1-value",
                    "NAO.r.val" = "r-value",
                    "NAO.p.val" = "\u03C1-value",
                    "PDO.r.val" = "r-value",
                    "PDO.p.val" = "\u03C1-value",
                    "SOI.r.val" = "r-value",
                    "SOI.p.val" = "\u03C1-value",
                    "ONI.r.val" = "r-value",
                    "ONI.p.val" = "\u03C1-value",
                    "ENSO.r.val" = "r-value",
                    "ENSO.p.val" = "\u03C1-value")%>%
  add_header("AMO.r.val" = "AMO",
             "AMO.p.val" = "AMO",
             "NAO.r.val" = "NAO",
             "NAO.p.val" = "NAO",
             "PDO.r.val" = "PDO",
             "PDO.p.val" = "PDO",
             "SOI.r.val" = "SOI",
             "SOI.p.val" = "SOI",
             "ONI.r.val" = "ONI",
             "ONI.p.val" = "ONI",
             "ENSO.r.val" = "MEI",
             "ENSO.p.val" = "MEI")%>%
  merge_h(part="header")%>%align(align="center",part="header")%>%
  width(width=c(0.5,0.8,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75))%>%
  align(align="center",part="header")%>%
  align(j=2:14,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  hline(i=4)%>%
  vline(j=c(2,4,6,8,10,12,14),border=officer::fp_border(color="grey"))%>%
  fix_border_issues()%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=11,part="all")%>%
  fontsize(size=12,part="header")# %>%print("docx")
  

tele.wq.month=ddply(wq.dat.melt6,c("Site","WY","variable","month","winter","ice.period"),summarise,
          mean.val=mean(value,na.rm=T),N.val=N.obs(value),
          SE.val=SE(value),
          mean.amo=mean(AMO,na.rm=T),
          mean.nao=mean(NAO,na.rm=T),
          mean.pdo=mean(PDO,na.rm=T),
          mean.soi=mean(SOI,na.rm=T),
          mean.oni=mean(ONI,na.rm=T),
          mean.enso=mean(MEI,na.rm=T))


# png(filename=paste0(plot.path,"PLSF_monthlyMean_WQClimate.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,1.5,0.5,1.5),oma=c(3,3,0.25,0.1));
layout(matrix(1:24,4,6,byrow = F))
var.param=c("TP.ugL","SRP.ugL","TN.mgL","DIN.mgL")
var.lab=paste0(c('TP (\u03BC','SRP (\u03BC',"TN (m","DIN (m"),'g L\u207B\u00B9)')
ylim.val.min=c(5,1,0,0)
ylim.val.max=c(1000,100,3,2)
by.y.val=c(NA,NA,1,1)
xlim.val=c(-0.1,0.4);by.x=0.2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(j in 1:4){
if(var.param[j]%in%var.param[1:2]){
  ylim.val=c(ylim.val.min[j],ylim.val.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
  plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
}else{
  ylim.val=c(ylim.val.min[j],ylim.val.max[j]);by.y=by.y.val[j]
  ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
}
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(tele.wq.month,Site==sites.vals[1]&winter==1&variable==var.param[j]),
     pt_error(mean.amo,mean.val,SE.val,21,"indianred1","black",1,length=0.01,pt.lwd=0.01))
with(subset(tele.wq.month,Site==sites.vals[2]&winter==1&variable==var.param[j]),
     pt_error(mean.amo,mean.val,SE.val,21,"olivedrab2","black",1,length=0.01,pt.lwd=0.01))
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
if(j==4){mtext(side=1,line=2,"AMO")}
mtext(side=2,line=3,var.lab[j])
if(j==1){
  legend("topright",legend=c("Lake Inlet",'Lake Outlet'),
         pch=c(21),pt.bg=c("indianred1","olivedrab2"),pt.cex=c(1),
         lty=c(NA),lwd=c(0.01),col=c("black"),
         ncol=1,cex=0.5,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
}
}
xlim.val=c(-3.5,3);by.x=1.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(j in 1:4){
  if(var.param[j]%in%var.param[1:2]){
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  }else{
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);by.y=by.y.val[j]
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  }
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(tele.wq.month,Site==sites.vals[1]&winter==1&variable==var.param[j]),
       pt_error(mean.nao,mean.val,SE.val,21,"indianred1","black",1,length=0.01,pt.lwd=0.01))
  with(subset(tele.wq.month,Site==sites.vals[2]&winter==1&variable==var.param[j]),
       pt_error(mean.nao,mean.val,SE.val,21,"olivedrab2","black",1,length=0.01,pt.lwd=0.01))
  axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(j==4){mtext(side=1,line=2,"NAO")}
}
xlim.val=c(-2,2);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(j in 1:4){
  if(var.param[j]%in%var.param[1:2]){
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  }else{
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);by.y=by.y.val[j]
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  }
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(tele.wq.month,Site==sites.vals[1]&winter==1&variable==var.param[j]),
       pt_error(mean.pdo,mean.val,SE.val,21,"indianred1","black",1,length=0.01,pt.lwd=0.01))
  with(subset(tele.wq.month,Site==sites.vals[2]&winter==1&variable==var.param[j]),
       pt_error(mean.pdo,mean.val,SE.val,21,"olivedrab2","black",1,length=0.01,pt.lwd=0.01))
  axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(j==4){mtext(side=1,line=2,"PDO")}
}
xlim.val=c(-3,3);by.x=1.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(j in 1:4){
  if(var.param[j]%in%var.param[1:2]){
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  }else{
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);by.y=by.y.val[j]
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  }
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(tele.wq.month,Site==sites.vals[1]&winter==1&variable==var.param[j]),
       pt_error(mean.soi,mean.val,SE.val,21,"indianred1","black",1,length=0.01,pt.lwd=0.01))
  with(subset(tele.wq.month,Site==sites.vals[2]&winter==1&variable==var.param[j]),
       pt_error(mean.soi,mean.val,SE.val,21,"olivedrab2","black",1,length=0.01,pt.lwd=0.01))
  axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(j==4){mtext(side=1,line=2,"SOI")}
}
xlim.val=c(-2,3);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(j in 1:4){
  if(var.param[j]%in%var.param[1:2]){
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  }else{
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);by.y=by.y.val[j]
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  }
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(tele.wq.month,Site==sites.vals[1]&winter==1&variable==var.param[j]),
       pt_error(mean.oni,mean.val,SE.val,21,"indianred1","black",1,length=0.01,pt.lwd=0.01))
  with(subset(tele.wq.month,Site==sites.vals[2]&winter==1&variable==var.param[j]),
       pt_error(mean.oni,mean.val,SE.val,21,"olivedrab2","black",1,length=0.01,pt.lwd=0.01))
  axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(j==4){mtext(side=1,line=2,"ONI")}
}
xlim.val=c(-2.5,2.5);by.x=2.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(j in 1:4){
  if(var.param[j]%in%var.param[1:2]){
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",log="y")
  }else{
    ylim.val=c(ylim.val.min[j],ylim.val.max[j]);by.y=by.y.val[j]
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(mean.val~mean.amo,tele.wq.month,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  }
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(tele.wq.month,Site==sites.vals[1]&winter==1&variable==var.param[j]),
       pt_error(mean.enso,mean.val,SE.val,21,"indianred1","black",1,length=0.01,pt.lwd=0.01))
  with(subset(tele.wq.month,Site==sites.vals[2]&winter==1&variable==var.param[j]),
       pt_error(mean.enso,mean.val,SE.val,21,"olivedrab2","black",1,length=0.01,pt.lwd=0.01))
  axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(j==4){mtext(side=1,line=2,"MEI")}
}
dev.off()
# Ice period comparison ---------------------------------------------
## median ice/no ice with KW test stats for inflow and outflow
wq.dat.melt$ice.sea=with(wq.dat.melt,ifelse(month%in%c(12,1:4),"Ice",'NoIce'))
wq.dat.melt$ice.sea=factor(wq.dat.melt$ice.sea,levels=c("NoIce","Ice"))

sites.vals=c("Godbout", "Lake_Outlet") #, "In_Lake")
dcast(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars),variable+Site~ice.sea,value.var = "value",median,na.rm=T)
ddply(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars),
      c("variable","Site"),summarise,
      stat=kruskal.test(value~ice.sea)$statistic,
      pval=kruskal.test(value~ice.sea)$p.value)

# png(filename=paste0(plot.path,"PLSF_SeaComp_P.png"),width=6.5,height=8.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,3.5,1.5,0.25));
layout(matrix(1:12,6,2,byrow = T),widths=c(1,1))

xaxs.cex=1
ylab.ln=3
ylab.cex=0.8
# ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val=c(1,700);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="TP.ugL"
  axes.lab="TP (\u03BCg L\u207B\u00B9)"
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=ylab.ln,axes.lab,ylab.cex)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.2,300);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="SRP.ugL"
  axes.lab="SRP (\u03BCg L\u207B\u00B9)"
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=ylab.ln,axes.lab,ylab.cex)}
  # mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.1,10);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="TN.mgL"
  axes.lab="TN (mg L\u207B\u00B9)"
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=ylab.ln,axes.lab,ylab.cex)}
  # mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.001,5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="DIN.mgL"
  axes.lab="DIN (mg L\u207B\u00B9)"
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=ylab.ln,axes.lab,ylab.cex)}
  # mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.001,1);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="Phyco.ugL"
  axes.lab=expression(paste("Phyco"[italic(" in-vivo")]," (",mu,"g L"^"-1",")")) #(\u03BCg L\u207B\u00B9)"
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=ylab.ln,axes.lab,ylab.cex)}
  # mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.1,30);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="TChl.ugL"
  axes.lab=expression(paste("Chl"[italic(" in-vivo")]," (",mu,"g L"^"-1",")")) #(\u03BCg L\u207B\u00B9)"
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=ylab.ln,axes.lab,ylab.cex)}
  # mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1,cex=xaxs.cex)
}

mtext(side=1,line=2,outer=T,"Seasons")
dev.off()


wq.dat.melt2=merge(wq.dat.melt,ice_period[,c('Date',"ice","Ice_cum")],"Date",all.x=T )
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Godbout'&variable=="NOx.mgL"&Ice_cum>0))
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Godbout'&variable=="TN.mgL"&Ice_cum>0))
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Godbout'&variable=="TP.ugL"&Ice_cum>0),log="y")

plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="NOx.mgL"&Ice_cum>0))
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="NH4.mgL"&Ice_cum>0))
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0))
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0))

ddply(subset(wq.dat.melt2,Site%in%sites.vals&variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")),
      c("Site","variable"),summarise,
      r.val=cor.test(value,Ice_cum,method="spearman")$estimate,
      p.val=round(cor.test(value,Ice_cum,method="spearman")$p.value,3))

plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0),log="y")

## Segmented regession
library(segmented)

mod.TN.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0))
# gvlma::gvlma(mod.TN.outlet)
seg.mod.TN.outlet=segmented(mod.TN.outlet,~Ice_cum)
summary(seg.mod.TN.outlet)
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0))
plot(seg.mod.TN.outlet,add=T)

mod.DIN.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0))
# gvlma::gvlma(mod.DIN.outlet)
seg.mod.DIN.outlet=segmented(mod.DIN.outlet,~Ice_cum)
summary(seg.mod.DIN.outlet)
plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0))
plot(seg.mod.DIN.outlet,add=T)

## Explore GAM and other evluation
tmp=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0)
library(mgcv)
library(gratia)
gam1=gam(log(value)~
           s(Ice_cum,k=60)+
           s(WY,k=8)+
           ti(Ice_cum,WY,k=c(10,5)),
         data=tmp)
summary(gam1)
gam.check(gam1,pch=21,bg="grey",col="Red")
draw(gam1)

plot(value~Ice_cum,subset(tmp,WY==2015),ylim=c(0,1))
points(value~Ice_cum,subset(tmp,WY==2016),pch=21,bg="red")
## Yr is an important covariate since cumulative ice on varies qualitatively by year

test=ddply(tmp,"WY",summarise,
      slope.val=as.numeric(coefficients(mblm(value~Ice_cum))[2]),
      max.days=max(Ice_cum))

hist(test$slope.val)
plot(slope.val~max.days,test)
with(test,text(max.days,slope.val,WY,pos=3))

# png(filename=paste0(plot.path,"PLSF_DIN_example_yrs.png"),width=5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1),oma=c(2.5,3,1.5,0.25));

xlim.val=c(0,180);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1);by.y=0.25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(value~Ice_cum,tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(value~Ice_cum,subset(tmp,WY==2015),pch=21,bg="dodgerblue1",cex=1.25,lwd=0.1)
points(value~Ice_cum,subset(tmp,WY==2016),pch=21,bg="indianred1",cex=1.25,lwd=0.1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"Days since Ice On")
mtext(side=2,line=3,"DIN (mg L\u207B\u00B9)")
mtext(side=3,adj=1,"Lake Outlet ",font=3)
legend("topright",legend=c(2015,2016),
       pch=c(21),pt.bg=c("dodgerblue1","indianred1"),pt.cex=c(1.5),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
dev.off()

# png(filename=paste0(plot.path,"PLSF_IceOn_DIN.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1),oma=c(2.5,3,1.5,0.25));

xlim.val=c(90,180);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(-0.010,0.005);by.y=0.005;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(slope.val~max.days,test,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(slope.val~max.days,test,pch=21,bg="indianred1",cex=1.5)
with(test,text(max.days,slope.val,WY,pos=4,cex=0.75,offset=0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"Total Days since Ice On")
mtext(side=2,line=3,"DIN Conc. x Cum. Days Since Ice On Slope\n(mg L\u207B\u00B9 D\u207B\u00B9)")
mtext(side=3,adj=1,"Lake Outlet ",font=3)
dev.off()


# rate of change ice cover ------------------------------------------------
## Kincaid et al. 2022. Ice cover and thaw events influence nitrogen 
### partitioning and concentration in two shallow eutrophic lakes.
### Biogeochemistry 157: 15–29. https://doi.org/10.1007/s10533-021-00872-x.

NOX.change.kincaid.fig5=data.frame(
  lake=c(rep("MB",10),rep("SP",5)),
  yr=c(rep(2014,5),rep(2015,10)),
  depth=rep(c("top","mid1","mid2","mid3","bottom"),3),
  NO3.slope.uMd=c(NA,NA,-0.19,-0.19,NA,
              NA,-0.34,-0.45,-0.80,-0.73,
              NA,NA,-0.09,-0.07,-0.03)
)
NOX.change.kincaid.fig5$NO3.slope.ugLd=NOX.change.kincaid.fig5$NO3.slope.uMd*N.mw
range(NOX.change.kincaid.fig5$NO3.slope.ugLd,na.rm=T)

## 
link="https://pasta.lternet.edu/package/data/eml/edi/1020/1/9f13f287e0fd857ef902c2c5a6d7337c"
kincaid.dat=read.csv(link)
kincaid.dat$date=as.Date(kincaid.dat$date)
kincaid.dat$CY=as.numeric(format(kincaid.dat$date,"%Y"))

## Fig 5d
z.vars=c('Top',"Mid-1","Mid-2","Mid-3",'Bottom')
par(family="serif",mar=c(1,3.5,0.5,1.5),oma=c(2.5,2,0.25,0.25));
layout(matrix(c(1:4),4,1,byrow = F))
for(i in 1:4){
plot(NO3~yday,subset(kincaid.dat,site=="mb"&CY==2014&samp_depth_cat2==z.vars[i]),ylim=c(0,60))
abline(v=78)
}

kincaid.dat$NO3.ugL=kincaid.dat$NO3*N.mw
kincaid.dat$ice=with(kincaid.dat,ifelse(CY==2014&yday<=78|CY==2015&yday<=70,1,0))

kincaid.ann.slope.ice=ddply(subset(kincaid.dat,ice==1&is.na(NO3.ugL)==F),c("CY","site","samp_depth_cat2"),summarise,
                    slope.val=as.numeric(coefficients(mblm(NO3.ugL~yday))[2]),
                    max.days=max(yday))
range(kincaid.ann.slope.ice$slope.val)

plot(slope.val~max.days,kincaid.ann.slope.ice)


## PLSF

ddply(subset(wq.dat.melt2,Site=='Lake_Outlet'&Ice_cum>0),c("variable"),summarise,
      est=cor.test(value,Ice_cum,method='spearman')$estimate,
      pval=cor.test(value,Ice_cum,method='spearman')$p.value,
      pval2=round(pval,2))

### 
N.vars=c("TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL",'TON.mgL')
wq.dat.melt2$value2=with(wq.dat.melt2,ifelse(variable%in%N.vars,value*1000,value))
ann.slope.ice=ddply(subset(wq.dat.melt2,Site=='Lake_Outlet'&Ice_cum>0),c("WY","variable"),summarise,
      slope.val=as.numeric(coefficients(mblm(value2~Ice_cum))[2]),
      max.days=max(Ice_cum),
      Ci=value2[1])
ddply(ann.slope.ice,"variable",summarise,min.val=min(slope.val),max.val=round(max(slope.val),4))

subset(ann.slope.ice,variable=="NOx.mgL")$slope.val
subset(ann.slope.ice,variable=="NH4.mgL")
subset(ann.slope.ice,variable=="DIN.mgL")
subset(ann.slope.ice,variable=="TON.mgL")
range(subset(ann.slope.ice,variable=="TN.mgL")$slope.val)
range(subset(ann.slope.ice,variable=="DIN.mgL")$slope.val)
ddply(ann.slope.ice,c("variable"),summarise,N.val=N.obs(slope.val),
      est=cor.test(slope.val,max.days,method='spearman')$estimate,
      pval=cor.test(slope.val,max.days,method='spearman')$p.value)

plot(Ci~slope.val,subset(ann.slope.ice,variable=="TP.ugL"),log="y")
plot(Ci~slope.val,subset(ann.slope.ice,variable=="SRP.ugL"),log="y")
plot(Ci~slope.val,subset(ann.slope.ice,variable=="TN.mgL"),log="y")
plot(Ci~slope.val,subset(ann.slope.ice,variable=="DIN.mgL"),log="y")
ddply(ann.slope.ice,c("variable"),summarise,N.val=N.obs(slope.val),
      est=cor.test(slope.val,Ci,method='spearman')$estimate,
      pval=cor.test(slope.val,Ci,method='spearman')$p.value)


test=mblm::mblm(slope.val~max.days,subset(ann.slope.ice,variable=="DIN.mgL"))
(0-coef(test)[1])/coef(test)[2]

plot(slope.val~max.days,subset(ann.slope.ice,variable=="DIN.mgL"))
plot(slope.val~max.days,subset(ann.slope.ice,variable=="NOx.mgL"));# NOx drives relationship, increase with mx days
plot(slope.val~max.days,subset(ann.slope.ice,variable=="NH4.mgL"))

plot(slope.val~max.days,subset(ann.slope.ice,variable=="TN.mgL"))

plot(slope.val~max.days,subset(ann.slope.ice,variable=="SRP.ugL"))
plot(slope.val~max.days,subset(ann.slope.ice,variable=="TP.ugL"))
plot(slope.val~max.days,subset(ann.slope.ice,variable=="DP.ugL"))
plot(slope.val~max.days,subset(ann.slope.ice,variable=="DOP.ugL"))
# with(subset(ann.slope.ice,variable=="DP.ugL"),cor.test(slope.val,max.days,method="spearman"))
plot(slope.val~max.days,subset(ann.slope.ice,variable=="Phyco.ugL"))



# png(filename=paste0(plot.path,"PLSF_RateIceOn_v2.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,1.5),oma=c(2.5,2,0.25,0.25));
layout(matrix(c(1:4),2,2,byrow = F),widths=c(1,1))

xlim.val=c(90,180);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.min=c(-0.6,-0.4,-8,-8)
ylim.max=c(0.6,0.3,0,2)
by.y.val=c(0.3,0.2,2,2)
var=c("TP.ugL","SRP.ugL","TN.mgL","DIN.mgL")
var.lab=c("TP Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)",
          "SRP Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)",
          "TN Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)",
          "DIN Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)")

for(j in 1:4){
  ylim.val=c(ylim.min[j],ylim.max[j]);ymaj=seq(ylim.val[1],ylim.val[2],by.y.val[j]);ymin=seq(ylim.val[1],ylim.val[2],by.y.val[j])
  tmp.dat=subset(ann.slope.ice,variable==var[j])
  plot(slope.val~max.days,tmp.dat,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  abline(h=0)
  points(slope.val~max.days,tmp.dat,
         pch=21,bg="dodgerblue1",cex=1.25,lwd=0.1)
  mod=mblm::mblm(slope.val~max.days,tmp.dat)
  x.val=with(tmp.dat,seq(min(max.days),max(max.days),length.out=50))
  mod.pred=predict(mod,data.frame(max.days=x.val),interval="confidence")
  lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
  lines(x.val,mod.pred[,2],lty=2,col="black")
  lines(x.val,mod.pred[,3],lty=2,col="black")
  
  if(j%in%c(2,4)){axis_fun(1,xmaj,xmin,xmaj,line=-0.5)}else{axis_fun(1,xmaj,xmin,NA)}
  if(j%in%c(2,4)){mtext(side=1,line=1.75,"Total Days Since Ice On")}
  axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1));box(lwd=1)
  mtext(side=2,line=3.25,var.lab[j])
}
dev.off()


# png(filename=paste0(plot.path,"PLSF_RateIceOn_Ci.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.5,3.5,0.5,0.5),oma=c(1,1,0.25,0.25));
layout(matrix(c(1:4),2,2,byrow = F),widths=c(1,1))

xlim.min=c(-0.6,-0.4,-8,-8)
xlim.max=c(0.6,0.3,0,2)
by.x.val=c(0.3,0.2,2,2)
ylim.min=c(0,0,1000,200)
ylim.max=c(200,50,2000,1000)
by.y.val=c(50,20,200,200)

var=c("TP.ugL","SRP.ugL","TN.mgL","DIN.mgL")
var.y.lab=paste("C\u1D62",
                c("TP (\u03BCg L\u207B\u00B9)",
                  "SRP (\u03BCg L\u207B\u00B9)",
                  "TN (mg L\u207B\u00B9)",
                  "DIN (mg L\u207B\u00B9)")
                )
var.x.lab=c("TP Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)",
          "SRP Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)",
          "TN Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)",
          "DIN Slope (\u03BCg L\u207B\u00B9 d\u207B\u00B9)")

for(j in 1:4){
  ylim.val=c(ylim.min[j],ylim.max[j]);ymaj=seq(ylim.val[1],ylim.val[2],by.y.val[j]);ymin=seq(ylim.val[1],ylim.val[2],by.y.val[j]/2)
  xlim.val=c(xlim.min[j],xlim.max[j]);xmaj=seq(xlim.val[1],xlim.val[2],by.x.val[j]);xmin=seq(xlim.val[1],xlim.val[2],by.x.val[j]/2)
  tmp.dat=subset(ann.slope.ice,variable==var[j])
  plot(Ci~slope.val,tmp.dat,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  points(Ci~slope.val,tmp.dat,
         pch=21,bg="dodgerblue1",cex=1.25,lwd=0.1)
  mod=mblm::mblm(Ci~slope.val,tmp.dat)
  x.val=with(tmp.dat,seq(min(slope.val),max(slope.val),length.out=50))
  mod.pred=predict(mod,data.frame(slope.val=x.val),interval="confidence")
  lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
  lines(x.val,mod.pred[,2],lty=2,col="black")
  lines(x.val,mod.pred[,3],lty=2,col="black")
  axis_fun(1,xmaj,xmin,format(xmaj,nsmall=1),line=-0.5)
  if(j%in%c(3:4)){axis_fun(2,ymaj,ymin,format(ymaj/1000))}else{axis_fun(2,ymaj,ymin,ymaj)}
  mtext(side=2,line=2.5,var.y.lab[j])
  mtext(side=1,line=1.5,var.x.lab[j])
  box(lwd=1)
}
dev.off()


vars1=c("TP.ugL","SRP.ugL","TN.mgL","DIN.mgL")
vars2=c("NOx.mgL","NH4.mgL","DP.ugL",'DOP.ugL')

ddply(subset(ann.slope.ice,variable%in%vars1),"variable",summarise,
      min.val=min(slope.val),
      mean.val=mean(slope.val),
      max.val=max(slope.val),
      sd.val=sd(slope.val))%>%
  flextable()%>%
  colformat_double(j=2:5,digits=2,na_str = "---")%>%
  compose(j="variable",i=~variable=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  compose(j="variable",i=~variable=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('TN (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  compose(j="variable",i=~variable=="DIN.mgL",value=as_paragraph('DIN (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  set_header_labels("variable" = "Parameter",
                    "min.val" = "Minimum",
                    "mean.val"='Mean',
                    "max.val"="Maximum",
                    "sd.val"="Standard\nDeviation")%>%
  width(width=c(1.25,0.75,0.75,0.75,0.75))%>%
  align(j=1,align="left",part="all")%>%
  valign(j=1,valign="top")%>%
  align(j=2:5,align="center",part="all")%>%
  padding(padding=1.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")#%>%print("docx")

ddply(subset(ann.slope.ice,variable%in%vars2),"variable",summarise,
      min.val=min(slope.val),
      mean.val=mean(slope.val),
      max.val=max(slope.val),
      sd.val=sd(slope.val))%>%
  flextable()%>%
  colformat_double(j=2:5,digits=2,na_str = "---")%>%
  compose(j="variable",i=~variable=="NOx.mgL",value=as_paragraph('NO\u2093 (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  compose(j="variable",i=~variable=="NH4.mgL",value=as_paragraph('NH\u2084 (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  compose(j="variable",i=~variable=="DP.ugL",value=as_paragraph('DP (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  compose(j="variable",i=~variable=="DOP.ugL",value=as_paragraph('DOP (\u03BCg L\u207B\u00B9 d\u00B9)'))%>%
  set_header_labels("variable" = "Parameter",
                    "min.val" = "Minimum",
                    "mean.val"='Mean',
                    "max.val"="Maximum",
                    "sd.val"="Standard\nDeviation")%>%
  width(width=c(1.25,0.75,0.75,0.75,0.75))%>%
  align(j=1,align="left",part="all")%>%
  valign(j=1,valign="top")%>%
  align(j=2:5,align="center",part="all")%>%
  padding(padding=1.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")# %>%print("docx")




### 
mod.TP.outlet=lm(log(value)~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0))
# gvlma::gvlma(mod.TP.outlet)
seg.mod.TP.outlet=segmented(mod.TP.outlet,~Ice_cum)
summary(seg.mod.TP.outlet)
mod.SRP.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0))
# gvlma::gvlma(mod.SRP.outlet)
seg.mod.SRP.outlet=segmented(mod.SRP.outlet,~Ice_cum)
summary(seg.mod.SRP.outlet)


range(subset(wq.dat.melt2,Ice_cum>0)$WY)
# png(filename=paste0(plot.path,"PLSF_IceOnYr.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,1.5),oma=c(2.5,1,1.5,0.25));
layout(matrix(c(1:4,5,5),2,3,byrow = F),widths=c(1,1,0.3))

WYs.val=seq(2011,2020)
# cols=adjustcolor(colorRampPalette(c("blue","dodgerblue1","indianred1","red"))(length(WYs.val)),0.5)
# cols=viridisLite::magma(length(WYs.val),alpha=0.5)
cols=scico::scico(length(WYs.val),palette="berlin",alpha=0.5)
cols=adjustcolor(rev(MetBrewer::met.brewer("Homer1",length(WYs.val),'continuous')),0.5)
xlim.val=c(0,180);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.min=c(20,1,0.5,0.1)
ylim.max=c(200,70,2.5,1.5)
var=c("TP.ugL","SRP.ugL","TN.mgL","DIN.mgL")
var.lab=c("TP (\u03BCg L\u207B\u00B9)","SRP (\u03BCg L\u207B\u00B9)","TN (mg L\u207B\u00B9)","DIN (mg L\u207B\u00B9)")

for(j in 1:4){
ylim.val=c(ylim.min[j],ylim.max[j]);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable==var[j]&Ice_cum>0)
plot(value~Ice_cum,tmp.dat,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:length(WYs.val)){
  with(subset(tmp.dat,WY==WYs.val[i]),pt_line(Ice_cum,value,2,cols[i],1,21,cols[i],pt.lwd=0.1))
}
mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
lines(x.val,mod.pred[,2],lty=2,col="black")
lines(x.val,mod.pred[,3],lty=2,col="black")
cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)

if(j%in%c(2,4)){axis_fun(1,xmaj,xmin,xmaj,line=-0.5)}else{axis_fun(1,xmaj,xmin,NA)}
if(j%in%c(2,4)){mtext(side=1,line=1.75,"Days Since Ice On")}
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1));box(lwd=1)
mtext(side=2,line=3,var.lab[j])
}

par(mar=c(1,0.5,0.5,0.5))
plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(WYs.val,cols,leg.title="Year",
        x.min=0,x.max=0.4,leg.type="continuous",xpd=NA)
legend(0.5,0.15,legend=c("Correlation\n\u00B1 95% CI"),
       pch=NA,pt.bg=NA,pt.cex=NA,
       lty=c(1),lwd=c(1),col=c("black"),
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)
dev.off()







# TCI vs WQ ---------------------------------------------------------------
wq.xtab1=dcast(wq.dat.melt,Date+Site+ice.period~variable,value.var = "value",mean)
wq.xtab1$winter=with(wq.xtab1,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),1,0))


tmp=subset(wq.xtab1,Site=="Lake_Outlet")
range(tmp$TCI,na.rm=T)


TCI.cor.rslt=rbind(
with(tmp,data.frame(param="TP",est=cor.test(TP.ugL,TCI,method="spearman")$estimate,pval=cor.test(TP.ugL,TCI,method="spearman")$p.value)),
with(tmp,data.frame(param="SRP",est=cor.test(SRP.ugL,TCI,method="spearman")$estimate,pval=cor.test(SRP.ugL,TCI,method="spearman")$p.value)),
with(tmp,data.frame(param="TN",est=cor.test(TN.mgL,TCI,method="spearman")$estimate,pval=cor.test(TN.mgL,TCI,method="spearman")$p.value)),
with(tmp,data.frame(param="DIN",est=cor.test(DIN.mgL,TCI,method="spearman")$estimate,pval=cor.test(DIN.mgL,TCI,method="spearman")$p.value))
)
round(TCI.cor.rslt$pval,2)

TN.TCI.mod=lm(TN.mgL~TCI,tmp)
summary(TN.TCI.mod)
pt(-4.693,352)

TN.TCI.mod.seg=segmented(TN.TCI.mod,~TCI)
TN.TCI.mod.seg$psi
summary(lm(TN.mgL~TCI,subset(tmp,TCI>=TN.TCI.mod.seg$psi[,2])))

DIN.TCI.mod=lm(DIN.mgL~TCI,tmp)
DIN.TCI.mod.seg=segmented(DIN.TCI.mod,~TCI)
DIN.TCI.mod.seg$psi

TP.TCI.mod=lm(TP.ugL~TCI,tmp)
TP.TCI.mod.seg=segmented(TP.TCI.mod,~TCI)
TP.TCI.mod.seg$psi
summary(lm(TP.ugL~TCI,subset(tmp,TCI>TP.TCI.mod.seg$psi[,2])))

SRP.TCI.mod=lm(SRP.ugL~TCI,tmp)
SRP.TCI.mod.seg=segmented(SRP.TCI.mod,~TCI)
SRP.TCI.mod.seg$psi

cpt.rslt=rbind(
  data.frame(param='TP',bk.pt=TP.TCI.mod.seg$psi[,2],se=TP.TCI.mod.seg$psi[,3]),
  data.frame(param='SRP',bk.pt=SRP.TCI.mod.seg$psi[,2],se=SRP.TCI.mod.seg$psi[,3]),
  data.frame(param='TN',bk.pt=TN.TCI.mod.seg$psi[,2],se=TN.TCI.mod.seg$psi[,3]),
  data.frame(param='DIN',bk.pt=DIN.TCI.mod.seg$psi[,2],se=DIN.TCI.mod.seg$psi[,3])
)


cbind(TCI.cor.rslt,cpt.rslt[,c("bk.pt","se")])%>%
  flextable()%>%
  colformat_double(j=2,digits = 2)%>%
  colformat_double(j=4,digits = 3)%>%
  colformat_double(j=5,digits = 2)%>%
  compose(j="pval",i=~pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="pval",i=~pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="bk.pt",i=~param=="DIN",value=as_paragraph('-6.8 x 10\u207B\u2076'))%>%
  set_header_labels("param"="Variable",
                    "est"="r-value",
                    "pval"="\u03C1-value",
                    "bk.pt"="Estimate",
                    "se"="Standard Error")%>%
  add_header("est"="Correlation",
             "pval"="Correlation",
             "bk.pt" = "Breakpoint",
             "se"="Breakpoint")%>%
  merge_h(part="header")%>%align(align="center",part="header")%>%
  width(width=c(0.75,1,1,1,1))%>%
  align(align="center",part="header")%>%
  align(j=2:5,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=11,part="all")%>%
  fontsize(size=12,part="header")# %>%print("docx")




cols=adjustcolor(c("white","dodgerblue1"),0.75)
ci.val=0.95
# png(filename=paste0(plot.path,"PLSF_WQ_TCI.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1),oma=c(2.5,2,1.5,0.25));
layout(matrix(1:4,2,2,byrow = F),widths=c(1,1))

xlim.val=c(-1.5,1.5);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(10,2000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(TP.ugL~TCI,tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(v=0)
points(TP.ugL~TCI,tmp,
       pch=21,bg=cols[ice.period],col="grey",lwd=0.01,cex=1.25)
mod=lm(TP.ugL~TCI,tmp)
mod.seg=segmented(mod,~TCI)
x.val=seq(min(tmp$TCI,na.rm=T),max(tmp$TCI,na.rm=T),length.out=50)
mod.pred=predict.segmented(mod.seg,data.frame(TCI=x.val),se.fit=T)
lines(x.val,mod.pred$fit,col="indianred1")
lines(x.val,mod.pred$fit-qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
lines(x.val,mod.pred$fit+qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)")

ylim.val=c(1,300);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(SRP.ugL~TCI,tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(v=0)
points(SRP.ugL~TCI,tmp,
       pch=21,bg=cols[ice.period],col="grey",lwd=0.01,cex=1.25)
mod=lm(SRP.ugL~TCI,tmp)
mod.seg=segmented(mod,~TCI)
x.val=seq(min(tmp$TCI,na.rm=T),max(tmp$TCI,na.rm=T),length.out=50)
mod.pred=predict.segmented(mod.seg,data.frame(TCI=x.val),se.fit=T)
lines(x.val,mod.pred$fit,col="indianred1")
lines(x.val,mod.pred$fit-qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
lines(x.val,mod.pred$fit+qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"SRP (\u03BCg L\u207B\u00B9)")

ylim.val=c(0.2,20);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(TN.mgL~TCI,tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(v=0)
points(TN.mgL~TCI,tmp,
       pch=21,bg=cols[ice.period],col="grey",lwd=0.01,cex=1.25)
mod=lm(TN.mgL~TCI,tmp)
mod.seg=segmented(mod,~TCI)
x.val=seq(min(tmp$TCI,na.rm=T),max(tmp$TCI,na.rm=T),length.out=50)
mod.pred=predict.segmented(mod.seg,data.frame(TCI=x.val),se.fit=T)
lines(x.val,mod.pred$fit,col="indianred1")
lines(x.val,mod.pred$fit-qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
lines(x.val,mod.pred$fit+qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TN (mg L\u207B\u00B9)")
legend("topleft",legend=c("No Ice","Ice"),
       pch=c(21),pt.bg=c(cols),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01),col=c("grey"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(0.001,2);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(DIN.mgL~TCI,tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(v=0)
points(DIN.mgL~TCI,tmp,
       pch=21,bg=cols[ice.period],col="grey",lwd=0.01,cex=1.25)
mod=lm(DIN.mgL~TCI,tmp)
mod.seg=segmented(mod,~TCI)
x.val=seq(min(tmp$TCI,na.rm=T),max(tmp$TCI,na.rm=T),length.out=50)
mod.pred=predict.segmented(mod.seg,data.frame(TCI=x.val),se.fit=T)
lines(x.val,mod.pred$fit,col="indianred1")
lines(x.val,mod.pred$fit-qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
lines(x.val,mod.pred$fit+qt(1-(1-ci.val)/2,mod.pred$df)*mod.pred$se.fit,lty=2,col="indianred1")
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"DIN (mg L\u207B\u00B9)")

mtext(side=1,line=1,outer=T,"Temp Change Index (\u2103 d\u207B\u00B9)")
dev.off()




# Biotia ------------------------------------------------------------------
## From PLSF "master" file
# names(dat)
# 
# # Biotia specific parameters
# bio.dat=dat[,c(1:3,102:363)]
# names(bio.dat)
# 
# # all WQ Vars
# bio.vars=c("Date", "Site", "ENKI",names(bio.dat)[4:265])
# colnames(bio.dat)=bio.vars
# 
# bio.vars2=c("Date", "Site", "ENKI",
#             "Phytoplankton_total_Total_biovolume_µm3/mL",
#             "Zooplankton_total_Total_biomass_µg/L",
#             "Protozoa_total_Total_biomass_µg/L"
# )
# bio.dat2=bio.dat[,bio.vars2]
# colnames(bio.dat2)=c(bio.vars2[1:3],"phyto_biovol_um3mL","zoo_biomass_ugL","proto_biomass_ugL")
# 
# subset(bio.dat2,is.na(phyto_biovol_um3mL)==F)
# 
# bio.dat2$winter=with(bio.dat2,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),1,0))
# bio.dat2$ice.period=with(bio.dat2,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:4),"Ice","NoIce"))
# bio.dat2$WY=WY(bio.dat2$Date)

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

vars=c('Date',"Site",
       "Phytoplankton.total_biovol_um3.mL",
       "Zooplankton.total_biomass_ug.L",
       "Protozoa.total_biomass_ug.L")
bio.dat2=bio.dat[,vars]

bio.dat2$winter=with(bio.dat2,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),1,0))
bio.dat2$ice.period=with(bio.dat2,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:4),"Ice","NoIce"))
bio.dat2$WY=WY(bio.dat2$Date)
### 

kruskal.test(Phytoplankton.total_biovol_um3.mL~ice.period,bio.dat2)
kruskal.test(Zooplankton.total_biomass_ug.L~ice.period,bio.dat2)
kruskal.test(Protozoa.total_biomass_ug.L~ice.period,bio.dat2);#very limited data

kruskal.test(Phytoplankton.total_biovol_um3.mL~ice.period,subset(bio.dat2,WY>2016))
kruskal.test(Zooplankton.total_biomass_ug.L~ice.period,subset(bio.dat2,WY>2016))

mean.ice=ddply(bio.dat2,c("Site","ice.period","WY"),summarise,
               mean.phyto=mean(Phytoplankton.total_biovol_um3.mL,na.rm=T),
               mean.zoo=mean(Zooplankton.total_biomass_ug.L,na.rm=T),
               mean.proto=mean(Protozoa.total_biomass_ug.L,na.rm=T))

plot(mean.phyto~WY,subset(mean.ice,Site=="Lake_Outlet"&ice.period=="NoIce"))
points(mean.phyto~WY,subset(mean.ice,Site=="Lake_Outlet"&ice.period=="Ice"),pch=21,bg="blue")

plot(mean.zoo~WY,subset(mean.ice,Site=="Lake_Outlet"&ice.period=="NoIce"))
points(mean.zoo~WY,subset(mean.ice,Site=="Lake_Outlet"&ice.period=="Ice"),pch=21,bg="blue")

plot(mean.proto~WY,subset(mean.ice,Site=="Lake_Outlet"&ice.period=="Ice"),pch=21,bg="blue",ylim=c(0,8e8))
points(mean.proto~WY,subset(mean.ice,Site=="Lake_Outlet"&ice.period=="NoIce"))

# plot(proto_biomass_ugL~Date,subset(bio.dat2,Site=="Lake_Outlet"))

## protozoa units are biovolume (um3/L) as per Barry
bio.dat2.outlet=subset(bio.dat2,Site=="Lake_Outlet")
bio.dat2.outlet[bio.dat2.outlet$zoo_biomass_ugL%in%0,]=NA
bio.dat2.outlet$month=as.numeric(format(bio.dat2.outlet$Date,'%m'))
bio.dat2.outlet$CY=as.numeric(format(bio.dat2.outlet$Date,'%Y'))

range(subset(bio.dat2.outlet,is.na(phyto_biovol_um3mL)==F)$Date)
range(subset(bio.dat2.outlet,is.na(zoo_biomass_ugL)==F)$Date)
range(subset(bio.dat2.outlet,is.na(proto_biomass_ugL)==F)$Date)

range(bio.dat2.outlet$Phytoplankton.total_biovol_um3.mL,na.rm=T)/1e6; # mm2/L
range(bio.dat2.outlet$Zooplankton.total_biomass_ug.L,na.rm=T)/1e3
range(bio.dat2.outlet$Protozoa.total_biomass_ug.L,na.rm=T)/1e6

bio.dat2.outlet=subset(bio.dat2.outlet,
       is.na(Phytoplankton.total_biovol_um3.mL)==F|is.na(Zooplankton.total_biomass_ug.L)==F|is.na(Protozoa.total_biomass_ug.L)==F)

bio.dat2.outlet=merge(bio.dat2.outlet,data.frame(expand.grid(CY=2009:2020,month=1:12)),c("CY","month"),all.y=T)

cols=c("olivedrab3","seashell3","black")
# png(filename=paste0(plot.path,"PLSF_Biota.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(2.5,6,1,0.25));
layout(matrix(1:6,3,2,byrow = T),widths=c(1,0.5))

xlim.val=date.fun(c("2009-12-01","2020-12-1"));by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
xlim.val2=c(1,12);by.x=3;xmaj2=seq(xlim.val2[1],xlim.val2[2],by.x);xmin2=seq(xlim.val2[1],xlim.val2[2],by.x/by.x)

ylim.val=c(1e4,5e8);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(Phytoplankton.total_biovol_um3.mL~Date,bio.dat2.outlet,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
yr.val=seq(2008,2021,1)
for(i in 1:length(yr.val)){
  xx.val=date.fun(c(paste(yr.val[i],"12-01",sep="-"),
                    paste(yr.val[i+1],"04-30",sep="-")))
  xx=c(xx.val,rev(xx.val))
  yy=c(1e3,1e3,5e9,5e9)
  polygon(xx,yy,col=adjustcolor("lightblue",0.5),border="grey")
}
with(bio.dat2.outlet,pt_line(Date,Phytoplankton.total_biovol_um3.mL,2,cols[1],1,21,cols[1],pt.lwd=0.01))
# axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj/1e6,scientific=F));box(lwd=1)
mtext(side=2,line=4,"Phytoplankton\nBiovolume (mm\u00B3 L\u207B\u00B9)",cex=0.8)

plot(Phytoplankton.total_biovol_um3.mL~month,bio.dat2.outlet,ylim=ylim.val,xlim=xlim.val2,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj2,lty=3,col="grey",lwd=0.75)
xx1=c(11.5,12.5)
xx2=c(0,4.5)
polygon(c(xx1,rev(xx1)),yy,col=adjustcolor("lightblue",0.5),border="grey")
polygon(c(xx2,rev(xx2)),yy,col=adjustcolor("lightblue",0.5),border="grey")
points(Phytoplankton.total_biovol_um3.mL~month,bio.dat2.outlet,pch=21,bg=cols[1],lwd=0.01)
# axis_fun(1,xmaj2,xmin2,month.abb[xmaj2],line=-0.5)
axis_fun(1,xmaj2,xmin2,NA,line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
# mod=loess(phyto_biovol_um3mL~month,subset(bio.dat2.outlet,is.na(phyto_biovol_um3mL)==F))
# x.val=seq(1,12,length.out=50)
# mod.pred=predict(mod,data.frame(month=x.val))
# lines(mod.pred~x.val,lwd=2,col=adjustcolor("red",0.5))

ylim.val=c(1,3e4);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(Zooplankton.total_biomass_ug.L~Date,bio.dat2.outlet,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
yr.val=seq(2008,2021,1)
for(i in 1:length(yr.val)){
  xx.val=date.fun(c(paste(yr.val[i],"12-01",sep="-"),
                    paste(yr.val[i+1],"04-30",sep="-")))
  xx=c(xx.val,rev(xx.val))
  yy=c(0.01,0.01,3e5,3e5)
  polygon(xx,yy,col=adjustcolor("lightblue",0.5),border="grey")
}
with(bio.dat2.outlet,pt_line(Date,Zooplankton.total_biomass_ug.L,2,cols[2],1,21,cols[2],pt.lwd=0.01))
# axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj/1e3,scientific=F));box(lwd=1)
mtext(side=2,line=4,"Zooplankton\nBiomass (mg L\u207B\u00B9)",cex=0.8)

plot(Zooplankton.total_biomass_ug.L~month,bio.dat2.outlet,ylim=ylim.val,xlim=xlim.val2,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj2,lty=3,col="grey",lwd=0.75)
xx1=c(11.5,12.5)
xx2=c(0,4.5)
polygon(c(xx1,rev(xx1)),yy,col=adjustcolor("lightblue",0.5),border="grey")
polygon(c(xx2,rev(xx2)),yy,col=adjustcolor("lightblue",0.5),border="grey")
points(Zooplankton.total_biomass_ug.L~month,bio.dat2.outlet,pch=21,bg=cols[2],lwd=0.01)
# axis_fun(1,xmaj2,xmin2,month.abb[xmaj2],line=-0.5)
axis_fun(1,xmaj2,xmin2,NA,line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)

ylim.val=c(1e5,1e9);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(Protozoa.total_biomass_ug.L~Date,bio.dat2.outlet,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
yr.val=seq(2008,2021,1)
for(i in 1:length(yr.val)){
  xx.val=date.fun(c(paste(yr.val[i],"12-01",sep="-"),
                    paste(yr.val[i+1],"04-30",sep="-")))
  xx=c(xx.val,rev(xx.val))
  yy=c(0.01,0.01,1e10,1e10)
  polygon(xx,yy,col=adjustcolor("lightblue",0.5),border="grey")
}
with(bio.dat2.outlet,pt_line(Date,Protozoa.total_biomass_ug.L,2,cols[3],1,21,cols[3],pt.lwd=0.01))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj/1e6,scientific=F));box(lwd=1);# convert um3/L to mm3/L
mtext(side=2,line=4,"Protozoa\nBiovolume (mm\u00B3 L\u207B\u00B9)",cex=0.8)
mtext(side=1,line=1.75,"Date (Month-Year)")

plot(Protozoa.total_biomass_ug.L~month,bio.dat2.outlet,ylim=ylim.val,xlim=xlim.val2,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj2,lty=3,col="grey",lwd=0.75)
xx1=c(11.5,12.5)
xx2=c(0,4.5)
polygon(c(xx1,rev(xx1)),yy,col=adjustcolor("lightblue",0.5),border="grey")
polygon(c(xx2,rev(xx2)),yy,col=adjustcolor("lightblue",0.5),border="grey")
points(Protozoa.total_biomass_ug.L~month,bio.dat2.outlet,pch=21,bg=cols[3],lwd=0.01)
axis_fun(1,xmaj2,xmin2,month.abb[xmaj2],line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Month")
dev.off()
##### END -----------------------------------------------------------------
# workspace image ---------------------------------------------------------
## Save workspace image to this point
# save.image(file=paste0(export.path,"PLSF_winter/PLSF_winter.RData"))
# load(paste0(export.path,"PLSF_winter/PLSF_winter.RData"))

# cumulative snow vs WQ ---------------------------------------------------
head(wx.dat.mean)
head(wq.dat.melt2)

wq.dat.melt3=merge(wq.dat.melt2,wx.dat.mean[,c('date',"WY.cumsnow","WY.cumsnow2")],by.x='Date',by.y="date",all.x=T)

plot(value~WY.cumsnow,subset(wq.dat.melt3,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0),log="y")
plot(value~WY.cumsnow,subset(wq.dat.melt3,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0),log="y")
plot(value~WY.cumsnow,subset(wq.dat.melt3,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0),log="y")
plot(value~WY.cumsnow,subset(wq.dat.melt3,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0),log="y")

tmp=subset(wq.dat.melt3,Site=='Lake_Outlet'&variable=="TP.ugL"&is.na(WY.cumsnow2)==F&WY.cumsnow2!=0)
plot(value~WY.cumsnow2,tmp,log="y")
mod=lm(log(value)~WY.cumsnow,tmp)
gvlma::gvlma(mod)
seg.mod=segmented::segmented(mod,~WY.cumsnow)
summary(seg.mod)

tmp=subset(wq.dat.melt3,Site=='Lake_Outlet'&variable=="TN.mgL"&is.na(WY.cumsnow2)==F&WY.cumsnow2!=0)
plot(value~WY.cumsnow,tmp,log="y")
mod=lm(log(value)~WY.cumsnow,tmp)
gvlma::gvlma(mod)
seg.mod=segmented::segmented(mod,~WY.cumsnow)
summary(seg.mod)


## anova?
tmp=lm(log(value)~WY.cumsnow+Ice_cum,tmp)
summary(tmp)
gvlma::gvlma(tmp)


tmp=lm(log(value)~WY.cumsnow+Ice_cum,tmp)
summary(tmp)
gvlma::gvlma(tmp)




# Ice Period - GAM --------------------------------------------------------

wq.xtab1
ice_period[,c('Date',"ice","Ice_cum")]
wx.dat.mean[,c("date","WY.cumFDD","mean.snow_gnd")]

tmp=merge(subset(wq.xtab1,Site=="Lake_Outlet"),ice_period[,c('Date',"ice","Ice_cum")],"Date",all.x=T)
tmp=merge(tmp,wx.dat.mean[,c("date","WY.cumFDD","mean.snow_gnd")],by.x="Date",by.y="date",all.x=T)
tmp$WY=WY(tmp$Date,WY.type='Fed')
tmp$DOY.WY=hydro.day(tmp$Date,WY.type = "Fed")

plot(DIN.mgL~WY,tmp)
plot(DIN.mgL~DOY.WY,tmp)
plot(DIN.mgL~Ice_cum,subset(tmp,Ice_cum!=0))
# tmp$Ice_cum=with(tmp,ifelse(Ice_cum==0,NA,Ice_cum))

plot(DIN.mgL~WY.cumFDD,tmp)     
plot(DIN.mgL~mean.snow_gnd,subset(tmp,Ice_cum!=0))     
plot(DIN.mgL~TCI,tmp)     


# tmp2=subset(tmp,Ice_cum!=0)
tmp2=tmp
tmp2$mean.snow_gnd=with(tmp2,ifelse(mean.snow_gnd==0,NA,mean.snow_gnd))

mod1=gam(log(DIN.mgL)~
           s(WY,k=8)+
           s(DOY.WY,bs="cc",k=10)+
           ti(WY,DOY.WY,bs=c("tp","cc"),k=c(8,10))+
           s(WY.cumFDD)+
           s(mean.snow_gnd)+
           s(TCI),
         data=tmp2,
         knots=list(WY=c(2014,2020)))
summary(mod1)
layout(matrix(1:4,2,2));gam.check(mod1)


draw(mod1,residuals = T)
range(subset(tmp2,is.na(DIN.mgL)==F)$WY)





# piecewise SEM -----------------------------------------------------------
library(piecewiseSEM)

tmp$decWY=decimal.WY(tmp$Date,WY.type = "Fed")
tmp2=subset(tmp,is.na(DIN.mgL)==F&is.na(TCI)==F)
tmp2$mean.snow_gnd=with(tmp2,ifelse(is.na(mean.snow_gnd)==T,0,mean.snow_gnd))

climate.mod=lm(TCI~WY.cumFDD+mean.snow_gnd,tmp2)
season.mod=lm(DIN.mgL~WY+DOY.WY,tmp2)
DIN.mod=lm(DIN.mgL~TCI+WY+DOY.WY,tmp2)

model=psem(climate.mod,season.mod)
model
