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
# Weather -----------------------------------------------------------------
library(weathercan)
# stations_dl()
# example
# weather_dl(station_ids=42203,start = "2017-01-06", end = "2017-01-10")

# dates=date.fun(c("2010-10-01","2021-09-30"))
dates=date.fun(c("1978-10-01","2021-09-30"))

subset(stations(),station_name=="BROMPTONVILLE")
subset(stations(),station_id%in%c(5327,5322))

# subset(stations(),station_name=="BONSECOURS")
# subset(stations(),climate_id==7024440)
weathercan:::get_html(station_id=5327,interval="day")

wx.dat=data.frame(weather_dl(station_ids=c(5327,5322),start = dates[1], end = dates[2],interval="day",trim=F))
wx.dat$date=date.fun(wx.dat$date)
wx.dat$CY=as.numeric(format(wx.dat$date,"%Y"))

plot(max_temp~date,wx.dat)
plot(mean_temp~date,wx.dat)
plot(total_precip~date,wx.dat)

# Spatially averaged
# wx.dat.mean=ddply(wx.dat,c("date"),summarise,mean.min_temp=mean(min_temp,na.rm=T))
# wx.dat.mean$inter.mean.min_temp=zoo::na.approx(wx.dat.mean$mean.min_temp)
# plot(mean.min_temp~date,wx.dat.mean)

wx.dat.da=ddply(wx.dat,c("date","CY"),summarise,
                mean.min_temp=mean(min_temp,na.rm=T),
                mean.total_precip=mean(total_precip,na.rm=T),
                mean.total_rain=mean(total_rain,na.rm=T),
                mean.total_snow=mean(total_snow,na.rm=T))

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

# plot(mean.min_temp~month,wx.dat.mean)
fdd.month=ddply(wx.dat.mean,c("CY","month","WY","winter"),summarise,
                FDD=sum(ifelse(mean.min_temp<0,abs(mean.min_temp),NA),na.rm=T),
                Tprep=sum(mean.total_precip,na.rm=T))
fdd.winter=ddply(subset(wx.dat.mean,winter==1),c("WY"),summarise,
                 FDD=sum(ifelse(mean.min_temp<0,(mean.min_temp*-1),NA),na.rm=T),
                 mean.temp=mean(inter.mean.min_temp,na.rm=T),
                 Tprep=sum(mean.total_precip,na.rm=T))
ann.precip=ddply(wx.dat.mean,c("WY"),summarise,
                 sum.total_precip=sum(mean.total_precip,na.rm=T)*0.1,
                 prep.nas=sum(is.na(mean.total_precip)),
                 sum.RF=sum(mean.total_rain,na.rm=T)*0.1,
                 sum.SF=sum(mean.total_snow,na.rm=T)*0.1)
plot(sum.total_precip~WY,ann.precip)

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


ylim.val=c(700,1700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
bks=seq(700,1700,100)
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

with(fdd.winter,cor.test(WY,FDD,method="kendall"))
mblm(FDD~WY,fdd.winter)

# winter precip
with(fdd.winter,cor.test(WY,Tprep,method="kendall"))
mblm(FDD~WY,fdd.winter)

with(ann.precip,cor.test(WY,sum.total_precip,method="kendall"))
with(subset(ann.precip,WY>=2010),cor.test(WY,sum.total_precip,method="kendall"))
mblm(sum.total_precip~WY,subset(ann.precip,WY>=2010))


# Multivariate ENSO -------------------------------------------------------
# https://psl.noaa.gov/enso/mei/
row.count=length(seq(1979,2022,1))
var.names=paste0(c("D",substring(month.name,1,1)),substring(month.name,1,1))[1:12]
ENSO.dat=read.table("https://psl.noaa.gov/enso/mei/data/meiv2.data",
                    header=F,skip=1,sep="",na.string="-999.00",
                    nrows=row.count)
colnames(ENSO.dat)=c("YR",var.names)

ENSO.dat.melt=melt(ENSO.dat,id.vars = "YR")
ENSO.dat.melt$WY=with(ENSO.dat.melt,ifelse(variable%in%c("ON",'ND'),YR+1,YR))

ENSO.winter=ddply(subset(ENSO.dat.melt,variable%in%c("DJ","JF","FM")),"WY",summarise,ENSO.mean=mean(value,na.rm=T))
plot(ENSO.mean~WY,ENSO.winter,type="b")

with(ENSO.winter,cor.test(WY,ENSO.mean,method="kendall"))
with(subset(ENSO.winter,WY>=2000),cor.test(WY,ENSO.mean,method="kendall"))


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
plot(ma~Date.mon,AMO.dat.melt)


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
wq.dat[wq.dat$TPReversal==1,c("TP.ugL","SRP.ugL")]

## TN Reversal Check
nrow(subset(wq.dat,is.na(NOx.mgL)==F&is.na(NH4.mgL)==F&is.na(Urea.mgL)==F))
nrow(subset(wq.dat,is.na(NOx.mgL)==F&is.na(NH4.mgL)==F|is.na(Urea.mgL)==T))

wq.dat$TNReversal=with(wq.dat,ifelse(is.na(DIN.mgL)==T|is.na(TN.mgL)==T,0,ifelse(DIN.mgL>(TN.mgL*1.3),1,0)));
sum(wq.dat$TNReversal,na.rm=T)
subset(wq.dat,TNReversal==1)
plot(TN.mgL~DIN.mgL,wq.dat,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TNReversal==1,"blue",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

wq.dat=subset(wq.dat,TPReversal==0|TNReversal==0)

pre.out.screen-nrow(wq.dat)

## Explore TCI 
plot(DIN.mgL~TCI,subset(wq.dat,Site=="Lake_Outlet"))
mod=lm(DIN.mgL~TCI,subset(wq.dat,Site=="Lake_Outlet"))
library(segmented)
seg.mod=segmented(mod,~TCI)
plot(seg.mod,add=T)
plot(SRP.ugL~TCI,subset(wq.dat,Site=="Lake_Outlet"),log="y")

idvars=c("Date","Site")
paramvars=c(paste(c("TP","SRP","DP"),"ugL",sep="."),
            paste(c("TN","NOx",'NH4',"DIN"),"mgL",sep="."),
            "Chla.ugL","Phyco.ugL", "TChl.ugL",
            "Temp.C","DO.per","TCI")

wq.dat.melt=melt(wq.dat[,c(idvars,paramvars)],id.vars = idvars)
wq.dat.melt$month=as.numeric(format(wq.dat.melt$Date,"%m"))
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)


# Ice period compare ------------------------------------------------------

wq.dat.melt$ice.period=with(wq.dat.melt,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:4),"Ice","NoIce"))
wq.dat.melt$ice.period=factor(wq.dat.melt$ice.period,level=c("NoIce","Ice"))
wq.dat.melt$WY=WY(wq.dat.melt$Date,WY.type = "FL")

sites.vals=c("Godbout","Lake_Outlet")
## "In_Lake" not sampled during winter
ice_period.mean=ddply(subset(wq.dat.melt,Site%in%sites.vals),c("Site","WY","ice.period","variable"),summarise,
                  mean.val=mean(value,na.rm=T),
                  GM.val=exp(mean(log(value),na.rm=T)),
                  N.val=N.obs(value))

boxplot(mean.val~ice.period,subset(ice_period.mean,Site==sites.vals[1]&variable=="TP.ugL"),outline=F,loog="y")
rslt=kruskal.test(mean.val~ice.period,subset(ice_period.mean,Site==sites.vals[1]&variable=="TP.ugL"))
if(rslt$p.value<0.05){text(ylim.val[2],1.5,"*")}

# png(filename=paste0(plot.path,"PLSF_SeaComp_P.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:4,2,2,byrow = T),widths=c(1,1))
xaxs.cex=0.8

ylim.val=c(10,300);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
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
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(1,100);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="SRP.ugL"
  axes.lab="SRP (\u03BCg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_SeaComp_N.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:4,2,2,byrow = T),widths=c(1,1))
xaxs.cex=0.8

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
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.1,2);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="DIN.mgL"
  axes.lab="DIN (mg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_SeaComp_N2.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:4,2,2,byrow = T),widths=c(1,1))
xaxs.cex=0.8

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
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
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
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_SeaComp_Algae.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:4,2,2,byrow = T),widths=c(1,1))
xaxs.cex=0.8

ylim.val=c(0.9,20);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="TChl.ugL"
  axes.lab=expression(paste("Chl"[italic(" in-vivo")]," (",mu,"g L"^"-1",")")) #(\u03BCg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0.001,0.5);by.y=200;ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
for(i in 1:2){
  param.val="Phyco.ugL"
  axes.lab=expression(paste("Phyco"[italic(" in-vivo")]," (",mu,"g L"^"-1",")")) #(\u03BCg L\u207B\u00B9)"
  tmp.dat=subset(ice_period.mean,Site==sites.vals[i]&variable==param.val)
  x=boxplot(mean.val~ice.period,tmp.dat,
            outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F,log="y")
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(mean.val~ice.period,tmp.dat)
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
dev.off()

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

wq.dat.melt.bimonth=merge(wq.dat.melt.bimonth,ENSO.dat.melt[,c('YR',"variable","value")],by.x=c("MEI.yr","month.period"),by.y=c("YR","variable"))
head(wq.dat.melt.bimonth)
wq.dat.melt.bimonth=rename(wq.dat.melt.bimonth,c("value"="MEI.ENSO"))

subset(wq.dat.melt.bimonth,variable=="TP.ugL"&mean.val>200)
subset(wq.dat.melt.bimonth,N.val==1)

wq.dat.melt.bimonth$mean.val=with(wq.dat.melt.bimonth,ifelse(N.val==1,NA,mean.val))

plot(mean.val~MEI.ENSO,subset(wq.dat.melt.bimonth,variable=="TP.ugL"&Site%in%sites.vals[1]),log="y")
plot(mean.val~MEI.ENSO,subset(wq.dat.melt.bimonth,variable=="TP.ugL"&month.period%in%c("SO","ON",'ND')&Site%in%sites.vals[1]),log="y")

plot(mean.val~MEI.ENSO,subset(wq.dat.melt.bimonth,variable=="TN.mgL"&Site%in%sites.vals[1]))
plot(mean.val~MEI.ENSO,subset(wq.dat.melt.bimonth,variable=="TN.mgL"&month.period%in%c("ON",'ND')&Site%in%sites.vals[2]))


ddply(subset(wq.dat.melt.bimonth,Site%in%sites.vals&variable%in%c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")),
      c("Site","variable"),summarise,
      r.val=cor.test(mean.val,MEI.ENSO,method="spearman")$estimate,
      p.val=round(cor.test(mean.val,MEI.ENSO,method="spearman")$p.value,3))


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

mod.NOX.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="NOx.mgL"&Ice_cum>0))
# gvlma::gvlma(mod.NOX.outlet)
seg.mod.NOX.outlet=segmented(mod.NOX.outlet,~Ice_cum)
summary(seg.mod.NOX.outlet)
mod.NH4.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="NH4.mgL"&Ice_cum>0))
# gvlma::gvlma(mod.NH4.outlet)
seg.mod.NH4.outlet=segmented(mod.NH4.outlet,~Ice_cum)
summary(seg.mod.NH4.outlet)

mod.DIN.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0))
# gvlma::gvlma(mod.DIN.outlet)
seg.mod.DIN.outlet=segmented(mod.DIN.outlet,~Ice_cum)
summary(seg.mod.DIN.outlet)
mod.TN.outlet=lm(log(value)~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0))
# gvlma::gvlma(mod.TN.outlet)
seg.mod.TN.outlet=segmented(mod.TN.outlet,~Ice_cum)
summary(seg.mod.TN.outlet)

mod.TP.outlet=lm(log(value)~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0))
# gvlma::gvlma(mod.TP.outlet)
seg.mod.TP.outlet=segmented(mod.TP.outlet,~Ice_cum)
summary(seg.mod.TP.outlet)
mod.SRP.outlet=lm(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0))
# gvlma::gvlma(mod.SRP.outlet)
seg.mod.SRP.outlet=segmented(mod.SRP.outlet,~Ice_cum)
summary(seg.mod.SRP.outlet)

# png(filename=paste0(plot.path,"PLSF_IceOn.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1),oma=c(2.5,2,1.5,0.25));
layout(matrix(1:4,2,2,byrow = F),widths=c(1,1))

xlim.val=c(0,180);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0)
ylim.val=c(0.5,2.5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(value~Ice_cum,tmp.dat,
       pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
lines(x.val,mod.pred[,2],lty=2,col="black")
lines(x.val,mod.pred[,3],lty=2,col="black")
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1));box(lwd=1)
mtext(side=2,line=2,"TN (mg L\u207B\u00B9)")
cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)

tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0)
ylim.val=c(0.1,1.5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(value~Ice_cum,tmp.dat,
       pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
lines(x.val,mod.pred[,2],lty=2,col="black")
lines(x.val,mod.pred[,3],lty=2,col="black")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1));box(lwd=1)
mtext(side=2,line=2,"DIN (mg L\u207B\u00B9)")
cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)

tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0)
ylim.val=c(20,200);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(value~Ice_cum,tmp.dat,
       pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
lines(x.val,mod.pred[,2],lty=2,col="black")
lines(x.val,mod.pred[,3],lty=2,col="black")
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0));box(lwd=1)
mtext(side=3,adj=1,"Lake Outlet ",font=3)
cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)

tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0)
ylim.val=c(1,70);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(value~Ice_cum,tmp.dat,
       pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
lines(x.val,mod.pred[,2],lty=2,col="black")
lines(x.val,mod.pred[,3],lty=2,col="black")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0));box(lwd=1)
mtext(side=2,line=2,"SRP (\u03BCg L\u207B\u00B9)")
mod.pred=predict.segmented(seg.mod.SRP.outlet,data.frame(Ice_cum=x.val))
lines(x.val,mod.pred,col="red",lwd=1)
mtext(side=1,outer=T,line=1,"Days since Ice On")
cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)

dev.off()

tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="NOx.mgL"&Ice_cum>0)
tmp.dat$value=with(tmp.dat,ifelse(value<0.1,NA,value))
ylim.val=c(0.1,2);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
points(value~Ice_cum,tmp.dat,
       pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
lines(x.val,mod.pred[,2],lty=2,col="black")
lines(x.val,mod.pred[,3],lty=2,col="black")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0));box(lwd=1)
mtext(side=2,line=2,"NOx (mg L\u207B\u00B9)")
mtext(side=1,outer=T,line=1,"Days since Ice On")
cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)



# TCI vs WQ ---------------------------------------------------------------
wq.xtab1=dcast(wq.dat.melt,Date+Site+ice.period~variable,value.var = "value",mean)

tmp=subset(wq.xtab1,Site=="Lake_Outlet")
cols=adjustcolor(c("white","dodgerblue1"),0.75)
ci.val=0.95
# png(filename=paste0(plot.path,"PLSF_WQ_TCI.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1),oma=c(2.5,2,1.5,0.25));
layout(matrix(1:4,2,2,byrow = F),widths=c(1,1))

xlim.val=c(-1.5,1.5);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
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

mtext(side=1,line=1,outer=T,"Temp Change Index (\u2103 d\u207B\u00B9)")
dev.off()