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

# png(filename=paste0(plot.path,"PLSF_winterATemp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,0.5,0.25));

ylim.val=c(-20,-5);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1980,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

bks=seq(-20,-7,0.5)
cols=viridis::plasma(length(bks))
col.vals=findInterval(fdd.winter$mean.temp,bks)

plot(mean.temp~WY,fdd.winter,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(fdd.winter,pt_line(WY,mean.temp,2,"grey40",1,21,cols[col.vals],1.25,0.01))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,'Mean Winter Air Temp (\u2103)')
mtext(side=1,line=2.5,'Water Year\n(Oct - Sept)')
mtext(side=1,adj=1,line=2.75,'Location: near Petit-lac-Saint-François\nData Source: Meteorological Service of Canada',
      cex=0.5,col="grey50",font=3)
dev.off()

# png(filename=paste0(plot.path,"PLSF_winter_cFDD.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,3,0.5,0.25));

ylim.val=c(700,1700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1980,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

bks=seq(700,1700,100)
cols=viridis::plasma(length(bks),direction=-1)
col.vals=findInterval(fdd.winter$FDD,bks)

plot(FDD~WY,fdd.winter,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(fdd.winter,pt_line(WY,FDD,2,"grey40",1,21,cols[col.vals],1.25,0.01))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=2,line=2.5,label_wrap_fun('Winter Cumulative Freezing Degree Days (\u2103)',30))
mtext(side=2,line=2.5,'Winter Cumulative\nFreezing Degree Days (\u2103)')
mtext(side=1,line=2.5,'Water Year\n(Oct - Sept)')
mtext(side=1,adj=1,line=2.75,'Location: near Petit-lac-Saint-François\nData Source: Meteorological Service of Canada',
      cex=0.5,col="grey50",font=3)
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
with(subset(ENSO.winter,WY>=2010),cor.test(WY,ENSO.mean,method="kendall"))



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


idvars=c("Date","Site")
paramvars=c(paste(c("TP","SRP","DP"),"ugL",sep="."),
            paste(c("TN","NOx",'NH4',"DIN"),"mgL",sep="."),
            "Chla.ugL","Phyco.ugL", "TChl.ugL",
            "Temp.C","DO.per")

wq.dat.melt=melt(wq.dat[,c(idvars,paramvars)],id.vars = idvars)
wq.dat.melt$month=as.numeric(format(wq.dat.melt$Date,"%m"))
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)
# Ice period comparison ---------------------------------------------
## median ice/no ice with KW test stats for inflow and outflow

plot(value~Date,subset(wq.dat.melt, variable=="Temp.C"& Site=="Godbout"))
tmp=subset(wq.dat.melt, variable=="Temp.C"& Site=="Godbout")
tmp$yr=as.numeric(format(tmp$Date,"%Y"))
tmp$month=as.numeric(format(tmp$Date,"%m"))
tmp$WY=WY(tmp$Date,WY.type="Fed")
test=ddply(tmp,"yr",summarise,
      min.date=as.POSIXct(min(ifelse(value<=0.5,Date,NA),na.rm=T),origin="1970-01-01",tz="EST"))

plot(value~month,tmp)

tmp$autumn=with(tmp,ifelse(month%in%seq(9,12,1),1,0))

test=ddply(subset(tmp,autumn==1),"yr",summarise,
      mean.temp=mean(value,na.rm=T))

test2=merge(ice.dat2,test,by.x="year",by.y="yr")
plot(mean.temp~Ice_On,test2)

## FWM rather than grab concentration
## or compare when flowing?

## boxplot for all params

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
with(subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0),cor.test(value,Ice_cum,method="spearman"))
with(subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0),cor.test(value,Ice_cum,method="spearman"))

with(subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0),cor.test(value,Ice_cum,method="spearman"))
with(subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0),cor.test(value,Ice_cum,method="spearman"))


plot(value~Ice_cum,subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0),log="y")


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
rslt=with(tmp.dat,cor.test(value,Ice_cum,method="kendall"))
mtext(side=3,line=-1.25,adj=1,cex=0.75,paste("\u03C4 = ",round(rslt$estimate,2),
                                             "; \u03C1",ifelse(rslt$p.value<0.01,"<0.01",
                                                               ifelse(rslt$p.value<0.05,"<0.05",
                                                                      paste("=",round(rslt$p.value,2)))),"  "))

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
rslt=with(tmp.dat,cor.test(value,Ice_cum,method="kendall"))
mtext(side=3,line=-1.25,adj=1,cex=0.75,paste("\u03C4 = ",round(rslt$estimate,2),
                                             "; \u03C1",ifelse(rslt$p.value<0.01,"<0.01",
                                                               ifelse(rslt$p.value<0.05,"<0.05",
                                                                      paste("=",round(rslt$p.value,2)))),"  "))


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
mtext(side=2,line=2,"TP (\u03BCg L\u207B\u00B9)")
rslt=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
rslt=with(tmp.dat,cor.test(value,Ice_cum,method="kendall"))
mtext(side=3,line=-1.25,adj=1,cex=0.75,paste("\u03C4 = ",round(rslt$estimate,2),
                                             "; \u03C1",ifelse(rslt$p.value<0.01,"<0.01",
                                                               ifelse(rslt$p.value<0.05,"<0.05",
                                                                      paste("=",round(rslt$p.value,2)))),"  "))

mtext(side=3,adj=1,"Lake Outlet ",font=3)

tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0)
ylim.val=c(1,60);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
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
rslt=with(tmp.dat,cor.test(value,Ice_cum,method="kendall"))
mtext(side=3,line=-1.25,adj=1,cex=0.75,paste("\u03C4 = ",round(rslt$estimate,2),
                                             "; \u03C1",ifelse(rslt$p.value<0.01,"<0.01",
                                                               ifelse(rslt$p.value<0.05,"<0.05",
                                                                      paste("=",round(rslt$p.value,2)))),"  "))

mtext(side=1,outer=T,line=1,"Days since Ice On")
dev.off()
# Winter trend ------------------------------------------------------------
wq.dat.melt$WY=WY(wq.dat.melt$Date,WY.type = "Fed")
wq.dat.melt$winter=with(wq.dat.melt,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),1,0))
wq.dat.melt$winter.txt=with(wq.dat.melt,ifelse(as.numeric(format(Date,"%m"))%in%c(12,1:2),"winter","other"))

## "In_Lake" not sampled during winter
winter.mean=ddply(subset(wq.dat.melt,winter==1&Site%in%sites.vals),c("Site","WY","winter","variable"),summarise,
                  mean.val=mean(value,na.rm=T),
                  GM.val=exp(mean(log(value),na.rm=T)),
                  N.val=N.obs(value))

# winter.mean=ddply(subset(wq.dat.melt,ice.sea=="Ice"&Site%in%sites.vals),c("Site","WY","variable"),summarise,
#                   mean.val=mean(value,na.rm=T),
#                   GM.val=exp(mean(log(value),na.rm=T)),
#                   N.val=N.obs(value))

trend.scr=ddply(winter.mean,c("Site","variable"),summarise,N.Yrs=N.obs(mean.val))
trend.scr$trend.scrn=with(trend.scr,ifelse(N.Yrs>=3,1,0))
winter.mean=merge(winter.mean,trend.scr,c("Site","variable"))

winter.mean=winter.mean[order(winter.mean$WY,winter.mean$Site,winter.mean$variable),]

winter.mean.trendrslt=ddply(subset(winter.mean,trend.scrn==1),c("Site","variable"),summarise,
      N.val=N.obs(mean.val),
      est=cor.test(mean.val,WY,method="kendall")$estimate,
      pval=cor.test(mean.val,WY,method="kendall")$p.value,
      sen.slope=coef(mblm(mean.val~WY,repeated=F))[2],
      sen.inter=coef(mblm(mean.val~WY,repeated=F))[1])


xlim.val=c(2010,2020);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)


var.val=paramvars=c(paste(c("TP","SRP","DP"),"ugL",sep="."),
                    paste(c("TN","NOx",'NH4',"DIN"),"mgL",sep="."),
                    "Phyco.ugL", "TChl.ugL",
                    "Temp.C","DO.per")
var.val.labs=c(paste(c("TP","SRP","DP"),"(\u03BCg L\u207B\u00B9)"),
               paste(c("TN","NOx",'NH4',"DIN"),"(mg L\u207B\u00B9)"),
               "Phyco. (\u03BCg L\u207B\u00B9)",
               "Chl (\u03BCg L\u207B\u00B9)",
               "Temp (\u2103)","DO (% Sat)")
par(family="serif",mar=c(1,2.5,0.5,2),oma=c(3,2,1.5,0.25));
layout(matrix(1:12,3,4,byrow = T))

ylim.min_max=ddply(winter.mean,"variable",summarise,min.val=min(mean.val)-min(mean.val)*0.2,max.val=max(mean.val)+max(mean.val)*0.2)
for(i in 1:length(var.val)){

ylim.val=with(subset(ylim.min_max,variable==var.val[i]),c(min.val,max.val));
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

tmp.dat=subset(winter.mean,variable==var.val[i])
plot(mean.val~WY,winter.mean,xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,log="y",type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(tmp.dat,Site%in%sites.vals[1]),pt_line(WY,mean.val,2,"dodgerblue1",1,21,"dodgerblue1",cex=1.25))
tmp.trend=with(subset(winter.mean.trendrslt,variable==var.val[i]&Site%in%sites.vals[1]),sen.slope*range(tmp.dat$WY)+sen.inter)
lines(range(tmp.dat$WY),tmp.trend,lwd=2,col="dodgerblue1")
with(subset(tmp.dat,Site%in%sites.vals[2]),pt_line(WY,mean.val,2,"indianred1",1,21,"indianred1",cex=1.25))
tmp.trend=with(subset(winter.mean.trendrslt,variable==var.val[i]&Site%in%sites.vals[2]),sen.slope*range(tmp.dat$WY)+sen.inter)
lines(range(tmp.dat$WY),tmp.trend,lwd=2,col="indianred1")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
mtext(side=2,line=3,var.val.labs[i])
}



## A loop to run trend analysis and store data
paramvars
paramvars2=c("TP.ugL", "SRP.ugL", "DP.ugL", "TN.mgL", "NOx.mgL", "NH4.mgL","DIN.mgL")
winter.sea.mean=ddply(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars2),c("Site","WY","ice.sea","variable"),summarise,
                      mean.val=mean(value,na.rm=T),
                      GM.val=exp(mean(log(value),na.rm=T)),
                      N.val=N.obs(value))
trend.scr=ddply(winter.sea.mean,c("Site","variable"),summarise,N.Yrs=N.obs(mean.val))
trend.scr$trend.scrn=with(trend.scr,ifelse(N.Yrs>=3,1,0))
winter.sea.mean=merge(winter.sea.mean,trend.scr,c("Site","variable"))
winter.sea.mean=subset(winter.sea.mean,trend.scrn==1&N.val>2)
# winter.sea.mean=subset(winter.sea.mean,!variable%in%paramvars[8:12])

trend.rslt=data.frame()
for(i in 1:length(paramvars2)){
  tmp=subset(winter.sea.mean,variable==paramvars2[i])
  for(j in 1:length(sites.vals)){
    tmp2=subset(tmp,Site==sites.vals[j])
  trend.test=with(tmp2,kendallSeasonalTrendTest(mean.val,ice.sea,WY))
  rslt=data.frame(Site=sites.vals[j],season="ice",param=paramvars[i],
                  N.val=as.numeric(trend.test$sample.size["Total"]),
                  chisq.stat=as.numeric(trend.test$statistic[1]),
                  chisq.pval=as.numeric(trend.test$p.value[1]),
                  z.stat=as.numeric(trend.test$statistic[2]),
                  tau=as.numeric(trend.test$estimate[1]),
                  z.pval=as.numeric(trend.test$p.value[2]),
                  sen.slope=as.numeric(trend.test$estimate[2])
                  # sen.int=as.numeric(trend.test$estimate[3])
  )
  trend.rslt=rbind(trend.rslt,rslt)
  }
}
trend.rslt

winter.sea.mean=ddply(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars2),c("Site","WY","winter.txt","variable"),summarise,
                      mean.val=mean(value,na.rm=T),
                      GM.val=exp(mean(log(value),na.rm=T)),
                      N.val=N.obs(value))
trend.scr=ddply(winter.sea.mean,c("Site","variable"),summarise,N.Yrs=N.obs(mean.val))
trend.scr$trend.scrn=with(trend.scr,ifelse(N.Yrs>=3,1,0))
winter.sea.mean=merge(winter.sea.mean,trend.scr,c("Site","variable"))
winter.sea.mean=subset(winter.sea.mean,trend.scrn==1&N.val>2)

trend.rslt2=data.frame()
for(i in 1:length(paramvars2)){
  tmp=subset(winter.sea.mean,variable==paramvars2[i])
  for(j in 1:length(sites.vals)){
    tmp2=subset(tmp,Site==sites.vals[j])
    trend.test=with(tmp2,kendallSeasonalTrendTest(mean.val,winter.txt,WY))
    rslt=data.frame(Site=sites.vals[j],season="winter.txt",param=paramvars[i],
                    N.val=as.numeric(trend.test$sample.size["Total"]),
                    chisq.stat=as.numeric(trend.test$statistic[1]),
                    chisq.pval=as.numeric(trend.test$p.value[1]),
                    z.stat=as.numeric(trend.test$statistic[2]),
                    tau=as.numeric(trend.test$estimate[1]),
                    z.pval=as.numeric(trend.test$p.value[2]),
                    sen.slope=as.numeric(trend.test$estimate[2])
                    # sen.int=as.numeric(trend.test$estimate[3])
    )
    trend.rslt2=rbind(trend.rslt2,rslt)
  }
}
trend.rslt2


## correlations
tmp.NOx=dcast(subset(winter.sea.mean,variable=="NOx.mgL"&winter.txt=="winter"),WY~Site,value.var="mean.val",mean)
# tmp.NOx=subset(tmp.NOx,WY>2012)

tmp.dat=merge(tmp.NOx,ENSO.winter,all.x=T,"WY")
tmp.dat=merge(tmp.dat,fdd.winter,all.x=T,"WY")


plot(Godbout~ENSO.mean,tmp.dat)
plot(Lake_Outlet~ENSO.mean,tmp.dat)

plot(Godbout~FDD,tmp.dat)
plot(Lake_Outlet~FDD,tmp.dat)

plot(Godbout~mean.temp,tmp.dat)
plot(Lake_Outlet~mean.temp,tmp.dat)

plot(Godbout~Tprep,tmp.dat)
plot(Lake_Outlet~Tprep,tmp.dat)

tmp.lm1=lm(Godbout~ENSO.mean+FDD+mean.temp+Tprep,tmp.dat)
summary(tmp.lm1)
gvlma::gvlma(tmp.lm1)
car::vif(tmp.lm1)
layout(matrix(1:4,2,2));plot(tmp.lm1)

library(nlme)
tmp.gls1=gls(Godbout~ENSO.mean+FDD+mean.temp+Tprep,subset(tmp.dat,WY>2012))
summary(tmp.gls1)
plot(tmp.gls1)

tmp.lm2=lm(Lake_Outlet~ENSO.mean+FDD+mean.temp+Tprep,tmp.dat)
summary(tmp.lm2)
gvlma::gvlma(tmp.lm2)
car::vif(tmp.lm2)
layout(matrix(1:4,2,2));plot(tmp.lm2)

## Linear mixed models
## https://m-clark.github.io/mixed-models-with-R/random_intercepts.html
tmp.NOX=subset(winter.sea.mean,variable=="NOx.mgL"&winter.txt=="winter")
tmp.NOX=subset(tmp.NOX,WY>2012)
tmp.dat=merge(tmp.NOX,ENSO.winter,all.x=T,"WY")
tmp.dat=merge(tmp.dat,fdd.winter,all.x=T,"WY")


library(lme4)
lme1=lmer(mean.val~WY+as.factor(Site)+(1|Site)+(1|WY),tmp.dat)
summary(lme1)

ranef(lme1)
coef(lme1)

library(merTools)
tmp.dat2=cbind(tmp.dat,predictInterval(lme1))

plot(fit~WY,subset(tmp.dat2,Site=="Godbout"),ylim=c(0,1.5))
lines(lwr~WY,subset(tmp.dat2,Site=="Godbout"))
lines(upr~WY,subset(tmp.dat2,Site=="Godbout"))

plot(fit~WY,subset(tmp.dat2,Site=="Lake_Outlet"),ylim=c(0,1.5))
lines(lwr~WY,subset(tmp.dat2,Site=="Lake_Outlet"))
lines(upr~WY,subset(tmp.dat2,Site=="Lake_Outlet"))

REsim(lme1) 
plotREsim(REsim(lme1)) 

## GAMM
library(mgcv)
b=gamm(log(mean.val)~s(WY,k=7)+s(ENSO.mean,k=4),data=tmp.dat,random=list(Site=~1))
# summary(b)
summary(b$gam)
summary(b$lme)

gam.check(b$gam)

gratia::draw(b)
# plot(b$gam)
plot(b$lme)

# MARSS -------------------------------------------------------------------
# https://atsa-es.github.io/atsa-labs/chap-msscov.html

library(MARSS)


tmp.NOx=dcast(subset(winter.sea.mean,variable=="NOx.mgL"&winter.txt=="winter"),WY~Site,value.var="mean.val",mean)
tmp.NOx=subset(tmp.NOx,WY>2012)

tmp.TN=dcast(subset(winter.sea.mean,variable=="TN.mgL"&winter.txt=="winter"),WY~Site,value.var="mean.val",mean)
ENSO.winter
fdd.winter

tmp.dat=merge(tmp.NOx,ENSO.winter,all.x=T,"WY")
tmp.dat=merge(tmp.dat,fdd.winter,all.x=T,"WY")

years=tmp.dat[,'WY']
marss.dat=t(tmp.dat[,c("Godbout","Lake_Outlet")])
covar.dat=t(tmp.dat[,c("ENSO.mean","FDD","mean.temp")])

## Observation-error only model
Q <- U <- x0 <- "zero"
B <- Z <- "identity"
d <- covar.dat
A <- "zero"
D <- "unconstrained"
y <- marss.dat  # to show relationship between dat & the equation
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, 
                   d = d, x0 = x0)
kem <- MARSS(y, model = model.list)


# Process-error only model
R <- A <- U <- "zero"
B <- Z <- "identity"
Q <- "equalvarcov"
C <- "unconstrained"
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   C = C, c = covar.dat)
kem <- MARSS(marss.dat, model = model.list)

model.list$B <- "diagonal and unequal"
kem <- MARSS(marss.dat, model = model.list)


x0 <- marss.dat[, 1, drop = FALSE]
model.list$tinitx <- 1
model.list$x0 <- x0
kem <- MARSS(marss.dat, model = model.list)


# Both process- and observation-error
D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
C <- "unconstrained"
c <- covar.dat
R <- diag(0.16, 2)
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(marss.dat, model = model.list)