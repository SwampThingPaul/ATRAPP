## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Sampling Frequency question
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

# GAM
library(mgcv)
library(gratia)
library(DHARMa)
library(ggplot2)

# Trend Analysis
library(modifiedmk)
library(trend)
library(EnvStats)

# Tables
library(flextable)
library(magrittr)


#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_1/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


## Functions
MDL_func=function(data,MDL,rng.val=TRUE){
  tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
  tmp=ifelse(tmp<MDL,MDL/2,tmp)
  if(rng.val==TRUE){print(range(tmp,na.rm=T))}
  return(tmp)
}

# MDLs --------------------------------------------------------------------
# units = mg/L
TP.MDL=0.7*0.001
SRP.MDL=TP.MDL
DP.MDL=TP.MDL
NOx.MDL=0.0004
NH4.MDL=0.0014
TN.MDL=0.004
SolN.MDL=0.004
Urea.MDL=0.03
TOC.MDL=0.15
SolOC.MDL=0.15
Turb.MDL=2
Color.MDL=15

Al.MDL=0.04
Sb.MDL=0.02; #antimony
As.MDL=0.05
Ba.MDL=0.002
Cd.MDL=0.0002
Ca.MDL=0.5
Cr.MDL=0.01
Co.MDL=0.001
Cu.MDL=0.009
Fe.MDL=0.1
Mg.MDL=0.5
Mn.MDL=0.01
Mo.MDL=0.001
Ni.MDL=0.01
Pb.MDL=0.01
K.MDL=0.5
Se.MDL=0.05
Ag.MDL=0.01
Na.MDL=0.5
Zn.MDL=0.02

# -------------------------------------------------------------------------
dat=read.xlsx(paste0(data.path,"PLSF Database-12 Years (v2021-07-07).xlsx"))
dat$Date=date.fun(convertToDate(dat$Date))

## Parsing main dataset into separate files
names(dat)
params.all=names(dat)
match(params.all,"Chlorophyll-a_µg/L")
which(params.all=="Chlorophyll-a_µg/L")      
which(params.all=="Conductivity") 
which(params.all=="Colour") 
which(params.all=="Secchi_cm") 

# Water quality specific parameters
wq.dat=dat[,c(1:20,364:374,387)]
names(wq.dat)

# WQ Vars
# TDS units = mg/L?
# ORP units = mV?
# Resistivity = ohm?
# Phycocyanin = ug/L?
# Chlorophyll_invitro=ug/L?
wq.vars=c("Date", "Site", "ENKI", "N_P", "TP.mgL", 
          "PP.calc.mgL", "DP.mgL","SRP.mgL", "DOP.calc.mgL", 
          "TN.mgL", "TKN.mgL", "NH4.mgL","NOx.mgL", "Urea.mgL", "DON.mgL", 
          "SolN.mgL", "SolOC.mgL", "TOC.mgL", 
          "pH", "Chla.ugL", "Cond", "DO.per", 
          "TDS.mgL", "Temp.C", "ORP.mV", "Sal",
          "Resistivity.ohm","Phyco.ugL", "TChl.ugL", 
          "Turb.NTU", "Colour_PCU","Secchi_cm")
colnames(wq.dat)=wq.vars

wq.dat$CY=as.numeric(format(wq.dat$Date,"%Y"))
wq.dat$DOY=as.numeric(format(wq.dat$Date,"%j"))
wq.dat$month=as.numeric(format(wq.dat$Date,"%m"))
wq.dat$time=as.numeric(wq.dat$Date)/100; # From .../PLSF/code/biophysical parameters.RMD
wq.dat$WY=WY(wq.dat$Date,WY.type='Fed')
wq.dat$CY.d=lubridate::decimal_date(wq.dat$Date)

### Need MDL values for parameters
summary(wq.dat)
unique(wq.dat$SRP.mgL)
tmp=as.numeric(wq.dat$SRP.mgL)
range(tmp[tmp!=0],na.rm=T)

wq.dat$TP.ugL=with(wq.dat,MDL_func(TP.mgL,TP.MDL)*1000)
wq.dat$SRP.ugL=with(wq.dat,MDL_func(SRP.mgL,SRP.MDL)*1000)
wq.dat$DP.ugL=with(wq.dat,MDL_func(DP.mgL,DP.MDL)*1000)

wq.dat$NOx.mgL=with(wq.dat,MDL_func(NOx.mgL,NOx.MDL))
wq.dat$NH4.mgL=with(wq.dat,MDL_func(NH4.mgL,NH4.MDL))
wq.dat$TN.mgL=with(wq.dat,MDL_func(TN.mgL,TN.MDL))
wq.dat$SolN.mgL=with(wq.dat,MDL_func(SolN.mgL,SolN.MDL))
wq.dat$Urea.mgL=with(wq.dat,MDL_func(Urea.mgL,Urea.MDL))

wq.dat$TOC.mgL=with(wq.dat,MDL_func(TOC.mgL,TOC.MDL))
wq.dat$SolOC.mgL=with(wq.dat,MDL_func(SolOC.mgL,SolOC.MDL))

wq.dat$Chla.ugL=with(wq.dat,MDL_func(Chla.ugL,0.1))
## Finding high sediment outliers (as communicated by Barry)
plot(TP.ugL~Turb.NTU,wq.dat)
plot(TN.mgL~Turb.NTU,wq.dat)

wq.dat$Turb.NTU=with(wq.dat,MDL_func(Turb.NTU,Turb.MDL))
wq.dat$Colour_PCU=with(wq.dat,MDL_func(Colour_PCU,Color.MDL))

# sampling error/outlier?
turb.outliers=subset(wq.dat,Turb.NTU>100)
High.TPTN=subset(wq.dat,TP.ugL>5000)

pre.out.screen=nrow(wq.dat)
wq.dat=subset(wq.dat,Turb.NTU<100|is.na(Turb.NTU)==T)
wq.dat=subset(wq.dat,TP.ugL<5000|is.na(TP.ugL)==T)

pre.out.screen-nrow(wq.dat)

subset(wq.dat,Site=="Godbout"&month%in%c(1,2)&CY==2011)
wq.dat[wq.dat$Site=="Godbout"&wq.dat$Date==date.fun("2011-02-06"),c("TP.ugL","PP.ugL")]=NA

subset(wq.dat,NH4.mgL>1)
wq.dat[wq.dat$Site=="Godbout"&wq.dat$Date==date.fun("2019-12-02"),"NH4.mgL"]=NA
## TP Reversal Check 
plot(TP.ugL~SRP.ugL,wq.dat);abline(0,1)
plot(TP.ugL~DP.ugL,wq.dat);abline(0,1)
plot(DP.ugL~SRP.ugL,wq.dat);abline(0,1)

# Evaluation
wq.dat$TPReversal=with(wq.dat,ifelse(is.na(SRP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(SRP.ugL>(TP.ugL*1.3),1,0)));
sum(wq.dat$TPReversal,na.rm=T)
subset(wq.dat,TPReversal==1)
plot(TP.ugL~SRP.ugL,wq.dat,ylab="TP (\u03BCg L\u207B\u00b9)",xlab="SRP (\u03BCg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")
wq.dat[wq.dat$TPReversal==1,c("TP.ugL","SRP.ugL")]

wq.dat$TPReversal2=with(wq.dat,ifelse(is.na(DP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(DP.ugL>(TP.ugL*1.3),1,0)));
sum(wq.dat$TPReversal2,na.rm=T)
subset(wq.dat,TPReversal2==1)

P.species=c("TP.ugL","DP.ugL",'SRP.ugL')
wq.dat[wq.dat$TPReversal==1|wq.dat$TPReversal2==1,P.species]=NA

## TN Reversal Check
nrow(subset(wq.dat,is.na(NOx.mgL)==F&is.na(NH4.mgL)==F&is.na(Urea.mgL)==F))
nrow(subset(wq.dat,is.na(NOx.mgL)==F&is.na(NH4.mgL)==F|is.na(Urea.mgL)==T))
wq.dat$DIN.mgL=with(wq.dat,ifelse(is.na(Urea.mgL)==T,NOx.mgL+NH4.mgL,NOx.mgL+NH4.mgL+Urea.mgL))
plot(TN.mgL~DIN.mgL,wq.dat);abline(0,1)
plot(TN.mgL~TKN.mgL,wq.dat);abline(0,1)

# Evaluation
wq.dat$TNReversal=with(wq.dat,ifelse(is.na(DIN.mgL)==T|is.na(TN.mgL)==T,0,ifelse(DIN.mgL>(TN.mgL*1.3),1,0)));
sum(wq.dat$TNReversal,na.rm=T)
subset(wq.dat,TNReversal==1)
plot(TN.mgL~DIN.mgL,wq.dat,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TNReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

n.species=c("TN.mgL","TKN.mgL","NH4.mgL","NOx.mgL","Urea.mgL","SolN.mgL",'DIN.mgL')
wq.dat[wq.dat$TNReversal==1,n.species]=NA

## Calculated values
wq.dat$PP.ugL=with(wq.dat,ifelse((TP.ugL-DP.ugL)<=0,TP.ugL,TP.ugL-DP.ugL))
range(wq.dat$PP.ugL,na.rm=T)
range(wq.dat$DP.ugL,na.rm=T)
range(wq.dat$TP.ugL,na.rm=T)
plot(PP.ugL~PP.calc.mgL,wq.dat);abline(0,1000)
abline(v=c(0.75,0.9))
subset(wq.dat,PP.calc.mgL>0.75&PP.calc.mgL<0.9);# calculation error in original dataset?

wq.dat$DOP.ugL=with(wq.dat,ifelse((DP.ugL-SRP.ugL)<=0,DP.ugL,DP.ugL-SRP.ugL))
range(wq.dat$DOP.ugL,na.rm=T)
range(wq.dat$DP.ugL,na.rm=T)
range(wq.dat$SRP.ugL,na.rm=T)
plot(DOP.ugL~DOP.calc.mgL,wq.dat);abline(0,1000)
subset(wq.dat,(DOP.ugL/(DOP.calc.mgL*1000))<1)

wq.dat$DON.calc=with(wq.dat,ifelse((SolN.mgL-DIN.mgL)<SolN.mgL,SolN.mgL,SolN.mgL-DIN))
range(wq.dat$DON.calc,na.rm=T)
plot(DON.mgL~DON.calc,wq.dat);abline(0,1)

wq.dat$DON.mgL=wq.dat$DON.calc
wq.dat$TON.mgL=with(wq.dat,ifelse(TKN.mgL<NH4.mgL|(TKN.mgL-NH4.mgL)==0,TKN.mgL,TKN.mgL-NH4.mgL))
range(wq.dat$TON.mgL,na.rm=T)

# wq.dat$DON.mgL
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107

wq.dat$TP.mM=with(wq.dat,(TP.ugL/1000)/P.mw)
wq.dat$TN.mM=with(wq.dat,TN.mgL/N.mw)
wq.dat$SRP.mM=with(wq.dat,(SRP.ugL/1000)/P.mw)
wq.dat$DIN.mM=with(wq.dat,DIN.mgL/N.mw)
wq.dat$TOC.mM=with(wq.dat,TOC.mgL/C.mw)

wq.dat$TN_TP=with(wq.dat,TN.mM/TP.mM)
wq.dat$TOC_TP=with(wq.dat,TOC.mM/TP.mM)
wq.dat$TOC_TN=with(wq.dat,TOC.mM/TN.mM)
wq.dat$DIN_SRP=with(wq.dat,DIN.mM/SRP.mM)

wq.dat$PP_TP=with(wq.dat,(PP.ugL/TP.ugL)*100)
wq.dat$DP_TP=with(wq.dat,ifelse((DP.ugL/TP.ugL)>1,100,(DP.ugL/TP.ugL)*100))
wq.dat$SRP_TP=with(wq.dat,(SRP.ugL/TP.ugL)*100)

wq.dat$NOx_TN=with(wq.dat,(NOx.mgL/TN.mgL)*100)
wq.dat$NH4_TN=with(wq.dat,(NH4.mgL/TN.mgL)*100)
##
## Other Parameters

range(wq.dat$Colour_PCU,na.rm=T)
# wq.dat$Colour_PCU[wq.dat$Colour_PCU==0]=1

range(wq.dat$Temp.C,na.rm=T)
range(wq.dat$Cond,na.rm=T)
range(wq.dat$DO.per,na.rm=T)
subset(wq.dat,DO.per>200)
wq.dat$DO.per[wq.dat$DO.per>200]=NA

range(wq.dat$pH,na.rm=T)
range(wq.dat$Chla.ugL,na.rm=T)
range(wq.dat$Phyco.ugL,na.rm=T)
range(wq.dat$TChl.ugL,na.rm=T)
##
# png(filename=paste0(plot.path,"chl_compare.png"),width=6,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,1,1,0.25));
ylim.val=c(0.05,100);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(0.05,4000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")

plot(TChl.ugL~Chla.ugL,wq.dat,type="n",ylim=ylim.val,xlim=xlim.val,log="xy",axes=F,ann=F);
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
abline(0,1,lwd=2)
with(subset(wq.dat,Site=="In_Lake"),points(TChl.ugL~Chla.ugL,pch=21,bg="lightblue",cex=1.25,lwd=0.01))
with(subset(wq.dat,Site=="Lake_Outlet"),points(TChl.ugL~Chla.ugL,pch=21,bg="indianred",cex=1.25,lwd=0.01))
with(subset(wq.dat,Site=="Godbout"),points(TChl.ugL~Chla.ugL,pch=21,bg="orange",cex=1.25,lwd=0.01))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Chl-a Lab (\u03BCg L\u207B\u00B9)")
mtext(side=2,line=2,"Chl in-vivo (\u03BCg L\u207B\u00B9)")
legend("bottomright",legend=c("In-Lake","Lake Outlet","Lake Inlet","1:1 line"),
       pch=c(21,21,21,NA),lty=c(NA,NA,NA,1),lwd=c(0.1,0.1,0.1,1),
       col="black",pt.bg=c("lightblue","red","orange",NA),
       pt.cex=1.25,ncol=2,cex=0.75,bty="n",y.intersp=2,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()

## Dissolved Oxygen values
## Screen out 2010 to 2012 - error in calibration/calculations
wq.dat
plot(DO.per~Date,subset(wq.dat,Site=="Godbout"))
points(DO.per~Date,subset(wq.dat,Site=="Godbout"&Date>date.fun("2012-12-31")),pch=21,bg="red")

wq.dat$DO.per=with(wq.dat,ifelse(Date<date.fun("2012-12-31"),NA,DO.per))



# -------------------------------------------------------------------------
idvars=c("Date","Site","CY","CY.d","DOY","month")
paramvars=c("TP.ugL", "SRP.ugL", "DP.ugL","PP.ugL", "DOP.ugL",
            "TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL", "SolN.mgL","Urea.mgL","DON.mgL", "TON.mgL", "TKN.mgL",
            "TOC.mgL","SolOC.mgL",
            "TN_TP","TOC_TP", "TOC_TN", "DIN_SRP","PP_TP","SRP_TP","DP_TP","NH4_TN","NOx_TN",
            "Chla.ugL","Cond","DO.per","Colour_PCU","Turb.NTU","Secchi_cm",
            "Temp.C","pH","Phyco.ugL", "TChl.ugL")
wq.dat.melt=melt(wq.dat[,c(idvars,paramvars)],id.vars = idvars)
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)
unique(wq.dat.melt$variable)
head(subset(wq.dat.melt,variable=="Cond"))

wq.dat.melt$week.num=with(wq.dat.melt,as.numeric(format(wq.dat.melt$Date,"%U")))
biweek.samp=seq(0,53,2)
mon.samp=date.fun(datetimeutils::nth_day(wq.dat.melt$Date,"month","first"))

plot(CY~week.num,subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL"))
points(CY~week.num,subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL"&week.num%in%biweek.samp),pch=21,bg="red")
points(CY~week.num,subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL"&Date%in%mon.samp),pch=21,bg="green")






month.mean.all.dat=ddply(wq.dat.melt,c("Site","month","CY","variable"),summarise,
                     mean.value=mean(value,na.rm=T),
                     SE.value=SE(value),
                     sd.value=sd(value),
                     N.val=N.obs(value))

month.mean.bi.dat=ddply(subset(wq.dat.melt,week.num%in%biweek.samp),c("Site","month","CY","variable"),summarise,
                         mean.value=mean(value,na.rm=T),
                         SE.value=SE(value),
                         sd.value=sd(value),
                         N.val=N.obs(value))

month.mean.mon.dat=ddply(subset(wq.dat.melt,Date%in%mon.samp),c("Site","month","CY","variable"),summarise,
                        mean.value=mean(value,na.rm=T),
                        SE.value=SE(value),
                        sd.value=sd(value),
                        N.val=N.obs(value))
# variability -------------------------------------------------------------
all.dat.TP=subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL")
all.dat.TP$freq="weekly"

bi.dat.TP=subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL"&week.num%in%biweek.samp)
bi.dat.TP$freq="biweekly"

mon.dat.TP=subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL"&Date%in%mon.samp)
mon.dat.TP$freq="monthly"

sampfreq.dat=rbind(all.dat.TP,bi.dat.TP,mon.dat.TP)
sampfreq.dat$freq=factor(sampfreq.dat$freq,levels=c("weekly","biweekly","monthly"))

boxplot(value~freq,sampfreq.dat,outline=F)

## Turco and Haas
freq.sumstats=ddply(sampfreq.dat,"freq",summarise,
      var.val=var(value,na.rm=T),
      sd.val=sd(value,na.rm=T),
      IQR.val=IQR(value,na.rm=T),
      mean.val=mean(value,na.rm=T),
      N.val=N.obs(value))

zval=qt(0.025,1000,lower.tail = F)
# example from Turco and Maas
((zval^2)*(0.131^2))/((0.148*0.05)^2)

freq.sumstats$n.power=with(freq.sumstats,((zval^2)*(sd.val^2))/(mean.val*0.1)^2)

freq.sumstats$Lval=with(freq.sumstats,sqrt((zval^2)*(sd.val^2)/N.val)/mean.val)
freq.sumstats

# double check math
((zval^2)*(159.4643^2))/((159.4643*0.09)^2)

### LOESS smoothing
# interesting discussion on loess 
# https://stats.stackexchange.com/questions/557381/how-do-i-interpret-or-explain-loess-plot

all.dat.k=loess(value~CY.d,subset(all.dat.TP,is.na(value)==F))
bi.dat.k=loess(value~CY.d,subset(bi.dat.TP,is.na(value)==F))
mon.dat.k=loess(value~CY.d,subset(mon.dat.TP,is.na(value)==F))


# png(filename=paste0(plot.path,"Sampling_Freq/PLSF_TPOutlet_LOESS.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,2.5,0.5,0.25));

ylim.val=c(20,2200);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")# ;by.y=0.1;ymaj=seq(min(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(min(c(0,ylim.val[1])),ylim.val[2],by.y/2)
xlim.val=c(2010,2021);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(value~CY.d,subset(all.dat.TP,is.na(value)==F),type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(all.dat.TP,pt_line(CY.d,value,2,"grey",1,21,"grey"))

tmp.x=subset(all.dat.TP,is.na(value)==F)$CY.d
pred.k=predict(all.dat.k)
lines(tmp.x,pred.k,col="dodgerblue1",lwd=2)

tmp.x=subset(bi.dat.TP,is.na(value)==F)$CY.d
pred.k=predict(bi.dat.k)
lines(tmp.x,pred.k,col="indianred1",lwd=2)

tmp.x=subset(mon.dat.TP,is.na(value)==F)$CY.d
pred.k=predict(mon.dat.k)
lines(tmp.x,pred.k,col="forestgreen",lwd=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
mtext(side=2,line=3,"TP (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=1.5,"Year")
mtext(side=3,adj=0,"PLSF - Lake Outlet")

legend("topleft",legend=c("All data","Weekly LOESS smooth","Biweekly LOESS smooth","Monthly LOESS smooth"),
       lty=c(0,1,1,1),lwd=c(0.1,2,2,2),col=c("black","dodgerblue1","indianred1","forestgreen"),
       pch=c(21,NA,NA,NA),pt.bg=c("grey",NA,NA,NA),
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=0.8,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()
# Trend Detection ---------------------------------------------------------

tmp.dat=subset(month.mean.all.dat,Site=="Lake_Outlet"&variable=="TP.ugL")
sort(unique(tmp.dat$CY))
yrs=seq(2011,2020,1)
yrs2=yrs[3:length(yrs)]

trend.detect.all=data.frame()
for(i in 1:length(yrs2)){
  trend.test=with(subset(tmp.dat,CY%in%seq(2011,yrs2[i],1)),
                  kendallSeasonalTrendTest(mean.value,month,CY))
  rslt=data.frame(Site="Lake_Outlet",param="TP.ugL",CY=yrs2[i],
                  N.val=as.numeric(trend.test$sample.size["Total"]),
                  chisq.stat=as.numeric(trend.test$statistic[1]),
                  chisq.pval=as.numeric(trend.test$p.value[1]),
                  z.stat=as.numeric(trend.test$statistic[2]),
                  tau=as.numeric(trend.test$estimate[1]),
                  z.pval=as.numeric(trend.test$p.value[2]),
                  sen.slope=as.numeric(trend.test$estimate[2]),
                  sen.int=as.numeric(trend.test$estimate[3])
  )
  trend.detect.all=rbind(trend.detect.all,rslt)
  print(i)
}
trend.detect.all$yrs=trend.detect.all$CY-min(tmp.dat$CY)


tmp.dat=subset(month.mean.bi.dat,Site=="Lake_Outlet"&variable=="TP.ugL")
sort(unique(tmp.dat$CY))
yrs=seq(2011,2020,1)
yrs2=yrs[3:length(yrs)]

trend.detect.bi=data.frame()
for(i in 1:length(yrs2)){
  trend.test=with(subset(tmp.dat,CY%in%seq(2011,yrs2[i],1)),
                  kendallSeasonalTrendTest(mean.value,month,CY))
  rslt=data.frame(Site="Lake_Outlet",param="TP.ugL",CY=yrs2[i],
                  N.val=as.numeric(trend.test$sample.size["Total"]),
                  chisq.stat=as.numeric(trend.test$statistic[1]),
                  chisq.pval=as.numeric(trend.test$p.value[1]),
                  z.stat=as.numeric(trend.test$statistic[2]),
                  tau=as.numeric(trend.test$estimate[1]),
                  z.pval=as.numeric(trend.test$p.value[2]),
                  sen.slope=as.numeric(trend.test$estimate[2]),
                  sen.int=as.numeric(trend.test$estimate[3])
  )
  trend.detect.bi=rbind(trend.detect.bi,rslt)
  print(i)
}
trend.detect.bi$yrs=trend.detect.bi$CY-min(tmp.dat$CY)


tmp.dat=subset(month.mean.mon.dat,Site=="Lake_Outlet"&variable=="TP.ugL")
sort(unique(tmp.dat$CY))
yrs=seq(2011,2020,1)
yrs2=yrs[3:length(yrs)]

trend.detect.mon=data.frame()
for(i in 1:length(yrs2)){
  trend.test=with(subset(tmp.dat,CY%in%seq(2011,yrs2[i],1)),
                  kendallSeasonalTrendTest(mean.value,month,CY))
  rslt=data.frame(Site="Lake_Outlet",param="TP.ugL",CY=yrs2[i],
                  N.val=as.numeric(trend.test$sample.size["Total"]),
                  chisq.stat=as.numeric(trend.test$statistic[1]),
                  chisq.pval=as.numeric(trend.test$p.value[1]),
                  z.stat=as.numeric(trend.test$statistic[2]),
                  tau=as.numeric(trend.test$estimate[1]),
                  z.pval=as.numeric(trend.test$p.value[2]),
                  sen.slope=as.numeric(trend.test$estimate[2]),
                  sen.int=as.numeric(trend.test$estimate[3])
  )
  trend.detect.mon=rbind(trend.detect.mon,rslt)
  print(i)
}
trend.detect.mon$yrs=trend.detect.mon$CY-min(tmp.dat$CY)

col.rmp=viridis::plasma(4,direction = -1,alpha=0.5)
b2=c(1e-05, 0.001, 0.01, 0.05, 1)
trend.detect.all$bks=with(trend.detect.all,findInterval(z.pval,b2,rightmost.closed = F,left.open=T))
trend.detect.bi$bks=with(trend.detect.bi,findInterval(z.pval,b2,rightmost.closed = F,left.open=T))
trend.detect.mon$bks=with(trend.detect.mon,findInterval(z.pval,b2,rightmost.closed = F,left.open=T))


# png(filename=paste0(plot.path,"Sampling_Freq/PLSF_TPOutletTrenddetect_pval.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,1),oma=c(3,2.5,1.5,0.25));

ylim.val=c(0.00001,1);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")# ;by.y=0.1;ymaj=seq(min(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(min(c(0,ylim.val[1])),ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(z.pval~yrs,trend.detect.all,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(trend.detect.all,pt_line(yrs,z.pval,1,adjustcolor("black",0.5),1.5,21,"dodgerblue1",1.5,0.1))
with(trend.detect.bi,pt_line(yrs,z.pval,1,adjustcolor("black",0.5),1.5,21,"indianred1",1.5,0.1))
with(trend.detect.mon,pt_line(yrs,z.pval,1,adjustcolor("black",0.5),1.5,21,"darkolivegreen2",1.5,0.1))
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
abline(h=c(0.01,0.05),lty=2,col=c("firebrick1","firebrick4"))
mtext(side=3,adj=0,line=-1," Month as season",cex=0.75)

legend("bottomleft",legend=c("Weekly","Biweekly","Monthly","\u03C1-value = 0.01","\u03C1-value = 0.05"),
       lty=c(1,1,1,2,2),lwd=c(1.5),col=c(rep(adjustcolor("black",0.5),3),"firebrick1","firebrick4"),
       pch=c(NA),pt.bg=c(NA),
       pt.cex=1,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,text.col="white")
legend("bottomleft",legend=c("Weekly","Biweekly","Monthly","\u03C1-value = 0.01","\u03C1-value = 0.05"),
       lty=c(1,1,1,2,2),lwd=c(rep(0.1,3),1.5,1.5),col=c(rep(adjustcolor("black",0.5),3),"firebrick1","firebrick4"),
       pch=c(rep(21,3),NA,NA),pt.bg=c("dodgerblue1","indianred1","darkolivegreen2",NA,NA),
       pt.cex=1,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
mtext(side=3,adj=0,"Lake Outlet - Total Phosphorus")
mtext(side=2,line=4,"Mann-Kendall Seasonal Trend \u03C1-value")
mtext(side=1,line=2,"Years of Monitoring")
dev.off()

# png(filename=paste0(plot.path,"Sampling_Freq/PLSF_TPOutletTrenddetect.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:4,1,4,byrow = T),widths=c(1,1,1,0.5))

ylim.val=c(-8,22);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(sen.slope~yrs,trend.detect.all,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(trend.detect.all,pt_line(yrs,sen.slope,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
with(subset(trend.detect.all,z.pval<=0.05)[1,],arrows(yrs,-10,yrs,sen.slope-abs(sen.slope)*0.075,length=0.1,col="red",lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=0.9)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"Lake Outlet - Total Phosphorus")
mtext(side=2,line=2,"Thiel-Sen Slope (\u03BCg P L\u207B\u00B9 Yr\u207B\u00B9)")
mtext(side=1,line=2,"Years of Monitoring")
mtext(side=3,adj=1,line=-1.5,"Weekly \nSampling ",cex=0.5)

plot(sen.slope~yrs,trend.detect.all,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(trend.detect.bi,pt_line(yrs,sen.slope,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
with(subset(trend.detect.bi,sen.slope<0&z.pval<=0.05)[1,],arrows(yrs,-10,yrs,sen.slope-abs(sen.slope)*0.075,length=0.1,col="red",lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=0.9)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=2,"Years of Monitoring")
mtext(side=3,adj=1,line=-1.5,"Bi-weekly \nSampling ",cex=0.5)

plot(sen.slope~yrs,trend.detect.all,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(trend.detect.mon,pt_line(yrs,sen.slope,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
with(subset(trend.detect.mon,sen.slope<0&z.pval<=0.05)[1,],arrows(yrs,-10,yrs,sen.slope-abs(sen.slope)*0.075,length=0.1,col="red",lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=0.9)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=2,"Years of Monitoring")
mtext(side=3,adj=1,line=-1.5,"Monthly \nSampling ",cex=0.5)

plot(0:1,0:1,ann=F,axes=F,type="n")
b2=round(format(b2,scientific = F,nsmall=3),3)
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0("\u2265 ",b2[(l.b-1)])))
n.bks=length(b2) -1
top.val=0.8
bot.val=0.2
mid.v.val=bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
# legend_image=as.raster(matrix(rev(pal2),ncol=1))
# rasterImage(legend_image,x.min,bot.val,x.max,top.val)
# text(x=x.max, y = c(bot.val,mid.v.val,top.val), labels = format(c(min(b2),0,max(b2))),cex=0.75,adj=0,pos=4,offset=0.5)
bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(col.rmp),lty=0)
text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,"Trend \u03C1-value",adj=0,cex=0.8,pos=3,xpd=NA)
dev.off()


### add statistical power analysis/plot

