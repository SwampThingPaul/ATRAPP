## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 05/06/2022

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
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

# period of change -------------------------------------------------------
## For GAMs
## see https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
              tmpf)
source(tmpf)


## Other Functions
MDL_func=function(data,MDL,rng.val=TRUE){
  tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
  tmp=ifelse(tmp<MDL,MDL/2,tmp)
  if(rng.val==TRUE){print(range(tmp,na.rm=T))}
  return(tmp)
}

GAM.SigChange.fun=function(model,pdat,CI.alpha=0.95){
  # function to stream line significant changes in GAM 
  # for DOY and CY
  
  alpha=(1-CI.alpha)/2
  df.res <- df.residual(model)
  crit.t <- qt(alpha, df.res, lower.tail = FALSE)
  
  pdat <- transform(pdat,
                    upper.DOY = fit.DOY + (crit.t * SE.DOY),
                    lower.DOY = fit.DOY - (crit.t * SE.DOY),
                    upper.CY = fit.CY + (crit.t * SE.CY),
                    lower.CY = fit.CY - (crit.t * SE.CY),
                    upper.DOYCY = fit.DOYCY + (crit.t * SE.DOYCY),
                    lower.DOYCY = fit.DOYCY - (crit.t * SE.DOYCY))
  # CY significant change
  m.CY.d <- derivatives(model,newdata=pdat,
                        term='s(CY)',type = "central",interval="confidence",ncores=12)
  m.CY.dsig <- signifD(pdat$fit.CY,
                       d=m.CY.d$derivative,
                       pdat$upper.CY,
                       pdat$lower.CY)
  pdat$dsig.CY.incr=unlist(m.CY.dsig$incr)
  pdat$dsig.CY.decr=unlist(m.CY.dsig$decr)
  
  # DOY significant change
  m.DOY.d <- derivatives(model,newdata=pdat,
                         term='s(DOY)',type = "central",interval="confidence",ncores=12)
  m.DOY.dsig <- signifD(pdat$fit.DOY,
                        d=m.DOY.d$derivative,
                        pdat$upper.DOY,
                        pdat$lower.DOY)
  pdat$dsig.DOY.incr=unlist(m.DOY.dsig$incr)
  pdat$dsig.DOY.decr=unlist(m.DOY.dsig$decr)
  return(pdat)
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

# Water quality specific parameters
wq.dat=dat[,c(1:20,364:374)]
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
           "Turb.NTU", "Colour_PCU")
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

subset(wq.dat,Site=="Godbout"&month%in%c(1,2)&CY==2011)
wq.dat[wq.dat$Site=="Godbout"&wq.dat$Date==date.fun("2011-02-06"),c("TP.ugL","PP.ugL")]=NA

subset(wq.dat,NH4.mgL>1)
wq.dat[wq.dat$Site=="Godbout"&wq.dat$Date==date.fun("2019-12-02"),"NH4.mgL"]=NA
## TP Reversal 
plot(TP.ugL~SRP.ugL,wq.dat);abline(0,1)
plot(DP.ugL~SRP.ugL,wq.dat);abline(0,1)

# Evaluation
wq.dat$TPReversal=with(wq.dat,ifelse(is.na(SRP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(SRP.ugL>(TP.ugL*1.3),1,0)));
sum(wq.dat$TPReversal,na.rm=T)
subset(wq.dat,TPReversal==1)
plot(TP.ugL~SRP.ugL,wq.dat,ylab="TP (\u03BCg L\u207B\u00b9)",xlab="SRP (\u03BCg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

## TN Reversal 
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

# n.species=c("TN.mgL","TKN.mgL","NH4.mgL","NOx.mgL","Urea.mgL","SolN.mgL",'DIN.mgL')
# wq.dat[wq.dat$TNReversal==1,n.species]=NA

## Calculated values
wq.dat$PP.ugL=with(wq.dat,ifelse((TP.ugL-DP.ugL)<=0,TP.ugL,TP.ugL-DP.ugL))
plot(PP.ugL~PP.calc.mgL,wq.dat);abline(0,1000)
abline(v=c(0.75,0.9))
subset(wq.dat,PP.calc.mgL>0.75&PP.calc.mgL<0.9);# calculation error in original dataset?

wq.dat$DOP.ugL=with(wq.dat,DP.ugL-SRP.ugL)
plot(DOP.ugL~DOP.calc.mgL,wq.dat);abline(0,1000)
subset(wq.dat,(DOP.ugL/(DOP.calc.mgL*1000))<1)


wq.dat$DON.calc=with(wq.dat,ifelse((SolN.mgL-DIN.mgL)<SolN.mgL,SolN.mgL,SolN.mgL-DIN))
plot(DON.mgL~DON.calc,wq.dat);abline(0,1)

wq.dat$DON.mgL=wq.dat$DON.calc

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
wq.dat$DIN_SRP=with(wq.dat,TN.mM/TP.mM)


## Only nutrients for now
head(wq.dat)
idvars=c("Date","Site","CY","CY.d","DOY","month")
paramvars=c("TP.ugL", "SRP.ugL", "DP.ugL","PP.ugL", "DOP.ugL",
            "TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL", "SolN.mgL","Urea.mgL","DON.mgL",  
            "TOC.mgL","SolOC.mgL",
            "TN_TP","TOC_TP", "TOC_TN", "DIN_SRP")
wq.dat.melt=melt(wq.dat[,c(idvars,paramvars)],id.vars = idvars)
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)
# GAM ---------------------------------------------------------------------
dat.out=subset(wq.dat,Site=="Lake_Outlet")
range(dat.out$Date)

dat.in=subset(wq.dat,Site=="Godbout")

# For prediction
pdat=expand.grid(DOY=seq(1,366,14),
                 CY=seq(2010,2020,0.25))
# TP ----------------------------------------------------------------------
## Outflow 
plot(TP.ugL~Date,dat.out)
acf(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL)
pacf(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL)
# idea from https://rpubs.com/richkt/269797
Box.test(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL);# not stationary
tseries::adf.test(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL)
tseries::kpss.test(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL)

m.TP.out=gam(log(TP.ugL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY,k=11)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(9, 3)),
             data = dat.out, method = "REML")
summary(m.TP.out)

layout(matrix(1:4,2,2));gam.check(m.TP.out,pch=21)
dev.off()

testUniformity(simulateResiduals(m.TP.out))
testDispersion(simulateResiduals(m.TP.out))
# testResiduals(simulateResiduals(m.TP.out))

draw(m.TP.out)

pred.org=predict.gam(m.TP.out,type="terms")
partial.resids.TPout<-pred.org+residuals(m.TP.out)


hist(partial.resids.TPout[,1]);shapiro.test(partial.resids.TPout[,1])
hist(partial.resids.TPout[,2]);shapiro.test(partial.resids.TPout[,2])
hist(partial.resids.TPout[,3]);shapiro.test(partial.resids.TPout[,3])

# Significant change
mod.fit=predict.gam(m.TP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

TP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(TP.out.pdat)


df.res <- df.residual(m.TP.out)
crit.t <- qt(0.025, df.res, lower.tail = FALSE)

TP.out.pdat <- transform(TP.out.pdat,
                  upper.DOY = fit.DOY + (crit.t * SE.DOY),
                  lower.DOY = fit.DOY - (crit.t * SE.DOY),
                  upper.CY = fit.CY + (crit.t * SE.CY),
                  lower.CY = fit.CY - (crit.t * SE.CY),
                  upper.DOYCY = fit.DOYCY + (crit.t * SE.DOYCY),
                  lower.DOYCY = fit.DOYCY - (crit.t * SE.DOYCY))

m.CY.d <- derivatives(m.TP.out,newdata=TP.out.pdat,
                      term='s(CY)',type = "central",interval="confidence",ncores=12)

m.mod.CY.dsig <- signifD(TP.out.pdat$fit.CY,
                         d=m.CY.d$derivative,
                         TP.out.pdat$upper.CY,TP.out.pdat$lower.CY)
TP.out.pdat$dsig.CY.incr=unlist(m.mod.CY.dsig$incr)
TP.out.pdat$dsig.CY.decr=unlist(m.mod.CY.dsig$decr)
unique(subset(TP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TP.out.pdat,is.na(dsig.CY.decr)==F)$CY)

m.DOY.d <- derivatives(m.TP.out,newdata=TP.out.pdat,
                         term='s(DOY)',type = "central",interval="confidence",ncores=12)
m.mod.DOY.dsig <- signifD(TP.out.pdat$fit.DOY,
                            d=m.DOY.d$derivative,
                          TP.out.pdat$upper.DOY,TP.out.pdat$lower.DOY)
TP.out.pdat$dsig.DOY.incr=unlist(m.mod.DOY.dsig$incr)
TP.out.pdat$dsig.DOY.decr=unlist(m.mod.DOY.dsig$decr)
unique(subset(TP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)

## Significant change funtion
TP.out.pdat2=GAM.SigChange.fun(m.TP.out,TP.out.pdat)
unique(subset(TP.out.pdat2,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TP.out.pdat2,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TP.out.pdat2,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TP.out.pdat2,is.na(dsig.DOY.decr)==F)$DOY)


TP.out.DOY.sig=ddply(TP.out.pdat2,c('DOY'),summarise,
                fit=mean(fit.DOY),
                UCI=mean(upper.DOY,na.rm=T),
                LCI=mean(lower.DOY,na.rm=T),
                dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                dsig.decre=mean(dsig.DOY.decr,na.rm=T))


# png(filename=paste0(plot.path,"PLSF_GAM_weeklyTP_out.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,1,1.5),oma=c(1,1.5,0.25,0.25));
layout(matrix(c(1:4),2,2,byrow = T))

ylim.val=c(-1.5,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.out$model$DOY,partial.resids.TPout[,1],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(month.sig,shaded.range(month,LCI,UCI,"grey",lty=1))
lines(fit~DOY,TP.out.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(DOY)")
mtext(side=1,line=2,"DOY")
mtext(side=2,line=2,"Effect")

ylim.val=c(-1.5,1.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.out$model$CY,partial.resids.TPout[,2],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(pdat,shaded.range(CY,lower.CY,upper.CY,"grey",lty=1))
lines(fit.CY~CY,TP.out.pdat,lwd=2)
lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Lake Outlet - TP",font=3)
mtext(side=3,adj=0,"s(Year)")
mtext(side=1,line=2,"Year")

ylim.val=c(1,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma1=with(TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TP.out.pdat$CY)),nrow=length(unique(TP.out.pdat$DOY))))
brk=10
breaks.val=classInt::classIntervals(TP.out.pdat$fit.DOYCY,style="equal",n=brk)
pal=hcl.colors(n=brk,alpha=0.75)
image(x=unique(TP.out.pdat$CY),y=unique(TP.out.pdat$DOY),z=t(tmp.ma1),
      breaks=breaks.val$brks,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=unique(TP.out.pdat$CY),y=unique(TP.out.pdat$DOY),z=t(tmp.ma1),
        levels=breaks.val$brks,nlevels=brk,
        add=T,drawlabels=F,lwd=1.25,col="white")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"ti(DOY,Year)")
mtext(side=1,line=2.25,"Year")
mtext(side=2,line=3,"DOY")

legend_image=as.raster(matrix(rev(pal),ncol=1))
par(xpd=NA,mar=c(2,1,1,0))
plot(c(0,1),c(0,1),type = 'n', axes = F,ann=F)
rasterImage(legend_image, 0, 0.25, 0.3,0.75)
text(x=0.3, y = seq(0.25,0.75,length.out=2), labels = format(round(range(breaks.val$brks),2)),cex=1,pos=4)
text(0.15,0.76,"Effect",pos=3,xpd=NA)

legend(0.8,0.8,legend=c("Fitted","95% CI","Significant Change\n(Increasing)","Significant Change\n(Decreasing)","Partial Residual"),
       lty=c(1,2,1,1,0),col=c("black","black","red","blue",adjustcolor("grey",0.25)),
       pch=c(NA,NA,NA,NA,21),pt.bg=c(NA,NA,NA,NA,adjustcolor("grey",0.25)),
       pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()

# as_flextable(m.TP.out)%>%print("docx")

## Inflow
plot(TP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL)
pacf(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL)
lmtest::bgtest(TP.ugL~CY.d,data=dat.in)
# idea from https://rpubs.com/richkt/269797
Box.test(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL);# not stationary
tseries::adf.test(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL)
tseries::kpss.test(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL)




m.TP.in=gam(log(TP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=11)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(8, 3)),
             data = dat.in, method = "REML")
summary(m.TP.in)

layout(matrix(1:4,2,2));gam.check(m.TP.in,pch=21)
dev.off()

testUniformity(simulateResiduals(m.TP.in))
testDispersion(simulateResiduals(m.TP.in))
# testResiduals(simulateResiduals(m.TP.out))
plot(residuals(simulateResiduals(m.TP.in)))
plotQQunif(simulateResiduals(m.TP.in)) # left plot in plot.DHARMa()
plotResiduals(simulateResiduals(m.TP.in)) # right plot in plot.DHARMa()

draw(m.TP.in)

pred.org=predict.gam(m.TP.in,type="terms")
partial.resids.TPin<-pred.org+residuals(m.TP.in)

mod.fit=predict.gam(m.TP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

TP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(TP.in.pdat)

# low model fit and skewed residuals - no significant change test
TP.in.pdat=GAM.SigChange.fun(m.TP.in,TP.in.pdat)
unique(subset(TP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TP.in.DOY.sig=ddply(TP.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))


# png(filename=paste0(plot.path,"PLSF_GAM_weeklyTP_in.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,1,1.5),oma=c(1,1.5,0.25,0.25));
layout(matrix(c(1:4),2,2,byrow = T))

ylim.val=c(-2,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.in$model$DOY,partial.resids.TPin[,1],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(month.sig,shaded.range(month,LCI,UCI,"grey",lty=1))
lines(fit~DOY,TP.in.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
# lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
# lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(DOY)")
mtext(side=1,line=2,"DOY")
mtext(side=2,line=2,"Effect")

ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.in$model$CY,partial.resids.TPin[,2],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(pdat,shaded.range(CY,lower.CY,upper.CY,"grey",lty=1))
lines(fit.CY~CY,TP.in.pdat,lwd=2)
lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
# lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
# lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Lake inlet - TP",font=3)
mtext(side=3,adj=0,"s(Year)")
mtext(side=1,line=2,"Year")

ylim.val=c(1,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma1=with(TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TP.in.pdat$CY)),nrow=length(unique(TP.in.pdat$DOY))))
brk=10
breaks.val=classInt::classIntervals(TP.in.pdat$fit.DOYCY,style="equal",n=brk)
pal=hcl.colors(n=brk,alpha=0.75)
image(x=unique(TP.in.pdat$CY),y=unique(TP.in.pdat$DOY),z=t(tmp.ma1),
      breaks=breaks.val$brks,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=unique(TP.in.pdat$CY),y=unique(TP.in.pdat$DOY),z=t(tmp.ma1),
        levels=breaks.val$brks,nlevels=brk,
        add=T,drawlabels=F,lwd=1.25,col="white")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"ti(DOY,Year)")
mtext(side=1,line=2.25,"Year")
mtext(side=2,line=3,"DOY")

legend_image=as.raster(matrix(rev(pal),ncol=1))
par(xpd=NA,mar=c(2,1,1,0))
plot(c(0,1),c(0,1),type = 'n', axes = F,ann=F)
rasterImage(legend_image, 0, 0.25, 0.3,0.75)
text(x=0.3, y = seq(0.25,0.75,length.out=2), labels = format(round(range(breaks.val$brks),2)),cex=1,pos=4)
text(0.15,0.76,"Effect",pos=3,xpd=NA)

legend(0.8,0.8,legend=c("Fitted","95% CI","Significant Change\n(Increasing)","Significant Change\n(Decreasing)","Partial Residual"),
       lty=c(1,2,1,1,0),col=c("black","black","red","blue",adjustcolor("grey",0.25)),
       pch=c(NA,NA,NA,NA,21),pt.bg=c(NA,NA,NA,NA,adjustcolor("grey",0.25)),
       pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()


# as_flextable(m.TP.in)%>%print("docx")


# SRP ---------------------------------------------------------------------
# Outflow
plot(SRP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(SRP.ugL)==F)$SRP.ugL)
pacf(subset(dat.out,is.na(SRP.ugL)==F)$SRP.ugL)
lmtest::bgtest(SRP.ugL~CY.d,data=dat.out)
# idea from https://rpubs.com/richkt/269797
Box.test(subset(dat.out,is.na(SRP.ugL)==F)$TP.ugL);# not stationary
tseries::adf.test(subset(dat.out,is.na(SRP.ugL)==F)$SRP.ugL)
tseries::kpss.test(subset(dat.out,is.na(SRP.ugL)==F)$SRP.ugL)

m.SRP.out=gam(log(SRP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=10)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(25, 10)),
            data = dat.out, method = "REML")
summary(m.SRP.out)
draw(m.SRP.out)
layout(matrix(1:4,2,2));gam.check(m.SRP.out,pch=21)
dev.off()

testUniformity(simulateResiduals(m.SRP.out))
acf(m.SRP.out$residuals)

testDispersion(simulateResiduals(m.SRP.out))
# testResiduals(simulateResiduals(m.SRP.out))
plot(residuals(simulateResiduals(m.SRP.out)))
plotQQunif(simulateResiduals(m.SRP.out)) # left plot in plot.DHARMa()
plotResiduals(simulateResiduals(m.SRP.out)) # right plot in plot.DHARMa()

pred.org=predict.gam(m.SRP.out,type="terms")
partial.resids.SRPout<-pred.org+residuals(m.SRP.out)

mod.fit=predict.gam(m.SRP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

SRP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(SRP.out.pdat)

# low model fit and skewed residuals - no significant change test
SRP.out.pdat=GAM.SigChange.fun(m.SRP.out,SRP.out.pdat)
unique(subset(SRP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(SRP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(SRP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(SRP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
SRP.out.DOY.sig=ddply(SRP.out.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

ylim.val=c(-3,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.SRP.out$model$DOY,partial.resids.SRPout[,1],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(month.sig,shaded.range(month,LCI,UCI,"grey",lty=1))
lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(DOY)")
mtext(side=1,line=2,"DOY")
mtext(side=2,line=2,"Effect")

# Inflow
plot(SRP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(SRP.ugL)==F)$SRP.ugL)
pacf(subset(dat.in,is.na(SRP.ugL)==F)$SRP.ugL)
lmtest::bgtest(SRP.ugL~CY.d,data=dat.in)
# idea from https://rpubs.com/richkt/269797
Box.test(subset(dat.in,is.na(SRP.ugL)==F)$TP.ugL);# not stationary
tseries::adf.test(subset(dat.in,is.na(SRP.ugL)==F)$SRP.ugL)
tseries::kpss.test(subset(dat.in,is.na(SRP.ugL)==F)$SRP.ugL)

m.SRP.in=gam(log(SRP.ugL)~
                s(DOY, bs = "cc",k=15) + 
                s(CY,k=4)+
                ti(DOY,CY,bs = c('cc', 'tp'), k = c(4, 3)),
              data = dat.in, method = "REML")
summary(m.SRP.in)
layout(matrix(1:4,2,2));gam.check(m.SRP.in,pch=21)
dev.off()
draw(m.SRP.in)
testUniformity(simulateResiduals(m.SRP.in))

pred.org=predict.gam(m.SRP.in,type="terms")
partial.resids.SRPout<-pred.org+residuals(m.SRP.in)

mod.fit=predict.gam(m.SRP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

SRP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(SRP.in.pdat)

SRP.in.pdat=GAM.SigChange.fun(m.SRP.in,SRP.in.pdat)
unique(subset(SRP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(SRP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(SRP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(SRP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
SRP.in.DOY.sig=ddply(SRP.in.pdat,c('DOY'),summarise,
                      fit=mean(fit.DOY),
                      UCI=mean(upper.DOY,na.rm=T),
                      LCI=mean(lower.DOY,na.rm=T),
                      dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                      dsig.decre=mean(dsig.DOY.decr,na.rm=T))


# PP ----------------------------------------------------------------------
# Outflow
plot(PP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(PP.ugL)==F)$PP.ugL)
pacf(subset(dat.out,is.na(PP.ugL)==F)$PP.ugL)

m.PP.out=gam(log(PP.ugL)~
                s(DOY, bs = "cc",k=15) + 
                s(CY,k=10)+
                ti(DOY,CY,bs = c('cc', 'tp'), k = c(25, 11)),
              data = dat.out, method = "REML")
summary(m.PP.out)

layout(matrix(1:4,2,2));gam.check(m.PP.out,pch=21)
dev.off()
draw(m.PP.out)

testUniformity(simulateResiduals(m.PP.out))
# acf(m.PP.out$residuals)

pred.org=predict.gam(m.PP.out,type="terms")
partial.resids.PPout<-pred.org+residuals(m.PP.out)

mod.fit=predict.gam(m.PP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

PP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(PP.out.pdat)

PP.out.pdat=GAM.SigChange.fun(m.PP.out,PP.out.pdat)
unique(subset(PP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(PP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(PP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(PP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
PP.out.DOY.sig=ddply(PP.out.pdat,c('DOY'),summarise,
                      fit=mean(fit.DOY),
                      UCI=mean(upper.DOY,na.rm=T),
                      LCI=mean(lower.DOY,na.rm=T),
                      dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                      dsig.decre=mean(dsig.DOY.decr,na.rm=T))

# Outflow
plot(PP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(PP.ugL)==F)$PP.ugL)
pacf(subset(dat.in,is.na(PP.ugL)==F)$PP.ugL)
lmtest::bgtest(PP.ugL~CY.d,data=dat.in)

m.PP.in=gam(log(PP.ugL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(4, 3)),
             data = dat.in, method = "REML")
summary(m.PP.in)

layout(matrix(1:4,2,2));gam.check(m.PP.in,pch=21)
dev.off()
draw(m.PP.in)
testUniformity(simulateResiduals(m.PP.in))

pred.org=predict.gam(m.PP.in,type="terms")
partial.resids.PPin<-pred.org+residuals(m.PP.in)

mod.fit=predict.gam(m.PP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

PP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(PP.in.pdat)

PP.in.pdat=GAM.SigChange.fun(m.PP.in,PP.in.pdat)
unique(subset(PP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(PP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(PP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(PP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
PP.in.DOY.sig=ddply(PP.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))



# DP ----------------------------------------------------------------------
# Outflow
plot(DP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(DP.ugL)==F)$DP.ugL)
pacf(subset(dat.out,is.na(DP.ugL)==F)$DP.ugL)
lmtest::bgtest(DP.ugL~CY.d,data=dat.out)

m.DP.out=gam(log(DP.ugL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY,k=3)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(17, 11)),
             data = dat.out, method = "REML")
summary(m.DP.out)

layout(matrix(1:4,2,2));gam.check(m.DP.out,pch=21)
dev.off()
draw(m.DP.out)


# GAM plots ---------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weeklyT_combo.png"),width=12,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:6,1,6,byrow = T))

ylim.val=c(-2,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.in$model$DOY,partial.resids.TPin[,1],pch=19,col=adjustcolor("grey20",0.5))
lines(fit~DOY,TP.in.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
# lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
# lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(DOY)")
mtext(side=1,line=2,"DOY")mtext(side=2,line=2,"Effect")

ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.in$model$CY,partial.resids.TPin[,2],pch=19,col=adjustcolor("grey20",0.5))
lines(fit.CY~CY,TP.in.pdat,lwd=2)
lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
# lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
# lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(Year)")
mtext(side=1,line=2,"Year")
mtext(side=3,line=1,"Inflow",cex=1,font=2)

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2008-12-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(TP.ugL~Date,dat.in,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(dat.in,pt_line(Date,TP.ugL,2,"grey",1,21,"grey"))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5);
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"Date")
mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)")

ylim.val=c(-1.5,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.out$model$DOY,partial.resids.TPout[,1],pch=19,col=adjustcolor("grey",0.5))
lines(fit~DOY,TP.out.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(DOY)")
mtext(side=1,line=2,"DOY")


ylim.val=c(-1.5,1.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.out$model$CY,partial.resids.TPout[,2],pch=19,col=adjustcolor("grey",0.5))
lines(fit.CY~CY,TP.out.pdat,lwd=2)
lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(Year)")
mtext(side=1,line=2,"Year")
mtext(side=3,line=1,"Outflow",cex=1,font=2)

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2008-12-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(TP.ugL~Date,dat.out,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(dat.out,pt_line(Date,TP.ugL,2,"grey20",1,21,"grey20"))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5);
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"Date")
mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)")
dev.off()

# Trend Analyses ----------------------------------------------------------
month.mean.dat=ddply(wq.dat.melt,c("Site","month","CY","variable"),summarise,
                     mean.value=mean(value,na.rm=T),
                     SE.value=SE(value),
                     N.val=N.obs(value))

## Experimenting
tmp=subset(wq.dat.melt,Site=="Lake_Outlet"&variable=="TP.ugL")
mmkh.rslt=mmkh(tmp$value)
data.frame(Z_correct=as.numeric(mmkh.rslt[1]),
           new_pvalue=as.numeric(mmkh.rslt[2]),
           Z_org=as.numeric(mmkh.rslt[4]),
           org_pvalue=as.numeric(mmkh.rslt[5]),
           tau=as.numeric(mmkh.rslt[6]),
           sen_slope=as.numeric(mmkh.rslt[7]))

mmkh.trend=ddply(wq.dat.melt,c("Site","variable"),summarise,
            Z_correct=as.numeric(mmkh(value)[1]),
            new_pvalue=as.numeric(mmkh(value)[2]),
            Z_org=as.numeric(mmkh(value)[4]),
            org_pvalue=as.numeric(mmkh(value)[5]),
            tau=as.numeric(mmkh(value)[6]),
            sen_slope=as.numeric(mmkh(value)[7]))

pt.rslt=pettitt.test(tmp$TP.ugL);pt.rslt
tmp[pt.rslt$estimate,]
data.frame(stat=as.numeric(pt.rslt$statistic),
           pvalue=as.numeric(pt.rslt$p.value),
           chge_row=as.numeric(pt.rslt$estimate))

sites.vals=c("Godbout","Lake_Outlet")
paramvars
pt.rslt.all=data.frame()
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat.melt,Site==sites.vals[i])
  for(j in 1:length(paramvars)){
    tmp.dat2=subset(tmp.dat,variable==paramvars[j])
    if(nrow(tmp.dat2)==0){next}
    pt=pettitt.test(tmp.dat2$value)
    date.change=if(as.numeric(pt$p.value)<0.05){tmp.dat2[pt$estimate,"CY.d"]}else{NA}
    rslt=data.frame(Site=sites.vals[i],param=paramvars[j],
      stat=as.numeric(pt$statistic),
               pvalue=as.numeric(pt$p.value),
               chge_row=as.numeric(pt$estimate),
               date.change=date.change)
    pt.rslt.all=rbind(pt.rslt.all,rslt)
  }
}
pt.rslt.all

trend.rslt.all=data.frame()
for(i in 1:length(sites.vals)){
  tmp.dat=subset(month.mean.dat,Site==sites.vals[i])
  for(j in 1:length(paramvars)){
    tmp.dat2=subset(tmp.dat,variable==paramvars[j])
    if(nrow(tmp.dat2)==0){next}
    trend.test=with(tmp.dat2,kendallSeasonalTrendTest(mean.value,month,CY))
    rslt=data.frame(Site=sites.vals[i],param=paramvars[j],
                    N.val=as.numeric(trend.test$sample.size["Total"]),
                    chisq.stat=as.numeric(trend.test$statistic[1]),
                    chisq.pval=as.numeric(trend.test$p.value[1]),
                    z.stat=as.numeric(trend.test$statistic[2]),
                    tau=as.numeric(trend.test$estimate[1]),
                    z.pval=as.numeric(trend.test$p.value[2]),
                    sen.slope=as.numeric(trend.test$estimate[2]),
                    sen.int=as.numeric(trend.test$estimate[3])
                    )
    trend.rslt.all=rbind(trend.rslt.all,rslt)
  }
}
trend.rslt.all

pt.trend.all=merge(trend.rslt.all,pt.rslt.all,c('Site',"param"))
pt.trend.all$date.change2=format(lubridate::date_decimal(pt.trend.all$date.change),"%b %d,%Y")

vars=c("TP.ugL","SRP.ugL","DP.ugL","PP.ugL","DOP.ugL","TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL")
pt.trend.all=subset(pt.trend.all,param%in%vars)
pt.trend.all$Site=factor(pt.trend.all$Site,levels=c("Godbout","Lake_Outlet"))
pt.trend.all$param=factor(pt.trend.all$param,levels=vars)
pt.trend.all=pt.trend.all[order(pt.trend.all$param,pt.trend.all$Site),]

vars=c("Site", "param", "N.val",
       # "chisq.stat", "chisq.pval", 
       "tau", "z.pval", "sen.slope",
       "stat","pvalue","date.change2")
pt.trend.all[,vars]%>%
  flextable()%>%
  # colformat_double(j="chisq.stat",digits=1)%>%
  # colformat_double(j="chisq.pval",i=~chisq.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  # compose(j="chisq.pval",i=~chisq.pval<0.05,value=as_paragraph('< 0.05'))%>%
  colformat_double(j="tau",digits=2)%>%
  colformat_double(j="sen.slope",digits=3)%>%
  colformat_double(j="z.pval",i=~z.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="z.pval",i=~z.pval<0.05,value=as_paragraph('< 0.05'))%>%
  colformat_double(j="stat",digits=0,big.mark="")%>%
  italic(j="z.pval",i=~z.pval<0.05)%>%
  colformat_double(j="pvalue",i=~pvalue>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  colformat_char(j="date.change2",na_str=" --- ")%>%
  italic(j="pvalue",i=~pvalue<0.05)%>%
  compose(j="pvalue",i=~pvalue<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="param",i=~param=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="DP.ugL",value=as_paragraph('DP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="PP.ugL",value=as_paragraph('PP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="DOP.ugL",value=as_paragraph('DOP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="TN.mgL",value=as_paragraph('TN (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="NOx.mgL",value=as_paragraph('NO\u2093 (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="NH4.mgL",value=as_paragraph('NH\u2084 (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="DIN.mgL",value=as_paragraph('DIN (mg L\u207B\u00B9)'))%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Outlet'))%>%
  merge_v(j="param")%>%
  set_header_labels("Site"="Site",
                    "param" = "Parameter",
                    "N.val" = "Sample\nSize",
                    "tau" = "Kendall's \u03C4",
                    "z.pval" = "\u03C1-value",
                    "sen.slope" = "Thiel-Sen Slope",
                    "stat"="Pettitt's\nStatistic",
                    "pvalue"="\u03C1-value",
                    "date.change2"="Date\nChange Point")%>%
  width(width=c(0.5,1.25,0.75,1,0.75,0.75,0.75,0.75,1))%>%
  align(align="center",part="header")%>%
  align(j=2:9,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  hline(i=seq(2,16,2))%>%
  fix_border_issues()%>%
  footnote(j=3:6,ref_symbols = " 1 ",part="header",
           value=as_paragraph("Seasonal Mann-Kendall using month as season"))%>%
  footnote(j=7:9,ref_symbols = " 2 ",part="header",
           value=as_paragraph("Pettitt Change point analysis performed using weekly samples"))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")%>%print("docx")

  
  
  

## flextable output

month.mean.dat$monCY=with(month.mean.dat,date.fun(paste(CY,month,"01",sep="-")))
month.mean.dat$CY.d=lubridate::decimal_date(month.mean.dat$monCY)

sea.ken.TPout=with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"),
                   kendallSeasonalTrendTest(mean.value,month,CY))
print(sea.ken.TPout)
sea.ken.TPin=with(subset(month.mean.dat,Site=="Godbout"&variable=="TP.ugL"),
                  kendallSeasonalTrendTest(mean.value,month,CY))
print(sea.ken.TPin)

plot(mean.value~monCY,subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"))
trend_mblm=mblm::mblm(mean.value~CY.d,subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"))
x.val=with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"),seq(min(CY.d),max(CY.d),length.out=10))
trend_mblm.pred=predict.lm(trend_mblm,newdata=data.frame(CY.d=x.val),interval="confidence")
lines(date.fun(lubridate::date_decimal(x.val)),trend_mblm.pred[,1],col="red")
lines(date.fun(lubridate::date_decimal(x.val)),trend_mblm.pred[,2],lty=2)
lines(date.fun(lubridate::date_decimal(x.val)),trend_mblm.pred[,3],lty=2)

sea.trend=sea.ken.TPout$estimate[2]*x.val+sea.ken.TPout$estimate[3]
lines(date.fun(lubridate::date_decimal(x.val)),sea.trend,col="blue")

plot(mean.value~CY,subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"&month==1))
trend_mblm=mblm::mblm(mean.value~CY.d,subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"&month==1))
x.val=with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"),seq(min(CY.d),max(CY.d),length.out=10))
trend_mblm.pred=predict.lm(trend_mblm,newdata=data.frame(CY.d=x.val),interval="confidence")
lines(x.val,trend_mblm.pred[,1],col="red")

sea.trend=sea.ken.TPout$seasonal.estimates[1,2]*x.val+sea.ken.TPout$seasonal.estimates[1,3]
lines(x.val,sea.trend,col="blue")


## seasonal plots
paramvars
month.mean.dat=merge(month.mean.dat,
                     expand.grid(Site=unique(month.mean.dat$Site),
                                 variable=unique(month.mean.dat$variable),
                                 CY=2009:2020,month=1:12),
                     c("Site","variable","CY","month"),all.y=T)
subset(month.mean.dat,variable=="Urea.mgL"&is.na(mean.value)==F)

## Phosphorus
# png(filename=paste0(plot.path,"PLSF_PSeasonal.png"),width=12,height=8.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(2,3,1,0.25));
layout(matrix(c(1:60),5,12,byrow = T))
xlim.val=c(2009,2021);by.x=7;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="TP.ugL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  # axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)")}
  mtext(side=3,month.abb[i])
  if(i==1){
    legend("topleft",legend=c("Inflow","Ouflow"),
           lty=0,lwd=0.1,col=c("black","black"),
           pch=21,pt.bg=c("black","grey"),
           pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
  }
}
ylim.val=c(0,250);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="DP.ugL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="DP.ugL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"DP (\u03BCg L\u207B\u00B9)")}
}
ylim.val=c(0,600);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="PP.ugL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="PP.ugL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"PP (\u03BCg L\u207B\u00B9)")}
}
ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="SRP.ugL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="SRP.ugL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"SRP (\u03BCg L\u207B\u00B9)")}
}
ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="DOP.ugL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="DOP.ugL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"DOP (\u03BCg L\u207B\u00B9)")}
}
mtext(side=1,line=0.5,outer=T,"Year")
dev.off()

## Nitrogen
# png(filename=paste0(plot.path,"PLSF_NSeasonal.png"),width=12,height=8.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(2,3,1,0.25));
layout(matrix(c(1:60),5,12,byrow = T))
xlim.val=c(2009,2021);by.x=7;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

ylim.val=c(0,8);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="TN.mgL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TN.mgL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  # axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"TN (mg L\u207B\u00B9)")}
  mtext(side=3,month.abb[i])
  if(i==1){
    legend("topleft",legend=c("Inflow","Ouflow"),
           lty=0,lwd=0.1,col=c("black","black"),
           pch=21,pt.bg=c("black","grey"),
           pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
  }
}
ylim.val=c(0,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="NOx.mgL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="NOx.mgL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"NO\u2093 (mg L\u207B\u00B9)")}
}
ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="NH4.mgL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="NH4.mgL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"NH\u2084 (mg L\u207B\u00B9)")}
}
# ylim.val=c(0,0.1);by.y=0.05;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# for(i in 1:12){
#   plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
#   abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
#   with(subset(month.mean.dat,Site=="Godbout"&variable=="Urea.mgL"&month==i),
#        pt_line(CY,mean.value,2,"black",1,21,"black"))
#   with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="Urea.mgL"&month==i),
#        pt_line(CY,mean.value,2,"grey",1,21,"grey"))
#   if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
#   axis_fun(1,xmaj,xmin,NA,line=-0.5)
#   box(lwd=1)
#   if(i==1){mtext(side=2,line=2.5,"Urea (mg L\u207B\u00B9)")}
# }
ylim.val=c(0,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="SolN.mgL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="SolN.mgL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"Sol. N (mg L\u207B\u00B9)")}
}
ylim.val=c(0,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:12){
  plot(mean.value~CY,month.mean.dat,type="n",axes=F,ann=F,ylim=ylim.val,xlim=xlim.val)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(subset(month.mean.dat,Site=="Godbout"&variable=="DON.mgL"&month==i),
       pt_line(CY,mean.value,2,"black",1,21,"black"))
  with(subset(month.mean.dat,Site=="Lake_Outlet"&variable=="DON.mgL"&month==i),
       pt_line(CY,mean.value,2,"grey",1,21,"grey"))
  if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"DON (mg L\u207B\u00B9)")}
}

mtext(side=1,line=0.5,outer=T,"Year")
dev.off()



# Seasonal Comparison -----------------------------------------------------

wq.dat.melt$bloom.sea=with(wq.dat.melt,ifelse(month%in%c(8:10),"Bloom",'NoBloom'))
wq.dat.melt$ice.sea=with(wq.dat.melt,ifelse(month%in%c(11:12,1:5),"Ice",'NoIce'))

boxplot(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"),outline=F)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"))
boxplot(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"),outline=F)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))

boxplot(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"),outline=F)
kruskal.test(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"))
boxplot(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"),outline=F)
kruskal.test(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))


# png(filename=paste0(plot.path,"PLSF_SeaComp_example.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(3,3,1,0.25));
layout(matrix(c(1:4),1,4,byrow = T))

ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"),
        outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"))
mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)")
mtext(side=3,"Lake Inlet")

boxplot(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"),
        outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))
mtext(side=3,"Lake Outlet")

boxplot(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"),
        outline=F,ylim=ylim.val,col=c("sky blue","white"),axes=F,ann=F)
axis_fun(1,1:2,1:2,c('Ice\n(Nov - May)',"No Ice\n(Apr - Oct)"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"))
mtext(side=3,"Lake Inlet")

boxplot(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"),
        outline=F,ylim=ylim.val,col=c("sky blue","white"),axes=F,ann=F)
axis_fun(1,1:2,1:2,c('Ice\n(Nov - May)',"No Ice\n(Apr - Oct)"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))
mtext(side=3,"Lake Outlet")
mtext(side=1,line=2,outer=T,"Seasons")
dev.off()