## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Version 3 after explore and streamlining process
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

# load(paste0(export.path,"PLSF_1/PLSF_GAM.RData"))

# period of change -------------------------------------------------------
## For GAMs
## see https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
# tmpf <- tempfile()
# download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
#               tmpf)
# source(tmpf)
source(paste0(wd,"/src/Deriv.R"))

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
gam.sum.table=function(mod,param){
  yval=as.character(formula(mod))[2]
  yval=strsplit(yval,split='(\\()')[[1]]
  trans=if(length(yval)>1){yval[1]}else{yval}
  sum.mod=summary(mod)
  data.frame(param=param,
             R2=sum.mod$r.sq,
             Dev=sum.mod$dev.expl,
             inter.est=sum.mod$p.table[1],
             inter.tval=sum.mod$p.table[3],
             inter.pval=sum.mod$p.table[4],
             DOY.edf=sum.mod$s.table[1,1],
             DOY.Fval=sum.mod$s.table[1,3],
             DOY.pval=sum.mod$s.table[1,4],
             CY.edf=sum.mod$s.table[2,1],
             CY.Fval=sum.mod$s.table[2,3],
             CY.pval=sum.mod$s.table[2,4],
             DOYCY.edf=sum.mod$s.table[3,1],
             DOYCY.Fval=sum.mod$s.table[3,3],
             DOYCY.pval=sum.mod$s.table[3,4],
             method=sum.mod$method,
             method.sp=as.numeric(sum.mod$sp.criterion[1]),
             scale.est=sum.mod$scale,
             N.value=sum.mod$n,
             res.skew=e1071::skewness(residuals(mod)),
             trans=trans)
}
# GAM model fit explaination...https://stats.stackexchange.com/a/191235/83845
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

# 100.1/40.1 Ca Hardness
# 100.1/24.3 Mg Hardness
# Hardness = 2.497 (Ca) + 4.119 (Mg)
# <60mg/L is soft water; 60 - 120 mod hard; 120 - 180 Hard; >180 vhard
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


## Double checking the 62-day dataset
# test=subset(wq.dat,Site=="In_Lake"&Date%in%seq(date.fun("2019-04-30"),date.fun("2019-10-29"),"1 days"))
# plot(TP.ugL~Date,test)
# plot(SRP.ugL~Date,test)
# 
# nrow(test[,c("Date","SRP.ugL")])
# nrow(dat.xtab2[,c("Date","SRP.ugL")])
# 
# test2=merge(test[,c("Date","SRP.ugL")],dat.xtab2[,c("Date","SRP.ugL")],"Date",all.x=T)
# 
# plot(SRP.ugL.x~SRP.ugL.y,test2);abline(0,1)


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
wq.dat$DIN_TP=with(wq.dat,DIN.mM/TP.mM)

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

## Carlson Trophic Index --------------------------------------------------
wq.dat$TSI_sd=with(wq.dat,60-14.41*log(Secchi_cm/100))
# wq.dat$TSI_chla=with(wq.dat,9.81*log(Chla.ugL*1000)+30.6)
wq.dat$TSI_chl=with(wq.dat,9.81*log(TChl.ugL)+30.6)
wq.dat$TSI_TP=with(wq.dat,14.42*log(TP.ugL)+4.15)
wq.dat$TSI_TN=with(wq.dat,54.45+14.43*log(TN.mgL))

wq.dat$TSI_mean=rowMeans(wq.dat[,c("TSI_chl","TSI_TP","TSI_TN")],na.rm=T)
wq.dat$TSI_mean2=rowMeans(wq.dat[,c("TSI_TP","TSI_TN")],na.rm=T)

plot(TSI_TP~TSI_TN,wq.dat);abline(0,1)
abline(h=c(30,40,60,70),v=c(30,40,60,70),lty=2)
###
head(wq.dat)
wq.dat[wq.dat$CY<2016,c("pH","Phyco.ugL")]=NA

idvars=c("Date","Site","CY","CY.d","DOY","month")
paramvars=c("TP.ugL", "SRP.ugL", "DP.ugL","PP.ugL", "DOP.ugL",
            "TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL", "SolN.mgL","Urea.mgL","DON.mgL", "TON.mgL", "TKN.mgL",
            "TOC.mgL","SolOC.mgL",
            "TN_TP","TOC_TP", "TOC_TN", "DIN_SRP","DIN_TP","PP_TP","SRP_TP","DP_TP","NH4_TN","NOx_TN",
            "Chla.ugL","Cond","DO.per","Colour_PCU","Turb.NTU","Secchi_cm",
            "Temp.C","pH","Phyco.ugL", "TChl.ugL",'TSI_mean2')
wq.dat.melt=melt(wq.dat[,c(idvars,paramvars)],id.vars = idvars)
wq.dat.melt=subset(wq.dat.melt,is.na(value)==F)
unique(wq.dat.melt$variable)
head(subset(wq.dat.melt,variable=="Cond"))
head(subset(wq.dat.melt,variable=="TSI_mean2"))


## Export to EDI
idvars=c("Date","Site")
paramvars=c("TP.ugL", "SRP.ugL", "DP.ugL","PP.ugL", "DOP.ugL",
            "TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL", "SolN.mgL","Urea.mgL","DON.mgL", "TON.mgL", "TKN.mgL",
            "TOC.mgL","SolOC.mgL",
            "Cond","DO.per","Colour_PCU","Turb.NTU","Secchi_cm",
            "Temp.C","pH","Phyco.ugL", "TChl.ugL","TN_TP", "DIN_SRP")
tmp=wq.dat[,c(idvars,paramvars)]
tmp$Site=with(tmp,ifelse(Site=="Godbout",'Lake_Inlet',Site))
unique(tmp$Site)

tmp$scn=rowSums(is.na(tmp[,3:ncol(tmp)]))
tmp=subset(tmp,scn<27)
range(tmp$Date)
summary(tmp)
# tmp2=melt(tmp,id.vars = idvars)
# tmp2=subset(tmp2,is.na(value)==F)
# # test=ddply(tmp2,c("Date",'Site',"variable"),summarise,N.val=N.obs(value))
# tmp2=dcast(tmp2,Site+Date~variable,value.var="value",mean)
# tmp=subset(tmp,Site%in%c("Lake_Inlet","Lake_Outlet"))

# write.csv(tmp[,c(idvars,paramvars)],paste0(export.path,"PLSF_DailyWQ.csv"),row.names = F)

# GAM ---------------------------------------------------------------------
dat.out=subset(wq.dat,Site=="Lake_Outlet")
range(dat.out$Date)

dat.in=subset(wq.dat,Site=="Godbout")

# For prediction
pdat=expand.grid(DOY=seq(1,366,14),
                 CY=seq(2010,2020,0.25))


## Inflow models ----------------------------------------------------------
# TP
plot(TP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL)
pacf(subset(dat.in,is.na(TP.ugL)==F)$TP.ugL)
lmtest::bgtest(TP.ugL~CY.d,data=dat.in)

m.TP.in=gam(log(TP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=9)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(20,5)),
            data = dat.in, method = "REML")
summary(m.TP.in)

layout(matrix(1:4,2,2));gam.check(m.TP.in,pch=21)
dev.off()
draw(m.TP.in)
testUniformity(simulateResiduals(m.TP.in))
acf(m.TP.in$residuals)


pred.org=predict.gam(m.TP.in,type="terms")
partial.resids.TP.in<-pred.org+residuals(m.TP.in)

mod.fit=predict.gam(m.TP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

TP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(TP.in.pdat)

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

gam.sum.table(m.TP.in,"TP")

# SRP
plot(SRP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(SRP.ugL)==F)$SRP.ugL)
pacf(subset(dat.in,is.na(SRP.ugL)==F)$SRP.ugL)
lmtest::bgtest(SRP.ugL~CY.d,data=dat.in)

m.SRP.in=gam(log(SRP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=11)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(35,11)),
            data = dat.in, method = "REML")
summary(m.SRP.in)

layout(matrix(1:4,2,2));gam.check(m.SRP.in,pch=21)
dev.off()
draw(m.SRP.in)
testUniformity(simulateResiduals(m.SRP.in))
acf(m.SRP.in$residuals)

pred.org=predict.gam(m.SRP.in,type="terms")
partial.resids.SRP.in<-pred.org+residuals(m.SRP.in)

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

gam.sum.table(m.SRP.in,"SRP")

# PP
plot(PP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(PP.ugL)==F)$PP.ugL)
pacf(subset(dat.in,is.na(PP.ugL)==F)$PP.ugL)
lmtest::bgtest(PP.ugL~CY.d,data=dat.in)

m.PP.in=gam(log(PP.ugL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY,k=11)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(40,11)),
             data = dat.in, method = "REML")
summary(m.PP.in)

layout(matrix(1:4,2,2));gam.check(m.PP.in,pch=21)
dev.off()
draw(m.PP.in)
testUniformity(simulateResiduals(m.PP.in))
acf(m.PP.in$residuals)

pred.org=predict.gam(m.PP.in,type="terms")
partial.resids.PP.in<-pred.org+residuals(m.PP.in)

mod.fit=predict.gam(m.PP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
PP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

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

gam.sum.table(m.PP.in,"PP")

# DP
plot(DP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(DP.ugL)==F)$DP.ugL)
pacf(subset(dat.in,is.na(DP.ugL)==F)$DP.ugL)
lmtest::bgtest(DP.ugL~CY.d,data=dat.in)

m.DP.in=gam(log(DP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=8)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,11)),
            data = dat.in, method = "REML")
summary(m.DP.in)

layout(matrix(1:4,2,2));gam.check(m.DP.in,pch=21)
dev.off()
draw(m.DP.in)
testUniformity(simulateResiduals(m.DP.in))
acf(m.DP.in$residuals)

pred.org=predict.gam(m.DP.in,type="terms")
partial.resids.DP.in<-pred.org+residuals(m.DP.in)

mod.fit=predict.gam(m.DP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

DP.in.pdat=GAM.SigChange.fun(m.DP.in,DP.in.pdat)
unique(subset(DP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DP.in.DOY.sig=ddply(DP.in.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DP.in,"DP")

# DOP
plot(DOP.ugL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(DOP.ugL)==F)$DOP.ugL)
pacf(subset(dat.in,is.na(DOP.ugL)==F)$DOP.ugL)
lmtest::bgtest(DOP.ugL~CY.d,data=dat.in)

m.DOP.in=gam(log(DOP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=8)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,11)),
            data = dat.in, method = "REML")
summary(m.DOP.in)

layout(matrix(1:4,2,2));gam.check(m.DOP.in,pch=21)
dev.off()
draw(m.DOP.in)
testUniformity(simulateResiduals(m.DOP.in))
acf(m.DOP.in$residuals)

pred.org=predict.gam(m.DOP.in,type="terms")
partial.resids.DOP.in<-pred.org+residuals(m.DOP.in)

mod.fit=predict.gam(m.DOP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DOP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

DOP.in.pdat=GAM.SigChange.fun(m.DOP.in,DOP.in.pdat)
unique(subset(DOP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DOP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DOP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DOP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DOP.in.DOY.sig=ddply(DOP.in.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DOP.in,"DOP")

# TN
plot(TN.mgL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(TN.mgL)==F)$TN.mgL)
pacf(subset(dat.in,is.na(TN.mgL)==F)$TN.mgL)
lmtest::bgtest(TN.mgL~CY.d,data=dat.in)

m.TN.in=gam(log(TN.mgL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(37,5)),
             data = dat.in, method = "REML")
summary(m.TN.in)

layout(matrix(1:4,2,2));gam.check(m.TN.in,pch=21)
dev.off()
draw(m.TN.in)
testUniformity(simulateResiduals(m.TN.in))
acf(m.TN.in$residuals)

pred.org=predict.gam(m.TN.in,type="terms")
partial.resids.TN.in<-pred.org+residuals(m.TN.in)

mod.fit=predict.gam(m.TN.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TN.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

TN.in.pdat=GAM.SigChange.fun(m.TN.in,TN.in.pdat)
unique(subset(TN.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TN.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TN.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TN.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TN.in.DOY.sig=ddply(TN.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TN.in,"TN")

# TKN
plot(TKN.mgL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(TKN.mgL)==F)$TKN.mgL)
pacf(subset(dat.in,is.na(TKN.mgL)==F)$TKN.mgL)
lmtest::bgtest(TKN.mgL~CY.d,data=dat.in)

m.TKN.in=gam(log(TKN.mgL)~
              s(DOY, bs = "cc",k=50) + 
              s(CY,k=11)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(40,6)),
            data = dat.in, method = "REML")
summary(m.TKN.in)

layout(matrix(1:4,2,2));gam.check(m.TKN.in,pch=21)
dev.off()
draw(m.TKN.in)
testUniformity(simulateResiduals(m.TKN.in))
acf(m.TKN.in$residuals)

pred.org=predict.gam(m.TKN.in,type="terms")
partial.resids.TKN.in<-pred.org+residuals(m.TKN.in)

mod.fit=predict.gam(m.TKN.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TKN.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

TKN.in.pdat=GAM.SigChange.fun(m.TKN.in,TKN.in.pdat)
unique(subset(TKN.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TKN.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TKN.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TKN.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TKN.in.DOY.sig=ddply(TKN.in.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TKN.in,"TKN")


# NH4
plot(NH4.mgL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(NH4.mgL)==F)$NH4.mgL)
pacf(subset(dat.in,is.na(NH4.mgL)==F)$NH4.mgL)
lmtest::bgtest(NH4.mgL~CY.d,data=dat.in)

m.NH4.in=gam(log(NH4.mgL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=8)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(40,5)),
            data = dat.in, method = "REML")
summary(m.NH4.in)

layout(matrix(1:4,2,2));gam.check(m.NH4.in,pch=21)
dev.off()
draw(m.NH4.in)
testUniformity(simulateResiduals(m.NH4.in))
acf(m.NH4.in$residuals)

pred.org=predict.gam(m.NH4.in,type="terms")
partial.resids.NH4.in<-pred.org+residuals(m.NH4.in)

mod.fit=predict.gam(m.NH4.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NH4.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

NH4.in.pdat=GAM.SigChange.fun(m.NH4.in,NH4.in.pdat)
unique(subset(NH4.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NH4.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NH4.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NH4.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NH4.in.DOY.sig=ddply(NH4.in.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NH4.in,"NH4")

# NOx
plot(NOx.mgL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(NOx.mgL)==F)$NOx.mgL)
pacf(subset(dat.in,is.na(NOx.mgL)==F)$NOx.mgL)
lmtest::bgtest(NOx.mgL~CY.d,data=dat.in)

m.NOx.in=gam(log(NOx.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=10)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
             data = dat.in, method = "REML")
summary(m.NOx.in)

layout(matrix(1:4,2,2));gam.check(m.NOx.in,pch=21)
dev.off()
draw(m.NOx.in)
testUniformity(simulateResiduals(m.NOx.in))
acf(m.NOx.in$residuals)

pred.org=predict.gam(m.NOx.in,type="terms")
partial.resids.NOx.in<-pred.org+residuals(m.NOx.in)

mod.fit=predict.gam(m.NOx.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NOx.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

NOx.in.pdat=GAM.SigChange.fun(m.NOx.in,NOx.in.pdat)
unique(subset(NOx.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NOx.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NOx.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NOx.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NOx.in.DOY.sig=ddply(NOx.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NOx.in,"NOx")

# DIN
plot(DIN.mgL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(DIN.mgL)==F)$DIN.mgL)
pacf(subset(dat.in,is.na(DIN.mgL)==F)$DIN.mgL)
lmtest::bgtest(DIN.mgL~CY.d,data=dat.in)

m.DIN.in=gam(log(DIN.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
             data = dat.in, method = "REML")
summary(m.DIN.in)

layout(matrix(1:4,2,2));gam.check(m.DIN.in,pch=21)
dev.off()
draw(m.DIN.in)
testUniformity(simulateResiduals(m.DIN.in))
acf(m.DIN.in$residuals)

pred.org=predict.gam(m.DIN.in,type="terms")
partial.resids.DIN.in<-pred.org+residuals(m.DIN.in)

mod.fit=predict.gam(m.DIN.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DIN.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

DIN.in.pdat=GAM.SigChange.fun(m.DIN.in,DIN.in.pdat)
unique(subset(DIN.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DIN.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DIN.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DIN.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DIN.in.DOY.sig=ddply(DIN.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DIN.in,"DIN")

# TON
plot(TON.mgL~Date,dat.in,log="y")
acf(subset(dat.in,is.na(TON.mgL)==F)$TON.mgL)
pacf(subset(dat.in,is.na(TON.mgL)==F)$TON.mgL)
lmtest::bgtest(TON.mgL~CY.d,data=dat.in)

m.TON.in=gam(log(TON.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
             data = dat.in, method = "REML")
summary(m.TON.in)

layout(matrix(1:4,2,2));gam.check(m.TON.in,pch=21)
dev.off()
draw(m.TON.in)
testUniformity(simulateResiduals(m.TON.in))
acf(m.TON.in$residuals)

pred.org=predict.gam(m.TON.in,type="terms")
partial.resids.TON.in<-pred.org+residuals(m.TON.in)

mod.fit=predict.gam(m.TON.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TON.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

TON.in.pdat=GAM.SigChange.fun(m.TON.in,TON.in.pdat)
unique(subset(TON.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TON.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TON.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TON.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TON.in.DOY.sig=ddply(TON.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TON.in,"TON")

# TN_TP
plot(TN_TP~Date,dat.in)
acf(subset(dat.in,is.na(TN_TP)==F)$TN_TP)
pacf(subset(dat.in,is.na(TN_TP)==F)$TN_TP)
lmtest::bgtest(TN_TP~CY.d,data=dat.in)

m.TN_TP.in=gam(TN_TP~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
             data = dat.in, method = "REML")
summary(m.TN_TP.in)

layout(matrix(1:4,2,2));gam.check(m.TN_TP.in,pch=21)
dev.off()
draw(m.TN_TP.in)
testUniformity(simulateResiduals(m.TN_TP.in))
acf(m.TN_TP.in$residuals)

pred.org=predict.gam(m.TN_TP.in,type="terms")
partial.resids.TN_TP.in<-pred.org+residuals(m.TN_TP.in)

mod.fit=predict.gam(m.TN_TP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TN_TP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

TN_TP.in.pdat=GAM.SigChange.fun(m.TN_TP.in,TN_TP.in.pdat)
unique(subset(TN_TP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TN_TP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TN_TP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TN_TP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TN_TP.in.DOY.sig=ddply(TN_TP.in.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TN_TP.in,"TN_TP")

# DIN_SRP
plot(DIN_SRP~Date,dat.in)
acf(subset(dat.in,is.na(DIN_SRP)==F)$DIN_SRP)
pacf(subset(dat.in,is.na(DIN_SRP)==F)$DIN_SRP)
lmtest::bgtest(DIN_SRP~CY.d,data=dat.in)

m.DIN_SRP.in=gam(DIN_SRP~
                 s(DOY, bs = "cc",k=20) + 
                 s(CY,k=8)+
                 ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
               data = dat.in, method = "REML")
summary(m.DIN_SRP.in)

layout(matrix(1:4,2,2));gam.check(m.DIN_SRP.in,pch=21)
dev.off()
draw(m.DIN_SRP.in)
testUniformity(simulateResiduals(m.DIN_SRP.in))
acf(m.DIN_SRP.in$residuals)

pred.org=predict.gam(m.DIN_SRP.in,type="terms")
partial.resids.DIN_SRP.in<-pred.org+residuals(m.DIN_SRP.in)

mod.fit=predict.gam(m.DIN_SRP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DIN_SRP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

DIN_SRP.in.pdat=GAM.SigChange.fun(m.DIN_SRP.in,DIN_SRP.in.pdat)
unique(subset(DIN_SRP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DIN_SRP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DIN_SRP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DIN_SRP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DIN_SRP.in.DOY.sig=ddply(DIN_SRP.in.pdat,c('DOY'),summarise,
                       fit=mean(fit.DOY),
                       UCI=mean(upper.DOY,na.rm=T),
                       LCI=mean(lower.DOY,na.rm=T),
                       dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                       dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DIN_SRP.in,"DIN_SRP")

# PP_TP
plot(PP_TP~Date,dat.in)
acf(subset(dat.in,is.na(PP_TP)==F)$PP_TP)
pacf(subset(dat.in,is.na(PP_TP)==F)$PP_TP)
lmtest::bgtest(PP_TP~CY.d,data=dat.in)

m.PP_TP.in=gam(PP_TP~
                   s(DOY, bs = "cc",k=20) + 
                   s(CY,k=11)+
                   ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
                 data = dat.in, method = "REML")
summary(m.PP_TP.in)

layout(matrix(1:4,2,2));gam.check(m.PP_TP.in,pch=21)
dev.off()
draw(m.PP_TP.in)
testUniformity(simulateResiduals(m.PP_TP.in))
acf(m.PP_TP.in$residuals)

pred.org=predict.gam(m.PP_TP.in,type="terms")
partial.resids.PP_TP.in<-pred.org+residuals(m.PP_TP.in)

mod.fit=predict.gam(m.PP_TP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
PP_TP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

PP_TP.in.pdat=GAM.SigChange.fun(m.PP_TP.in,PP_TP.in.pdat)
unique(subset(PP_TP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(PP_TP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(PP_TP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(PP_TP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
PP_TP.in.DOY.sig=ddply(PP_TP.in.pdat,c('DOY'),summarise,
                         fit=mean(fit.DOY),
                         UCI=mean(upper.DOY,na.rm=T),
                         LCI=mean(lower.DOY,na.rm=T),
                         dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                         dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.PP_TP.in,"PP_TP")

# DP_TP
plot(DP_TP~Date,dat.in)
acf(subset(dat.in,is.na(DP_TP)==F)$DP_TP)
pacf(subset(dat.in,is.na(DP_TP)==F)$DP_TP)
lmtest::bgtest(DP_TP~CY.d,data=dat.in)

m.DP_TP.in=gam(DP_TP~
                 s(DOY, bs = "cc",k=20) + 
                 s(CY,k=11)+
                 ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
               data = dat.in, method = "REML")
summary(m.DP_TP.in)

layout(matrix(1:4,2,2));gam.check(m.DP_TP.in,pch=21)
dev.off()
draw(m.DP_TP.in)
testUniformity(simulateResiduals(m.DP_TP.in))
acf(m.DP_TP.in$residuals)

pred.org=predict.gam(m.DP_TP.in,type="terms")
partial.resids.DP_TP.in<-pred.org+residuals(m.DP_TP.in)

mod.fit=predict.gam(m.DP_TP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DP_TP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

DP_TP.in.pdat=GAM.SigChange.fun(m.DP_TP.in,DP_TP.in.pdat)
unique(subset(DP_TP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DP_TP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DP_TP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DP_TP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DP_TP.in.DOY.sig=ddply(DP_TP.in.pdat,c('DOY'),summarise,
                       fit=mean(fit.DOY),
                       UCI=mean(upper.DOY,na.rm=T),
                       LCI=mean(lower.DOY,na.rm=T),
                       dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                       dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DP_TP.in,"DP_TP")

# SRP_TP
plot(SRP_TP~Date,dat.in)
acf(subset(dat.in,is.na(SRP_TP)==F)$SRP_TP)
pacf(subset(dat.in,is.na(SRP_TP)==F)$SRP_TP)
lmtest::bgtest(SRP_TP~CY.d,data=dat.in)

m.SRP_TP.in=gam(SRP_TP~
                 s(DOY, bs = "cc",k=20) + 
                 s(CY,k=11)+
                 ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
               data = dat.in, method = "REML")
summary(m.SRP_TP.in)

layout(matrix(1:4,2,2));gam.check(m.SRP_TP.in,pch=21)
dev.off()
draw(m.SRP_TP.in)
testUniformity(simulateResiduals(m.SRP_TP.in))
acf(m.SRP_TP.in$residuals)

pred.org=predict.gam(m.SRP_TP.in,type="terms")
partial.resids.SRP_TP.in<-pred.org+residuals(m.SRP_TP.in)

mod.fit=predict.gam(m.SRP_TP.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
SRP_TP.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

SRP_TP.in.pdat=GAM.SigChange.fun(m.SRP_TP.in,SRP_TP.in.pdat)
unique(subset(SRP_TP.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(SRP_TP.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(SRP_TP.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(SRP_TP.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
SRP_TP.in.DOY.sig=ddply(SRP_TP.in.pdat,c('DOY'),summarise,
                       fit=mean(fit.DOY),
                       UCI=mean(upper.DOY,na.rm=T),
                       LCI=mean(lower.DOY,na.rm=T),
                       dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                       dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.SRP_TP.in,"SRP_TP")

# NH4_TN
plot(NH4_TN~Date,dat.in)
acf(subset(dat.in,is.na(NH4_TN)==F)$NH4_TN)
pacf(subset(dat.in,is.na(NH4_TN)==F)$NH4_TN)
lmtest::bgtest(NH4_TN~CY.d,data=dat.in)

m.NH4_TN.in=gam(NH4_TN~
                  s(DOY, bs = "cc",k=20) + 
                  s(CY,k=8)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
                data = dat.in, method = "REML")
summary(m.NH4_TN.in)

layout(matrix(1:4,2,2));gam.check(m.NH4_TN.in,pch=21)
dev.off()
draw(m.NH4_TN.in)
testUniformity(simulateResiduals(m.NH4_TN.in))
acf(m.NH4_TN.in$residuals)

pred.org=predict.gam(m.NH4_TN.in,type="terms")
partial.resids.NH4_TN.in<-pred.org+residuals(m.NH4_TN.in)

mod.fit=predict.gam(m.NH4_TN.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NH4_TN.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

NH4_TN.in.pdat=GAM.SigChange.fun(m.NH4_TN.in,NH4_TN.in.pdat)
unique(subset(NH4_TN.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NH4_TN.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NH4_TN.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NH4_TN.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NH4_TN.in.DOY.sig=ddply(NH4_TN.in.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NH4_TN.in,"NH4_TN")

# NOx_TN
plot(NOx_TN~Date,dat.in)
acf(subset(dat.in,is.na(NOx_TN)==F)$NOx_TN)
pacf(subset(dat.in,is.na(NOx_TN)==F)$NOx_TN)
lmtest::bgtest(NOx_TN~CY.d,data=dat.in)

m.NOx_TN.in=gam(NOx_TN~
                  s(DOY, bs = "cc",k=20) + 
                  s(CY,k=10)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
                data = dat.in, method = "REML")
summary(m.NOx_TN.in)

layout(matrix(1:4,2,2));gam.check(m.NOx_TN.in,pch=21)
dev.off()
draw(m.NOx_TN.in)
testUniformity(simulateResiduals(m.NOx_TN.in))
acf(m.NOx_TN.in$residuals)

pred.org=predict.gam(m.NOx_TN.in,type="terms")
partial.resids.NOx_TN.in<-pred.org+residuals(m.NOx_TN.in)

mod.fit=predict.gam(m.NOx_TN.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NOx_TN.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

NOx_TN.in.pdat=GAM.SigChange.fun(m.NOx_TN.in,NOx_TN.in.pdat)
unique(subset(NOx_TN.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NOx_TN.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NOx_TN.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NOx_TN.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NOx_TN.in.DOY.sig=ddply(NOx_TN.in.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NOx_TN.in,"NOx_TN")

# Colour_PCU
plot(Colour_PCU~Date,dat.in)
range(subset(dat.in, is.na(Colour_PCU)==F)$CY)
acf(subset(dat.in,is.na(Colour_PCU)==F)$Colour_PCU)
pacf(subset(dat.in,is.na(Colour_PCU)==F)$Colour_PCU)
lmtest::bgtest(Colour_PCU~CY.d,data=dat.in)

m.Colour_PCU.in=gam(Colour_PCU~
                  s(DOY, bs = "cc",k=10) + 
                  s(CY,k=4)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(10,4)),
                data = dat.in, method = "REML")
summary(m.Colour_PCU.in)

layout(matrix(1:4,2,2));gam.check(m.Colour_PCU.in,pch=21)
dev.off()
draw(m.Colour_PCU.in)
testUniformity(simulateResiduals(m.Colour_PCU.in))
acf(m.Colour_PCU.in$residuals)

pred.org=predict.gam(m.Colour_PCU.in,type="terms")
partial.resids.Colour_PCU.in<-pred.org+residuals(m.Colour_PCU.in)

mod.fit=predict.gam(m.Colour_PCU.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Colour_PCU.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

Colour_PCU.in.pdat=GAM.SigChange.fun(m.Colour_PCU.in,Colour_PCU.in.pdat)
unique(subset(Colour_PCU.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Colour_PCU.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Colour_PCU.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Colour_PCU.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Colour_PCU.in.DOY.sig=ddply(Colour_PCU.in.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Colour_PCU.in,"Colour_PCU")

# Temp.C
plot(Temp.C~Date,dat.in)
range(subset(dat.in, is.na(Temp.C)==F)$CY)
acf(subset(dat.in,is.na(Temp.C)==F)$Temp.C)
pacf(subset(dat.in,is.na(Temp.C)==F)$Temp.C)
lmtest::bgtest(Temp.C~CY.d,data=dat.in)

m.Temp.C.in=gam(Temp.C~
                      s(DOY, bs = "cc",k=10) + 
                      s(CY,k=12)+
                      ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,12)),
                    data = dat.in, method = "REML")
summary(m.Temp.C.in)

layout(matrix(1:4,2,2));gam.check(m.Temp.C.in,pch=21)
dev.off()
draw(m.Temp.C.in)
testUniformity(simulateResiduals(m.Temp.C.in))
acf(m.Temp.C.in$residuals)

pred.org=predict.gam(m.Temp.C.in,type="terms")
partial.resids.Temp.C.in<-pred.org+residuals(m.Temp.C.in)

mod.fit=predict.gam(m.Temp.C.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Temp.C.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

Temp.C.in.pdat=GAM.SigChange.fun(m.Temp.C.in,Temp.C.in.pdat)
unique(subset(Temp.C.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Temp.C.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Temp.C.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Temp.C.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Temp.C.in.DOY.sig=ddply(Temp.C.in.pdat,c('DOY'),summarise,
                            fit=mean(fit.DOY),
                            UCI=mean(upper.DOY,na.rm=T),
                            LCI=mean(lower.DOY,na.rm=T),
                            dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                            dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Temp.C.in,"Temp.C")

# Cond
plot(Cond~Date,dat.in)
range(subset(dat.in, is.na(Cond)==F)$CY)
acf(subset(dat.in,is.na(Cond)==F)$Cond)
pacf(subset(dat.in,is.na(Cond)==F)$Cond)
lmtest::bgtest(Cond~CY.d,data=dat.in)

m.Cond.in=gam(Cond~
                  s(DOY, bs = "cc",k=10) + 
                  s(CY,k=12)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,12)),
                data = dat.in, method = "REML")
summary(m.Cond.in)

layout(matrix(1:4,2,2));gam.check(m.Cond.in,pch=21)
dev.off()
draw(m.Cond.in)
testUniformity(simulateResiduals(m.Cond.in))
acf(m.Cond.in$residuals)

pred.org=predict.gam(m.Cond.in,type="terms")
partial.resids.Cond.in<-pred.org+residuals(m.Cond.in)

mod.fit=predict.gam(m.Cond.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Cond.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

Cond.in.pdat=GAM.SigChange.fun(m.Cond.in,Cond.in.pdat)
unique(subset(Cond.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Cond.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Cond.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Cond.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Cond.in.DOY.sig=ddply(Cond.in.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Cond.in,"Cond")

# DO.per
plot(DO.per~Date,dat.in)
range(subset(dat.in, is.na(DO.per)==F)$CY)
acf(subset(dat.in,is.na(DO.per)==F)$DO.per)
pacf(subset(dat.in,is.na(DO.per)==F)$DO.per)
lmtest::bgtest(DO.per~CY.d,data=dat.in)

m.DO.per.in=gam(DO.per~
                s(DOY, bs = "cc",k=10) + 
                s(CY,k=7)+
                ti(DOY,CY,bs = c('cc', 'tp'), k = c(35,7)),
              data = dat.in, method = "REML")
summary(m.DO.per.in)

layout(matrix(1:4,2,2));gam.check(m.DO.per.in,pch=21)
dev.off()
draw(m.DO.per.in)
testUniformity(simulateResiduals(m.DO.per.in))
acf(m.DO.per.in$residuals)

pred.org=predict.gam(m.DO.per.in,type="terms")
partial.resids.DO.per.in<-pred.org+residuals(m.DO.per.in)

mod.fit=predict.gam(m.DO.per.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DO.per.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

DO.per.in.pdat=GAM.SigChange.fun(m.DO.per.in,DO.per.in.pdat)
unique(subset(DO.per.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DO.per.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DO.per.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DO.per.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DO.per.in.DOY.sig=ddply(DO.per.in.pdat,c('DOY'),summarise,
                      fit=mean(fit.DOY),
                      UCI=mean(upper.DOY,na.rm=T),
                      LCI=mean(lower.DOY,na.rm=T),
                      dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                      dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DO.per.in,"DO.per")

# pH
dat.in$H=10^dat.in
plot(pH~Date,dat.in)
range(subset(dat.in, is.na(pH)==F)$CY)
acf(subset(dat.in,is.na(pH)==F)$pH)
pacf(subset(dat.in,is.na(pH)==F)$pH)
lmtest::bgtest(pH~CY.d,data=dat.in)

m.pH.in=gam(pH~
                  s(DOY, bs = "cc",k=30) + 
                  s(CY,k=4)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,4)),
                data = dat.in, method = "REML")
summary(m.pH.in)

layout(matrix(1:4,2,2));gam.check(m.pH.in,pch=21)
dev.off()
draw(m.pH.in)
testUniformity(simulateResiduals(m.pH.in))
acf(m.pH.in$residuals)

pred.org=predict.gam(m.pH.in,type="terms")
partial.resids.pH.in<-pred.org+residuals(m.pH.in)

mod.fit=predict.gam(m.pH.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
pH.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

pH.in.pdat=GAM.SigChange.fun(m.pH.in,pH.in.pdat)
unique(subset(pH.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(pH.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(pH.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(pH.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
pH.in.DOY.sig=ddply(pH.in.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.pH.in,"pH")

# Chla.ugL
plot(Chla.ugL~Date,dat.in)

# Phyco.ugL
plot(Phyco.ugL~Date,dat.in)
range(subset(dat.in, is.na(Phyco.ugL)==F)$CY)
acf(subset(dat.in,is.na(Phyco.ugL)==F)$Phyco.ugL)
pacf(subset(dat.in,is.na(Phyco.ugL)==F)$Phyco.ugL)
lmtest::bgtest(Phyco.ugL~CY.d,data=dat.in)

m.Phyco.ugL.in=gam(log(Phyco.ugL)~
              s(DOY, bs = "cc",k=30) + 
              s(CY,k=5)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,5)),
            data = dat.in, method = "REML")
summary(m.Phyco.ugL.in)

layout(matrix(1:4,2,2));gam.check(m.Phyco.ugL.in,pch=21)
dev.off()
draw(m.Phyco.ugL.in)
testUniformity(simulateResiduals(m.Phyco.ugL.in))
acf(m.Phyco.ugL.in$residuals)

pred.org=predict.gam(m.Phyco.ugL.in,type="terms")
partial.resids.Phyco.ugL.in<-pred.org+residuals(m.Phyco.ugL.in)

mod.fit=predict.gam(m.Phyco.ugL.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Phyco.ugL.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

Phyco.ugL.in.pdat=GAM.SigChange.fun(m.Phyco.ugL.in,Phyco.ugL.in.pdat)
unique(subset(Phyco.ugL.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Phyco.ugL.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Phyco.ugL.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Phyco.ugL.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Phyco.ugL.in.DOY.sig=ddply(Phyco.ugL.in.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Phyco.ugL.in,"Phyco.ugL")

# TChl.ugL
plot(TChl.ugL~Date,dat.in)
range(subset(dat.in, is.na(TChl.ugL)==F)$CY)
acf(subset(dat.in,is.na(TChl.ugL)==F)$TChl.ugL)
pacf(subset(dat.in,is.na(TChl.ugL)==F)$TChl.ugL)
lmtest::bgtest(TChl.ugL~CY.d,data=dat.in)

m.TChl.ugL.in=gam(log(TChl.ugL)~
                     s(DOY, bs = "cc",k=30) + 
                     s(CY,k=5)+
                     ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,5)),
                   data = dat.in, method = "REML")
summary(m.TChl.ugL.in)

layout(matrix(1:4,2,2));gam.check(m.TChl.ugL.in,pch=21)
dev.off()
draw(m.TChl.ugL.in)
testUniformity(simulateResiduals(m.TChl.ugL.in))
acf(m.TChl.ugL.in$residuals)

pred.org=predict.gam(m.TChl.ugL.in,type="terms")
partial.resids.TChl.ugL.in<-pred.org+residuals(m.TChl.ugL.in)

mod.fit=predict.gam(m.TChl.ugL.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TChl.ugL.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

TChl.ugL.in.pdat=GAM.SigChange.fun(m.TChl.ugL.in,TChl.ugL.in.pdat)
unique(subset(TChl.ugL.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TChl.ugL.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TChl.ugL.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TChl.ugL.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TChl.ugL.in.DOY.sig=ddply(TChl.ugL.in.pdat,c('DOY'),summarise,
                           fit=mean(fit.DOY),
                           UCI=mean(upper.DOY,na.rm=T),
                           LCI=mean(lower.DOY,na.rm=T),
                           dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                           dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TChl.ugL.in,"TChl.ugL")

# TSI
plot(TSI_mean2~Date,dat.in)
range(subset(dat.in, is.na(TSI_mean2)==F)$CY)
acf(subset(dat.in,is.na(TSI_mean2)==F)$TSI_mean2)
pacf(subset(dat.in,is.na(TSI_mean2)==F)$TSI_mean2)
lmtest::bgtest(TSI_mean2~CY.d,data=dat.in)

m.TSI.in=gam(TSI_mean2~
                    s(DOY, bs = "cc",k=30) + 
                    s(CY,k=9)+
                    ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,9)),
                  data = dat.in, method = "REML")
summary(m.TSI.in)

layout(matrix(1:4,2,2));gam.check(m.TSI.in,pch=21)
dev.off()
draw(m.TSI.in)
testUniformity(simulateResiduals(m.TSI.in))
acf(m.TSI.in$residuals)

pred.org=predict.gam(m.TSI.in,type="terms")
partial.resids.TSI.in<-pred.org+residuals(m.TSI.in)

mod.fit=predict.gam(m.TSI.in,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
m.TSI.in.pdat=cbind(pdat,tmp.fit,tmp.SE)

m.TSI.in.pdat=GAM.SigChange.fun(m.TSI.in,m.TSI.in.pdat)
unique(subset(m.TSI.in.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(m.TSI.in.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(m.TSI.in.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(m.TSI.in.pdat,is.na(dsig.DOY.decr)==F)$DOY)
m.TSI.in.DOY.sig=ddply(m.TSI.in.pdat,c('DOY'),summarise,
                          fit=mean(fit.DOY),
                          UCI=mean(upper.DOY,na.rm=T),
                          LCI=mean(lower.DOY,na.rm=T),
                          dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                          dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TSI.in,"TSI")

## Outflow Models ----------------------------------------------------------
# TP
plot(TP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL)
pacf(subset(dat.out,is.na(TP.ugL)==F)$TP.ugL)
lmtest::bgtest(TP.ugL~CY.d,data=dat.out)

m.TP.out=gam(log(TP.ugL)~
              s(DOY, bs = "cc",k=50) + 
              s(CY,k=11)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(50,9)),
            data = dat.out, method = "REML")
summary(m.TP.out)

layout(matrix(1:4,2,2));gam.check(m.TP.out,pch=21)
dev.off()
draw(m.TP.out)
testUniformity(simulateResiduals(m.TP.out))
acf(m.TP.out$residuals)


pred.org=predict.gam(m.TP.out,type="terms")
partial.resids.TP.out<-pred.org+residuals(m.TP.out)

mod.fit=predict.gam(m.TP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

TP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(TP.out.pdat)

TP.out.pdat=GAM.SigChange.fun(m.TP.out,TP.out.pdat)
unique(subset(TP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TP.out.DOY.sig=ddply(TP.out.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TP.out,"TP")

# SRP
plot(SRP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(SRP.ugL)==F)$SRP.ugL)
pacf(subset(dat.out,is.na(SRP.ugL)==F)$SRP.ugL)
lmtest::bgtest(SRP.ugL~CY.d,data=dat.out)

m.SRP.out=gam(log(SRP.ugL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY,k=11)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(35,11)),
             data = dat.out, method = "REML")
summary(m.SRP.out)

layout(matrix(1:4,2,2));gam.check(m.SRP.out,pch=21)
dev.off()
draw(m.SRP.out)
testUniformity(simulateResiduals(m.SRP.out))
acf(m.SRP.out$residuals)

pred.org=predict.gam(m.SRP.out,type="terms")
partial.resids.SRP.out<-pred.org+residuals(m.SRP.out)

mod.fit=predict.gam(m.SRP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")

SRP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)
head(SRP.out.pdat)

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

gam.sum.table(m.SRP.out,"SRP")

# PP
plot(PP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(PP.ugL)==F)$PP.ugL)
pacf(subset(dat.out,is.na(PP.ugL)==F)$PP.ugL)
lmtest::bgtest(PP.ugL~CY.d,data=dat.out)

m.PP.out=gam(log(PP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=11)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,11)),
            data = dat.out, method = "REML")
summary(m.PP.out)

layout(matrix(1:4,2,2));gam.check(m.PP.out,pch=21)
dev.off()
draw(m.PP.out)
testUniformity(simulateResiduals(m.PP.out))
acf(m.PP.out$residuals)

pred.org=predict.gam(m.PP.out,type="terms")
partial.resids.PP.out<-pred.org+residuals(m.PP.out)

mod.fit=predict.gam(m.PP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
PP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

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

gam.sum.table(m.PP.out,"PP")

# DP
plot(DP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(DP.ugL)==F)$DP.ugL)
pacf(subset(dat.out,is.na(DP.ugL)==F)$DP.ugL)
lmtest::bgtest(DP.ugL~CY.d,data=dat.out)

m.DP.out=gam(log(DP.ugL)~
              s(DOY, bs = "cc",k=15) + 
              s(CY,k=8)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,11)),
            data = dat.out, method = "REML")
summary(m.DP.out)

layout(matrix(1:4,2,2));gam.check(m.DP.out,pch=21)
dev.off()
draw(m.DP.out)
testUniformity(simulateResiduals(m.DP.out))
acf(m.DP.out$residuals)

pred.org=predict.gam(m.DP.out,type="terms")
partial.resids.DP.out<-pred.org+residuals(m.DP.out)

mod.fit=predict.gam(m.DP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

DP.out.pdat=GAM.SigChange.fun(m.DP.out,DP.out.pdat)
unique(subset(DP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DP.out.DOY.sig=ddply(DP.out.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DP.out,"DP")

# DOP
plot(DOP.ugL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(DOP.ugL)==F)$DOP.ugL)
pacf(subset(dat.out,is.na(DOP.ugL)==F)$DOP.ugL)
lmtest::bgtest(DOP.ugL~CY.d,data=dat.out)

m.DOP.out=gam(log(DOP.ugL)~
               s(DOY, bs = "cc",k=15) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,11)),
             data = dat.out, method = "REML")
summary(m.DOP.out)

layout(matrix(1:4,2,2));gam.check(m.DOP.out,pch=21)
dev.off()
draw(m.DOP.out)
testUniformity(simulateResiduals(m.DOP.out))
acf(m.DOP.out$residuals)

pred.org=predict.gam(m.DOP.out,type="terms")
partial.resids.DOP.out<-pred.org+residuals(m.DOP.out)

mod.fit=predict.gam(m.DOP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DOP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

DOP.out.pdat=GAM.SigChange.fun(m.DOP.out,DOP.out.pdat)
unique(subset(DOP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DOP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DOP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DOP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DOP.out.DOY.sig=ddply(DOP.out.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DOP.out,"DOP")

# TN
plot(TN.mgL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(TN.mgL)==F)$TN.mgL)
pacf(subset(dat.out,is.na(TN.mgL)==F)$TN.mgL)
lmtest::bgtest(TN.mgL~CY.d,data=dat.out)

m.TN.out=gam(log(TN.mgL)~
              s(DOY, bs = "cc",k=20) + 
              s(CY,k=10)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(50,8)),
            data = dat.out, method = "REML")
summary(m.TN.out)

layout(matrix(1:4,2,2));gam.check(m.TN.out,pch=21)
dev.off()
draw(m.TN.out)
testUniformity(simulateResiduals(m.TN.out))
acf(m.TN.out$residuals)

pred.org=predict.gam(m.TN.out,type="terms")
partial.resids.TN.out<-pred.org+residuals(m.TN.out)

mod.fit=predict.gam(m.TN.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TN.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

TN.out.pdat=GAM.SigChange.fun(m.TN.out,TN.out.pdat)
unique(subset(TN.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TN.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TN.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TN.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TN.out.DOY.sig=ddply(TN.out.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TN.out,"TN")

# TKN
plot(TKN.mgL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(TKN.mgL)==F)$TKN.mgL)
pacf(subset(dat.out,is.na(TKN.mgL)==F)$TKN.mgL)
lmtest::bgtest(TKN.mgL~CY.d,data=dat.out)

m.TKN.out=gam(log(TKN.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=10)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(50,8)),
             data = dat.out, method = "REML")
summary(m.TKN.out)

layout(matrix(1:4,2,2));gam.check(m.TKN.out,pch=21)
dev.off()
draw(m.TKN.out)
testUniformity(simulateResiduals(m.TKN.out))
acf(m.TKN.out$residuals)

pred.org=predict.gam(m.TKN.out,type="terms")
partial.resids.TKN.out<-pred.org+residuals(m.TKN.out)

mod.fit=predict.gam(m.TKN.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TKN.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

TKN.out.pdat=GAM.SigChange.fun(m.TKN.out,TKN.out.pdat)
unique(subset(TKN.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TKN.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TKN.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TKN.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TKN.out.DOY.sig=ddply(TKN.out.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TKN.out,"TKN")

# NH4
plot(NH4.mgL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(NH4.mgL)==F)$NH4.mgL)
pacf(subset(dat.out,is.na(NH4.mgL)==F)$NH4.mgL)
lmtest::bgtest(NH4.mgL~CY.d,data=dat.out)

m.NH4.out=gam(log(NH4.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(53,7)),
             data = dat.out, method = "REML")
summary(m.NH4.out)

layout(matrix(1:4,2,2));gam.check(m.NH4.out,pch=21)
dev.off()
draw(m.NH4.out)
testUniformity(simulateResiduals(m.NH4.out))
acf(m.NH4.out$residuals)

pred.org=predict.gam(m.NH4.out,type="terms")
partial.resids.NH4.out<-pred.org+residuals(m.NH4.out)

mod.fit=predict.gam(m.NH4.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NH4.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

NH4.out.pdat=GAM.SigChange.fun(m.NH4.out,NH4.out.pdat)
unique(subset(NH4.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NH4.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NH4.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NH4.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NH4.out.DOY.sig=ddply(NH4.out.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NH4.out,"NH4")

# NOx
plot(NOx.mgL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(NOx.mgL)==F)$NOx.mgL)
pacf(subset(dat.out,is.na(NOx.mgL)==F)$NOx.mgL)
lmtest::bgtest(NOx.mgL~CY.d,data=dat.out)

m.NOx.out=gam(log(NOx.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=10)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
             data = dat.out, method = "REML")
summary(m.NOx.out)

layout(matrix(1:4,2,2));gam.check(m.NOx.out,pch=21)
dev.off()
draw(m.NOx.out)
testUniformity(simulateResiduals(m.NOx.out))
acf(m.NOx.out$residuals)

pred.org=predict.gam(m.NOx.out,type="terms")
partial.resids.NOx.out<-pred.org+residuals(m.NOx.out)

mod.fit=predict.gam(m.NOx.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NOx.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

NOx.out.pdat=GAM.SigChange.fun(m.NOx.out,NOx.out.pdat)
unique(subset(NOx.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NOx.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NOx.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NOx.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NOx.out.DOY.sig=ddply(NOx.out.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NOx.out,"NOx")

# DIN
plot(DIN.mgL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(DIN.mgL)==F)$DIN.mgL)
pacf(subset(dat.out,is.na(DIN.mgL)==F)$DIN.mgL)
lmtest::bgtest(DIN.mgL~CY.d,data=dat.out)

m.DIN.out=gam(log(DIN.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
             data = dat.out, method = "REML")
summary(m.DIN.out)

layout(matrix(1:4,2,2));gam.check(m.DIN.out,pch=21)
dev.off()
draw(m.DIN.out)
testUniformity(simulateResiduals(m.DIN.out))
acf(m.DIN.out$residuals)

pred.org=predict.gam(m.DIN.out,type="terms")
partial.resids.DIN.out<-pred.org+residuals(m.DIN.out)

mod.fit=predict.gam(m.DIN.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DIN.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

DIN.out.pdat=GAM.SigChange.fun(m.DIN.out,DIN.out.pdat)
unique(subset(DIN.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DIN.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DIN.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DIN.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DIN.out.DOY.sig=ddply(DIN.out.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DIN.out,"DIN")

# TON
plot(TON.mgL~Date,dat.out,log="y")
acf(subset(dat.out,is.na(TON.mgL)==F)$TON.mgL)
pacf(subset(dat.out,is.na(TON.mgL)==F)$TON.mgL)
lmtest::bgtest(TON.mgL~CY.d,data=dat.out)

m.TON.out=gam(log(TON.mgL)~
               s(DOY, bs = "cc",k=20) + 
               s(CY,k=8)+
               ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
             data = dat.out, method = "REML")
summary(m.TON.out)

layout(matrix(1:4,2,2));gam.check(m.TON.out,pch=21)
dev.off()
draw(m.TON.out)
testUniformity(simulateResiduals(m.TON.out))
acf(m.TON.out$residuals)

pred.org=predict.gam(m.TON.out,type="terms")
partial.resids.TON.out<-pred.org+residuals(m.TON.out)

mod.fit=predict.gam(m.TON.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TON.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

TON.out.pdat=GAM.SigChange.fun(m.TON.out,TON.out.pdat)
unique(subset(TON.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TON.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TON.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TON.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TON.out.DOY.sig=ddply(TON.out.pdat,c('DOY'),summarise,
                     fit=mean(fit.DOY),
                     UCI=mean(upper.DOY,na.rm=T),
                     LCI=mean(lower.DOY,na.rm=T),
                     dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                     dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TON.out,"TON")

# TN_TP
plot(TN_TP~Date,dat.out)
acf(subset(dat.out,is.na(TN_TP)==F)$TN_TP)
pacf(subset(dat.out,is.na(TN_TP)==F)$TN_TP)
lmtest::bgtest(TN_TP~CY.d,data=dat.out)

m.TN_TP.out=gam(TN_TP~
                 s(DOY, bs = "cc",k=20) + 
                 s(CY,k=10)+
                 ti(DOY,CY,bs = c('cc', 'tp'), k = c(43,8)),
               data = dat.out, method = "REML")
summary(m.TN_TP.out)

layout(matrix(1:4,2,2));gam.check(m.TN_TP.out,pch=21)
dev.off()
draw(m.TN_TP.out)
testUniformity(simulateResiduals(m.TN_TP.out))
acf(m.TN_TP.out$residuals)

pred.org=predict.gam(m.TN_TP.out,type="terms")
partial.resids.TN_TP.out<-pred.org+residuals(m.TN_TP.out)

mod.fit=predict.gam(m.TN_TP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TN_TP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

TN_TP.out.pdat=GAM.SigChange.fun(m.TN_TP.out,TN_TP.out.pdat)
unique(subset(TN_TP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TN_TP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TN_TP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TN_TP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TN_TP.out.DOY.sig=ddply(TN_TP.out.pdat,c('DOY'),summarise,
                       fit=mean(fit.DOY),
                       UCI=mean(upper.DOY,na.rm=T),
                       LCI=mean(lower.DOY,na.rm=T),
                       dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                       dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TN_TP.out,"TN_TP")

# DIN_SRP
plot(DIN_SRP~Date,dat.out)
subset(dat.out,DIN_SRP>1000)
dat.out[is.na(dat.out$DIN_SRP)==F&dat.out$DIN_SRP>1000,"DIN_SRP"]<-NA

acf(subset(dat.out,is.na(DIN_SRP)==F)$DIN_SRP)
pacf(subset(dat.out,is.na(DIN_SRP)==F)$DIN_SRP)
lmtest::bgtest(DIN_SRP~CY.d,data=dat.out)

m.DIN_SRP.out=gam(DIN_SRP~
                  s(DOY, bs = "cc",k=20) + 
                  s(CY,k=8)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(44,8)),
                data = dat.out, method = "REML")
summary(m.DIN_SRP.out)

layout(matrix(1:4,2,2));gam.check(m.DIN_SRP.out,pch=21)
dev.off()
draw(m.DIN_SRP.out)
testUniformity(simulateResiduals(m.DIN_SRP.out))
acf(m.DIN_SRP.out$residuals)

pred.org=predict.gam(m.DIN_SRP.out,type="terms")
partial.resids.DIN_SRP.out<-pred.org+residuals(m.DIN_SRP.out)

mod.fit=predict.gam(m.DIN_SRP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DIN_SRP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

DIN_SRP.out.pdat=GAM.SigChange.fun(m.DIN_SRP.out,DIN_SRP.out.pdat)
unique(subset(DIN_SRP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DIN_SRP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DIN_SRP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DIN_SRP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DIN_SRP.out.DOY.sig=ddply(DIN_SRP.out.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DIN_SRP.out,"DIN_SRP")

# PP_TP
plot(PP_TP~Date,dat.out)
acf(subset(dat.out,is.na(PP_TP)==F)$PP_TP)
pacf(subset(dat.out,is.na(PP_TP)==F)$PP_TP)
lmtest::bgtest(PP_TP~CY.d,data=dat.out)

m.PP_TP.out=gam(PP_TP~
                 s(DOY, bs = "cc",k=20) + 
                 s(CY,k=11)+
                 ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
               data = dat.out, method = "REML")
summary(m.PP_TP.out)

layout(matrix(1:4,2,2));gam.check(m.PP_TP.out,pch=21)
dev.off()
draw(m.PP_TP.out)
testUniformity(simulateResiduals(m.PP_TP.out))
acf(m.PP_TP.out$residuals)

pred.org=predict.gam(m.PP_TP.out,type="terms")
partial.resids.PP_TP.out<-pred.org+residuals(m.PP_TP.out)

mod.fit=predict.gam(m.PP_TP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
PP_TP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

PP_TP.out.pdat=GAM.SigChange.fun(m.PP_TP.out,PP_TP.out.pdat)
unique(subset(PP_TP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(PP_TP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(PP_TP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(PP_TP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
PP_TP.out.DOY.sig=ddply(PP_TP.out.pdat,c('DOY'),summarise,
                       fit=mean(fit.DOY),
                       UCI=mean(upper.DOY,na.rm=T),
                       LCI=mean(lower.DOY,na.rm=T),
                       dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                       dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.PP_TP.out,"PP_TP")

# DP_TP
plot(DP_TP~Date,dat.out)
acf(subset(dat.out,is.na(DP_TP)==F)$DP_TP)
pacf(subset(dat.out,is.na(DP_TP)==F)$DP_TP)
lmtest::bgtest(DP_TP~CY.d,data=dat.out)

m.DP_TP.out=gam(DP_TP~
                 s(DOY, bs = "cc",k=20) + 
                 s(CY,k=11)+
                 ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,11)),
               data = dat.out, method = "REML")
summary(m.DP_TP.out)

layout(matrix(1:4,2,2));gam.check(m.DP_TP.out,pch=21)
dev.off()
draw(m.DP_TP.out)
testUniformity(simulateResiduals(m.DP_TP.out))
acf(m.DP_TP.out$residuals)

pred.org=predict.gam(m.DP_TP.out,type="terms")
partial.resids.DP_TP.out<-pred.org+residuals(m.DP_TP.out)

mod.fit=predict.gam(m.DP_TP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DP_TP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

DP_TP.out.pdat=GAM.SigChange.fun(m.DP_TP.out,DP_TP.out.pdat)
unique(subset(DP_TP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DP_TP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DP_TP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DP_TP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DP_TP.out.DOY.sig=ddply(DP_TP.out.pdat,c('DOY'),summarise,
                       fit=mean(fit.DOY),
                       UCI=mean(upper.DOY,na.rm=T),
                       LCI=mean(lower.DOY,na.rm=T),
                       dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                       dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DP_TP.out,"DP_TP")

# SRP_TP
plot(SRP_TP~Date,dat.out)
acf(subset(dat.out,is.na(SRP_TP)==F)$SRP_TP)
pacf(subset(dat.out,is.na(SRP_TP)==F)$SRP_TP)
lmtest::bgtest(SRP_TP~CY.d,data=dat.out)

m.SRP_TP.out=gam(SRP_TP~
                  s(DOY, bs = "cc",k=20) + 
                  s(CY,k=11)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
                data = dat.out, method = "REML")
summary(m.SRP_TP.out)

layout(matrix(1:4,2,2));gam.check(m.SRP_TP.out,pch=21)
dev.off()
draw(m.SRP_TP.out)
testUniformity(simulateResiduals(m.SRP_TP.out))
acf(m.SRP_TP.out$residuals)

pred.org=predict.gam(m.SRP_TP.out,type="terms")
partial.resids.SRP_TP.out<-pred.org+residuals(m.SRP_TP.out)

mod.fit=predict.gam(m.SRP_TP.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
SRP_TP.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

SRP_TP.out.pdat=GAM.SigChange.fun(m.SRP_TP.out,SRP_TP.out.pdat)
unique(subset(SRP_TP.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(SRP_TP.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(SRP_TP.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(SRP_TP.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
SRP_TP.out.DOY.sig=ddply(SRP_TP.out.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.SRP_TP.out,"SRP_TP")

# NH4_TN
plot(NH4_TN~Date,dat.out)
acf(subset(dat.out,is.na(NH4_TN)==F)$NH4_TN)
pacf(subset(dat.out,is.na(NH4_TN)==F)$NH4_TN)
lmtest::bgtest(NH4_TN~CY.d,data=dat.out)

m.NH4_TN.out=gam(NH4_TN~
                  s(DOY, bs = "cc",k=20) + 
                  s(CY,k=8)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,8)),
                data = dat.out, method = "REML")
summary(m.NH4_TN.out)

layout(matrix(1:4,2,2));gam.check(m.NH4_TN.out,pch=21)
dev.off()
draw(m.NH4_TN.out)
testUniformity(simulateResiduals(m.NH4_TN.out))
acf(m.NH4_TN.out$residuals)

pred.org=predict.gam(m.NH4_TN.out,type="terms")
partial.resids.NH4_TN.out<-pred.org+residuals(m.NH4_TN.out)

mod.fit=predict.gam(m.NH4_TN.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NH4_TN.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

NH4_TN.out.pdat=GAM.SigChange.fun(m.NH4_TN.out,NH4_TN.out.pdat)
unique(subset(NH4_TN.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NH4_TN.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NH4_TN.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NH4_TN.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NH4_TN.out.DOY.sig=ddply(NH4_TN.out.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NH4_TN.out,"NH4_TN")

# NOx_TN
plot(NOx_TN~Date,dat.out)
acf(subset(dat.out,is.na(NOx_TN)==F)$NOx_TN)
pacf(subset(dat.out,is.na(NOx_TN)==F)$NOx_TN)
lmtest::bgtest(NOx_TN~CY.d,data=dat.out)

m.NOx_TN.out=gam(NOx_TN~
                  s(DOY, bs = "cc",k=25) + 
                  s(CY,k=10)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(41,9)),
                data = dat.out, method = "REML")
summary(m.NOx_TN.out)

layout(matrix(1:4,2,2));gam.check(m.NOx_TN.out,pch=21)
dev.off()
draw(m.NOx_TN.out)
testUniformity(simulateResiduals(m.NOx_TN.out))
acf(m.NOx_TN.out$residuals)

pred.org=predict.gam(m.NOx_TN.out,type="terms")
partial.resids.NOx_TN.out<-pred.org+residuals(m.NOx_TN.out)

mod.fit=predict.gam(m.NOx_TN.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
NOx_TN.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

NOx_TN.out.pdat=GAM.SigChange.fun(m.NOx_TN.out,NOx_TN.out.pdat)
unique(subset(NOx_TN.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(NOx_TN.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(NOx_TN.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(NOx_TN.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
NOx_TN.out.DOY.sig=ddply(NOx_TN.out.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.NOx_TN.out,"NOx_TN")

# Colour_PCU
plot(Colour_PCU~Date,dat.out)
range(subset(dat.out, is.na(Colour_PCU)==F)$CY)
acf(subset(dat.out,is.na(Colour_PCU)==F)$Colour_PCU)
pacf(subset(dat.out,is.na(Colour_PCU)==F)$Colour_PCU)
lmtest::bgtest(Colour_PCU~CY.d,data=dat.out)

m.Colour_PCU.out=gam(Colour_PCU~
                      s(DOY, bs = "cc",k=20) + 
                      s(CY,k=3)+
                      ti(DOY,CY,bs = c('cc', 'tp'), k = c(20,3)),
                    data = dat.out, method = "REML")
summary(m.Colour_PCU.out)

layout(matrix(1:4,2,2));gam.check(m.Colour_PCU.out,pch=21)
dev.off()
draw(m.Colour_PCU.out)
testUniformity(simulateResiduals(m.Colour_PCU.out))
acf(m.Colour_PCU.out$residuals)

pred.org=predict.gam(m.Colour_PCU.out,type="terms")
partial.resids.Colour_PCU.out<-pred.org+residuals(m.Colour_PCU.out)

mod.fit=predict.gam(m.Colour_PCU.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Colour_PCU.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

Colour_PCU.out.pdat=GAM.SigChange.fun(m.Colour_PCU.out,Colour_PCU.out.pdat)
unique(subset(Colour_PCU.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Colour_PCU.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Colour_PCU.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Colour_PCU.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Colour_PCU.out.DOY.sig=ddply(Colour_PCU.out.pdat,c('DOY'),summarise,
                            fit=mean(fit.DOY),
                            UCI=mean(upper.DOY,na.rm=T),
                            LCI=mean(lower.DOY,na.rm=T),
                            dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                            dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Colour_PCU.out,"Colour_PCU")

# Temp.C
plot(Temp.C~Date,dat.out)
range(subset(dat.out, is.na(Temp.C)==F)$CY)
acf(subset(dat.out,is.na(Temp.C)==F)$Temp.C)
pacf(subset(dat.out,is.na(Temp.C)==F)$Temp.C)
lmtest::bgtest(Temp.C~CY.d,data=dat.out)

m.Temp.C.out=gam(Temp.C~
                  s(DOY, bs = "cc",k=10) + 
                  s(CY,k=11)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(20,11)),
                data = dat.out, method = "REML")
summary(m.Temp.C.out)

layout(matrix(1:4,2,2));gam.check(m.Temp.C.out,pch=21)
dev.off()
draw(m.Temp.C.out)
testUniformity(simulateResiduals(m.Temp.C.out))
acf(m.Temp.C.out$residuals)

pred.org=predict.gam(m.Temp.C.out,type="terms")
partial.resids.Temp.C.out<-pred.org+residuals(m.Temp.C.out)

mod.fit=predict.gam(m.Temp.C.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Temp.C.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

Temp.C.out.pdat=GAM.SigChange.fun(m.Temp.C.out,Temp.C.out.pdat)
unique(subset(Temp.C.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Temp.C.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Temp.C.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Temp.C.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Temp.C.out.DOY.sig=ddply(Temp.C.out.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Temp.C.out,"Temp.C")

# Cond
plot(Cond~Date,dat.out)
range(subset(dat.out, is.na(Cond)==F)$CY)
acf(subset(dat.out,is.na(Cond)==F)$Cond)
pacf(subset(dat.out,is.na(Cond)==F)$Cond)
lmtest::bgtest(Cond~CY.d,data=dat.out)

m.Cond.out=gam(Cond~
                s(DOY, bs = "cc",k=10) + 
                s(CY,k=11)+
                ti(DOY,CY,bs = c('cc', 'tp'), k = c(30,11)),
              data = dat.out, method = "REML")
summary(m.Cond.out)

layout(matrix(1:4,2,2));gam.check(m.Cond.out,pch=21)
dev.off()
draw(m.Cond.out)
testUniformity(simulateResiduals(m.Cond.out))
acf(m.Cond.out$residuals)

pred.org=predict.gam(m.Cond.out,type="terms")
partial.resids.Cond.out<-pred.org+residuals(m.Cond.out)

mod.fit=predict.gam(m.Cond.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Cond.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

Cond.out.pdat=GAM.SigChange.fun(m.Cond.out,Cond.out.pdat)
unique(subset(Cond.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Cond.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Cond.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Cond.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Cond.out.DOY.sig=ddply(Cond.out.pdat,c('DOY'),summarise,
                      fit=mean(fit.DOY),
                      UCI=mean(upper.DOY,na.rm=T),
                      LCI=mean(lower.DOY,na.rm=T),
                      dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                      dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Cond.out,"Cond")

# DO.per
plot(DO.per~Date,dat.out)
range(subset(dat.out, is.na(DO.per)==F)$CY)
acf(subset(dat.out,is.na(DO.per)==F)$DO.per)
pacf(subset(dat.out,is.na(DO.per)==F)$DO.per)
lmtest::bgtest(DO.per~CY.d,data=dat.out)

m.DO.per.out=gam(DO.per~
                  s(DOY, bs = "cc",k=10) + 
                  s(CY,k=8)+
                  ti(DOY,CY,bs = c('cc', 'tp'), k = c(40,8)),
                data = dat.out, method = "REML")
summary(m.DO.per.out)

layout(matrix(1:4,2,2));gam.check(m.DO.per.out,pch=21)
dev.off()
draw(m.DO.per.out)
testUniformity(simulateResiduals(m.DO.per.out))
acf(m.DO.per.out$residuals)

pred.org=predict.gam(m.DO.per.out,type="terms")
partial.resids.DO.per.out<-pred.org+residuals(m.DO.per.out)

mod.fit=predict.gam(m.DO.per.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
DO.per.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

DO.per.out.pdat=GAM.SigChange.fun(m.DO.per.out,DO.per.out.pdat)
unique(subset(DO.per.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(DO.per.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(DO.per.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(DO.per.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
DO.per.out.DOY.sig=ddply(DO.per.out.pdat,c('DOY'),summarise,
                        fit=mean(fit.DOY),
                        UCI=mean(upper.DOY,na.rm=T),
                        LCI=mean(lower.DOY,na.rm=T),
                        dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                        dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.DO.per.out,"DO.per")

# pH
dat.out$H=10^dat.out$pH
plot(pH~Date,dat.out)
range(subset(dat.out, is.na(pH)==F)$CY)
acf(subset(dat.out,is.na(pH)==F)$pH)
pacf(subset(dat.out,is.na(pH)==F)$pH)
lmtest::bgtest(pH~CY.d,data=dat.out)

m.pH.out=gam(pH~
              s(DOY, bs = "cc",k=30) + 
              s(CY,k=4)+
              ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,4)),
            data = dat.out, method = "REML")
summary(m.pH.out)

layout(matrix(1:4,2,2));gam.check(m.pH.out,pch=21)
dev.off()
draw(m.pH.out)
testUniformity(simulateResiduals(m.pH.out))
acf(m.pH.out$residuals)

pred.org=predict.gam(m.pH.out,type="terms")
partial.resids.pH.out<-pred.org+residuals(m.pH.out)

mod.fit=predict.gam(m.pH.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
pH.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

pH.out.pdat=GAM.SigChange.fun(m.pH.out,pH.out.pdat)
unique(subset(pH.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(pH.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(pH.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(pH.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
pH.out.DOY.sig=ddply(pH.out.pdat,c('DOY'),summarise,
                    fit=mean(fit.DOY),
                    UCI=mean(upper.DOY,na.rm=T),
                    LCI=mean(lower.DOY,na.rm=T),
                    dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                    dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.pH.out,"pH")

# Chla.ugL
plot(Chla.ugL~Date,dat.out)

# Phyco.ugL
plot(Phyco.ugL~Date,dat.out)
range(subset(dat.out, is.na(Phyco.ugL)==F)$CY)
acf(subset(dat.out,is.na(Phyco.ugL)==F)$Phyco.ugL)
pacf(subset(dat.out,is.na(Phyco.ugL)==F)$Phyco.ugL)
lmtest::bgtest(Phyco.ugL~CY.d,data=dat.out)

m.Phyco.ugL.out=gam(log(Phyco.ugL)~
                     s(DOY, bs = "cc",k=30) + 
                     s(CY,k=5)+
                     ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,5)),
                   data = dat.out, method = "REML")
summary(m.Phyco.ugL.out)

layout(matrix(1:4,2,2));gam.check(m.Phyco.ugL.out,pch=21)
dev.off()
draw(m.Phyco.ugL.out)
testUniformity(simulateResiduals(m.Phyco.ugL.out))
acf(m.Phyco.ugL.out$residuals)

pred.org=predict.gam(m.Phyco.ugL.out,type="terms")
partial.resids.Phyco.ugL.out<-pred.org+residuals(m.Phyco.ugL.out)

mod.fit=predict.gam(m.Phyco.ugL.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
Phyco.ugL.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

Phyco.ugL.out.pdat=GAM.SigChange.fun(m.Phyco.ugL.out,Phyco.ugL.out.pdat)
unique(subset(Phyco.ugL.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(Phyco.ugL.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(Phyco.ugL.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(Phyco.ugL.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
Phyco.ugL.out.DOY.sig=ddply(Phyco.ugL.out.pdat,c('DOY'),summarise,
                           fit=mean(fit.DOY),
                           UCI=mean(upper.DOY,na.rm=T),
                           LCI=mean(lower.DOY,na.rm=T),
                           dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                           dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.Phyco.ugL.out,"Phyco.ugL")

# TChl.ugL
plot(TChl.ugL~Date,dat.out)
range(subset(dat.out, is.na(TChl.ugL)==F)$CY)
acf(subset(dat.out,is.na(TChl.ugL)==F)$TChl.ugL)
pacf(subset(dat.out,is.na(TChl.ugL)==F)$TChl.ugL)
lmtest::bgtest(TChl.ugL~CY.d,data=dat.out)

m.TChl.ugL.out=gam(log(TChl.ugL)~
                    s(DOY, bs = "cc",k=25) + 
                    s(CY,k=5)+
                    ti(DOY,CY,bs = c('cc', 'tp'), k = c(25,5)),
                  data = dat.out, method = "REML")
summary(m.TChl.ugL.out)

layout(matrix(1:4,2,2));gam.check(m.TChl.ugL.out,pch=21)
dev.off()
draw(m.TChl.ugL.out)
testUniformity(simulateResiduals(m.TChl.ugL.out))
acf(m.TChl.ugL.out$residuals)

pred.org=predict.gam(m.TChl.ugL.out,type="terms")
partial.resids.TChl.ugL.out<-pred.org+residuals(m.TChl.ugL.out)

mod.fit=predict.gam(m.TChl.ugL.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TChl.ugL.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

TChl.ugL.out.pdat=GAM.SigChange.fun(m.TChl.ugL.out,TChl.ugL.out.pdat)
unique(subset(TChl.ugL.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TChl.ugL.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TChl.ugL.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TChl.ugL.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TChl.ugL.out.DOY.sig=ddply(TChl.ugL.out.pdat,c('DOY'),summarise,
                          fit=mean(fit.DOY),
                          UCI=mean(upper.DOY,na.rm=T),
                          LCI=mean(lower.DOY,na.rm=T),
                          dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                          dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TChl.ugL.out,"TChl.ugL")

# TSI
plot(TSI_mean2~Date,dat.out)
range(subset(dat.out, is.na(TSI_mean2)==F)$CY)
acf(subset(dat.out,is.na(TSI_mean2)==F)$TSI_mean2)
pacf(subset(dat.out,is.na(TSI_mean2)==F)$TSI_mean2)
lmtest::bgtest(TSI_mean2~CY.d,data=dat.out)

m.TSI.out=gam(log(TSI_mean2)~
                     s(DOY, bs = "cc",k=30) + 
                     s(CY,k=11)+
                     ti(DOY,CY,bs = c('cc', 'tp'), k = c(35,11)),
                   data = dat.out, method = "REML")
summary(m.TSI.out)

layout(matrix(1:4,2,2));gam.check(m.TSI.out,pch=21)
dev.off()
draw(m.TSI.out)
testUniformity(simulateResiduals(m.TSI.out))
acf(m.TSI.out$residuals)

pred.org=predict.gam(m.TSI.out,type="terms")
partial.resids.TSI.out<-pred.org+residuals(m.TSI.out)

mod.fit=predict.gam(m.TSI.out,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("DOY","CY","DOYCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("DOY","CY","DOYCY"),sep=".")
TSI.out.pdat=cbind(pdat,tmp.fit,tmp.SE)

TSI.out.pdat=GAM.SigChange.fun(m.TSI.out,TSI.out.pdat)
unique(subset(TSI.out.pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(TSI.out.pdat,is.na(dsig.CY.decr)==F)$CY)
unique(subset(TSI.out.pdat,is.na(dsig.DOY.incr)==F)$DOY)
unique(subset(TSI.out.pdat,is.na(dsig.DOY.decr)==F)$DOY)
TSI.out.DOY.sig=ddply(TSI.out.pdat,c('DOY'),summarise,
                           fit=mean(fit.DOY),
                           UCI=mean(upper.DOY,na.rm=T),
                           LCI=mean(lower.DOY,na.rm=T),
                           dsig.incr=mean(dsig.DOY.incr,na.rm=T),
                           dsig.decre=mean(dsig.DOY.decr,na.rm=T))

gam.sum.table(m.TSI.out,"TSI")

# workspace image ---------------------------------------------------------
## Save workspace image to this point
# save.image(file=paste0(export.path,"PLSF_1/PLSF_GAM.RData"))
# load(paste0(export.path,"PLSF_1/PLSF_GAM.RData"))

# GAM table ---------------------------------------------------------------

gam.nut.in.rslt=rbind(
  gam.sum.table(m.TP.in,"TP"),
  gam.sum.table(m.DP.in,"DP"),
  gam.sum.table(m.SRP.in,"SRP"),
  gam.sum.table(m.PP.in,"PP"),
  gam.sum.table(m.DOP.in,"DOP"),
  gam.sum.table(m.TN.in,"TN"),
  gam.sum.table(m.TKN.in,"TKN"),
  gam.sum.table(m.NOx.in,"NOx"),
  gam.sum.table(m.NH4.in,"NH4"),
  gam.sum.table(m.DIN.in,"DIN"),
  gam.sum.table(m.TON.in,"TON"),
  gam.sum.table(m.TN_TP.in,"TN:TP"),
  gam.sum.table(m.DIN_SRP.in,"DIN:SRP"),
  gam.sum.table(m.PP_TP.in,"%PP"),
  gam.sum.table(m.DP_TP.in,"%DP"),
  gam.sum.table(m.SRP_TP.in,"%SRP"),
  gam.sum.table(m.NH4_TN.in,"%NH4"),
  gam.sum.table(m.NOx_TN.in,"%NOx")
)
gam.nut.in.rslt$Site="Lake Inlet"
bks=c(-8,-1,-0.5,0.5,1,8)
bks.vals=c("high","mod","sym","mod","high")
gam.nut.in.rslt$skew.cat=bks.vals[findInterval(gam.nut.in.rslt$res.skew,bks)]
gam.nut.in.rslt

gam.other.in.rslt=rbind(
  gam.sum.table(m.Colour_PCU.in,"Colour"),
  gam.sum.table(m.Temp.C.in,"Temp"),
  gam.sum.table(m.Cond.in,"SPC"),
  gam.sum.table(m.DO.per.in,"DO"),
  gam.sum.table(m.pH.in,"pH"),
  gam.sum.table(m.Phyco.ugL.in,"Phycocyanin"),
  gam.sum.table(m.TChl.ugL.in,"Chlorophyll")
)
gam.other.in.rslt$Site="Lake Inlet"
gam.other.in.rslt$skew.cat=bks.vals[findInterval(gam.other.in.rslt$res.skew,bks)]
gam.other.in.rslt

gam.nut.out.rslt=rbind(
  gam.sum.table(m.TP.out,"TP"),
  gam.sum.table(m.DP.out,"DP"),
  gam.sum.table(m.SRP.out,"SRP"),
  gam.sum.table(m.PP.out,"PP"),
  gam.sum.table(m.DOP.out,"DOP"),
  gam.sum.table(m.TN.out,"TN"),
  gam.sum.table(m.TKN.out,"TKN"),
  gam.sum.table(m.NOx.out,"NOx"),
  gam.sum.table(m.NH4.out,"NH4"),
  gam.sum.table(m.DIN.out,"DIN"),
  gam.sum.table(m.TON.out,"TON"),
  gam.sum.table(m.TN_TP.out,"TN:TP"),
  gam.sum.table(m.DIN_SRP.out,"DIN:SRP"),
  gam.sum.table(m.PP_TP.out,"%PP"),
  gam.sum.table(m.DP_TP.out,"%DP"),
  gam.sum.table(m.SRP_TP.out,"%SRP"),
  gam.sum.table(m.NH4_TN.out,"%NH4"),
  gam.sum.table(m.NOx_TN.out,"%NOx")
)
gam.nut.out.rslt$Site="Lake Outlet"
gam.nut.out.rslt$skew.cat=bks.vals[findInterval(gam.nut.out.rslt$res.skew,bks)]
gam.nut.out.rslt

gam.other.out.rslt=rbind(
  gam.sum.table(m.Colour_PCU.out,"Colour"),
  gam.sum.table(m.Temp.C.out,"Temp"),
  gam.sum.table(m.Cond.out,"SPC"),
  gam.sum.table(m.DO.per.out,"DO"),
  gam.sum.table(m.pH.out,"pH"),
  gam.sum.table(m.Phyco.ugL.out,"Phycocyanin"),
  gam.sum.table(m.TChl.ugL.out,"Chlorophyll")
)
gam.other.out.rslt$Site="Lake Outlet"
gam.other.out.rslt$skew.cat=bks.vals[findInterval(gam.other.out.rslt$res.skew,bks)]
gam.other.out.rslt

gam.TSI.in.rslt=gam.sum.table(m.TSI.in,"TSI")
gam.TSI.in.rslt$Site="Lake Inlet"
gam.TSI.in.rslt$skew.cat=bks.vals[findInterval(gam.TSI.in.rslt$res.skew,bks)]
gam.TSI.out.rslt=gam.sum.table(m.TSI.out,"TSI")
gam.TSI.out.rslt$Site="Lake Outlet"
gam.TSI.out.rslt$skew.cat=bks.vals[findInterval(gam.TSI.out.rslt$res.skew,bks)]


vars=c("Site","param","N.value","Dev", 
       "DOY.edf", "DOY.Fval", "DOY.pval", 
       "CY.edf", "CY.Fval", "CY.pval", 
       "DOYCY.edf", "DOYCY.Fval", "DOYCY.pval","trans","skew.cat")

rbind(gam.nut.in.rslt[,vars],gam.nut.out.rslt[,vars])%>%
  flextable(col_keys=vars[1:(length(vars)-2)])%>%
  colformat_double(j="Dev",digits=2)%>%
  colformat_double(j="DOY.edf",digits=2)%>%
  colformat_double(j="DOY.Fval",digits=2)%>%
  colformat_double(j="DOY.pval",i=~DOY.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="DOY.pval",i=~DOY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="DOY.pval",i=~DOY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="DOY.pval",i=~DOY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="DOY.pval",i=~DOY.pval<0.05)%>%
  # compose(j="DOY.edf",i=~DOY.edf<0.01,value=as_paragraph('< 0.01'))%>%
  # compose(j="DOY.Fval",i=~DOY.Fval<0.01,value=as_paragraph('< 0.01'))%>%
  bold(j="DOY.pval",i=~DOY.pval<0.01)%>%
  colformat_double(j="CY.edf",digits=2)%>%
  colformat_double(j="CY.Fval",digits=2)%>%
  colformat_double(j="CY.pval",i=~CY.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="CY.pval",i=~CY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="CY.pval",i=~CY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="CY.pval",i=~CY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="CY.pval",i=~CY.pval<0.05)%>%
  bold(j="CY.pval",i=~CY.pval<0.01)%>%
  colformat_double(j="DOYCY.edf",digits=2)%>%
  compose(j="DOYCY.edf",i=~DOYCY.edf<0.01,value=as_paragraph('< 0.01'))%>%
  colformat_double(j="DOYCY.Fval",digits=2)%>%
  colformat_double(j="DOYCY.pval",i=~DOYCY.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="DOYCY.pval",i=~DOYCY.pval<0.05)%>%
  bold(j="DOYCY.pval",i=~DOYCY.pval<0.01)%>%
  set_header_labels("param"="Parameter",
                    "N.value"="N",
                    "Dev"="Deviance\nExplained", 
                    "DOY.edf"="edf", 
                    "DOY.Fval"="F",
                    "DOY.pval"="\u03C1-value", 
                    "CY.edf"="edf",
                    "CY.Fval"="F",
                    "CY.pval"="\u03C1-value", 
                    "DOYCY.edf"="edf", 
                    "DOYCY.Fval"="F", 
                    "DOYCY.pval"="\u03C1-value"
  )%>%
  add_header(
    "DOY.edf"="Seasonal Trend\n(DOY)", 
    "DOY.Fval"="Seasonal Trend\n(DOY)",
    "DOY.pval"="Seasonal Trend\n(DOY)",
    "CY.edf"="Annual Trend\n(CY)",
    "CY.Fval"="Annual Trend\n(CY)",
    "CY.pval"="Annual Trend\n(CY)", 
    "DOYCY.edf"="Long-Term Trend\n(DOY,CY)", 
    "DOYCY.Fval"="Long-Term Trend\n(DOY,CY)", 
    "DOYCY.pval"="Long-Term Trend\n(DOY,CY)"
  )%>%merge_h(part="header")%>%align(align="center",part="header")%>%
  merge_v(j="Site")%>%
  fix_border_issues()%>%
  vline(j=c(4,7,10))%>%
  hline(i=18)%>%
  align(j=2:13,part="all",align="center")%>%
  width(width=c(0.7,0.9,0.25,1,0.5,0.5,0.75,0.5,0.5,0.75,0.5,0.5,0.75))%>%
  padding(padding=1.5,part="all")%>%
  footnote(j="param",i=~trans=="log",ref_symbols = " * ",part="body",
           value=as_paragraph("Log-transformed Data"),inline=T)%>%
  # footnote(j="param",i=~skew.cat!="sym",ref_symbols = " \u01C2 ",part="body",
  #          value=as_paragraph("skewed residuals"),inline=T)%>%
  footnote(j=3,ref_symbols = " 1 ",part="header",
           value=as_paragraph("Sample Size"),inline=T)%>%
  footnote(j=4,ref_symbols = " 2 ",part="header",
           value=as_paragraph("Similar to R\u00B2"),inline=T)%>%
  footnote(j="param",i=~param%in%c("PP","DOP","DIN","TON"),ref_symbols = " 3 ",part="body",
           value=as_paragraph("PP = TP - DP; DOP = DP - SRP; DIN = NH4+NOx; TON = TKN-NH4"))%>%
  footnote(j="param",i=~param%in%c("TN:TP","DIN:SRP"),ref_symbols = " 4 ",part="body",
           value=as_paragraph("Molar ratio; No transformation prior to analysis"))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header") #%>%print("docx")

rbind(gam.other.in.rslt[,vars],gam.other.out.rslt[,vars])%>%
  flextable(col_keys=vars[1:(length(vars)-2)])%>%
  colformat_double(j="Dev",digits=2,big.mark="")%>%
  colformat_double(j="DOY.edf",digits=2,big.mark="")%>%
  colformat_double(j="DOY.Fval",digits=2,big.mark="")%>%
  colformat_double(j="DOY.pval",i=~DOY.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="DOY.pval",i=~DOY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="DOY.pval",i=~DOY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="DOY.pval",i=~DOY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="DOY.pval",i=~DOY.pval<0.05)%>%
  # compose(j="DOY.edf",i=~DOY.edf<0.01,value=as_paragraph('< 0.01'))%>%
  # compose(j="DOY.Fval",i=~DOY.Fval<0.01,value=as_paragraph('< 0.01'))%>%
  bold(j="DOY.pval",i=~DOY.pval<0.01)%>%
  colformat_double(j="CY.edf",digits=2,big.mark="")%>%
  colformat_double(j="CY.Fval",digits=2,big.mark="")%>%
  colformat_double(j="CY.pval",i=~CY.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="CY.pval",i=~CY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="CY.pval",i=~CY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="CY.pval",i=~CY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="CY.pval",i=~CY.pval<0.05)%>%
  bold(j="CY.pval",i=~CY.pval<0.01)%>%
  colformat_double(j="DOYCY.edf",digits=2,big.mark="")%>%
  colformat_double(j="DOYCY.Fval",digits=2,big.mark="")%>%
  colformat_double(j="DOYCY.pval",i=~DOYCY.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="DOYCY.pval",i=~DOYCY.pval<0.05)%>%
  bold(j="DOYCY.pval",i=~DOYCY.pval<0.01)%>%
  set_header_labels("param"="Parameter",
                    "N.value"="N",
                    "Dev"="Deviance\nExplained", 
                    "DOY.edf"="edf", 
                    "DOY.Fval"="F",
                    "DOY.pval"="\u03C1-value", 
                    "CY.edf"="edf",
                    "CY.Fval"="F",
                    "CY.pval"="\u03C1-value", 
                    "DOYCY.edf"="edf", 
                    "DOYCY.Fval"="F", 
                    "DOYCY.pval"="\u03C1-value"
  )%>%
  add_header(
    "DOY.edf"="Seasonal Trend\n(DOY)", 
    "DOY.Fval"="Seasonal Trend\n(DOY)",
    "DOY.pval"="Seasonal Trend\n(DOY)",
    "CY.edf"="Annual Trend\n(CY)",
    "CY.Fval"="Annual Trend\n(CY)",
    "CY.pval"="Annual Trend\n(CY)", 
    "DOYCY.edf"="Long-Term Trend\n(DOY,CY)", 
    "DOYCY.Fval"="Long-Term Trend\n(DOY,CY)", 
    "DOYCY.pval"="Long-Term Trend\n(DOY,CY)"
  )%>%merge_h(part="header")%>%align(align="center",part="header")%>%
  merge_v(j="Site")%>%
  fix_border_issues()%>%
  vline(j=c(4,7,10))%>%
  hline(i=7)%>%
  align(j=2:13,part="all",align="center")%>%
  width(width=c(0.7,1,0.25,1,0.5,0.5,0.75,0.5,0.5,0.75,0.5,0.5,0.75))%>%
  padding(padding=1.5,part="all")%>%
  footnote(j="param",i=~trans=="log",ref_symbols = " * ",part="body",
           value=as_paragraph("Log-transformed Data"),inline=T)%>%
  # footnote(j="param",i=~skew.cat!="sym",ref_symbols = " \u01C2 ",part="body",
  #           value=as_paragraph("skewed residuals"),inline=T)%>%
  footnote(j=3,ref_symbols = " 1 ",part="header",
           value=as_paragraph("Sample Size"),inline=T)%>%
  footnote(j=4,ref_symbols = " 2 ",part="header",
           value=as_paragraph("Similar to R\u00B2"),inline=T)%>%
  footnote(j="param",i=~param%in%c("Phycocyanin","Chlorophyll"),ref_symbols = " 3 ",part="body",
           value=as_paragraph("Sonde measured pigments"))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header") %>%print("docx")

rbind(gam.TSI.in.rslt[,vars],gam.TSI.out.rslt[,vars])%>%
  flextable(col_keys=vars[1:(length(vars)-2)])%>%
  colformat_double(j="Dev",digits=2,big.mark="")%>%
  colformat_double(j="DOY.edf",digits=2,big.mark="")%>%
  colformat_double(j="DOY.Fval",digits=2,big.mark="")%>%
  colformat_double(j="DOY.pval",i=~DOY.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="DOY.pval",i=~DOY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="DOY.pval",i=~DOY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="DOY.pval",i=~DOY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="DOY.pval",i=~DOY.pval<0.05)%>%
  # compose(j="DOY.edf",i=~DOY.edf<0.01,value=as_paragraph('< 0.01'))%>%
  # compose(j="DOY.Fval",i=~DOY.Fval<0.01,value=as_paragraph('< 0.01'))%>%
  bold(j="DOY.pval",i=~DOY.pval<0.01)%>%
  colformat_double(j="CY.edf",digits=2,big.mark="")%>%
  colformat_double(j="CY.Fval",digits=2,big.mark="")%>%
  colformat_double(j="CY.pval",i=~CY.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="CY.pval",i=~CY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="CY.pval",i=~CY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="CY.pval",i=~CY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="CY.pval",i=~CY.pval<0.05)%>%
  bold(j="CY.pval",i=~CY.pval<0.01)%>%
  colformat_double(j="DOYCY.edf",digits=2,big.mark="")%>%
  colformat_double(j="DOYCY.Fval",digits=2,big.mark="")%>%
  colformat_double(j="DOYCY.pval",i=~DOYCY.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="DOYCY.pval",i=~DOYCY.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="DOYCY.pval",i=~DOYCY.pval<0.05)%>%
  bold(j="DOYCY.pval",i=~DOYCY.pval<0.01)%>%
  set_header_labels("param"="Parameter",
                    "N.value"="N",
                    "Dev"="Deviance\nExplained", 
                    "DOY.edf"="edf", 
                    "DOY.Fval"="F",
                    "DOY.pval"="\u03C1-value", 
                    "CY.edf"="edf",
                    "CY.Fval"="F",
                    "CY.pval"="\u03C1-value", 
                    "DOYCY.edf"="edf", 
                    "DOYCY.Fval"="F", 
                    "DOYCY.pval"="\u03C1-value"
  )%>%
  add_header(
    "DOY.edf"="Seasonal Trend\n(DOY)", 
    "DOY.Fval"="Seasonal Trend\n(DOY)",
    "DOY.pval"="Seasonal Trend\n(DOY)",
    "CY.edf"="Annual Trend\n(CY)",
    "CY.Fval"="Annual Trend\n(CY)",
    "CY.pval"="Annual Trend\n(CY)", 
    "DOYCY.edf"="Long-Term Trend\n(DOY,CY)", 
    "DOYCY.Fval"="Long-Term Trend\n(DOY,CY)", 
    "DOYCY.pval"="Long-Term Trend\n(DOY,CY)"
  )%>%merge_h(part="header")%>%align(align="center",part="header")%>%
  merge_v(j="Site")%>%
  fix_border_issues()%>%
  vline(j=c(4,7,10))%>%
  hline(i=1)%>%
  align(j=2:13,part="all",align="center")%>%
  width(width=c(0.7,1,0.25,1,0.5,0.5,0.75,0.5,0.5,0.75,0.5,0.5,0.75))%>%
  padding(padding=1.5,part="all")%>%
  footnote(j="param",i=~trans=="log",ref_symbols = " * ",part="body",
           value=as_paragraph("Log-transformed Data"),inline=T)%>%
  # footnote(j="param",i=~skew.cat!="sym",ref_symbols = " \u01C2 ",part="body",
  #           value=as_paragraph("skewed residuals"),inline=T)%>%
  footnote(j=3,ref_symbols = " 1 ",part="header",
           value=as_paragraph("Sample Size"),inline=T)%>%
  footnote(j=4,ref_symbols = " 2 ",part="header",
           value=as_paragraph("Similar to R\u00B2"),inline=T)%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header") %>%print("docx")

# sum stats ---------------------------------------------------------------
sites.vals=c("Godbout","Lake_Outlet")
idvars=c("Date","Site","CY","CY.d","DOY","month","ENKI","time","WY")
# paramvar.sum=names(wq.dat[,!(names(wq.dat)%in%idvars)])
# paramvar.sum=paramvar.sum[!(paramvar.sum%in%c("TPReversal","TPReversal2","TNReversal"))]
paramvar.sum=c("TP.ugL", "SRP.ugL", "DP.ugL", "PP.ugL","DOP.ugL",
               "TN.mgL","TKN.mgL", "NH4.mgL", "NOx.mgL", "Urea.mgL", "DON.mgL", 
               "SolN.mgL", "SolOC.mgL", "TOC.mgL", "pH", "Chla.ugL", "Cond", "Sal",
               "DO.per", "TDS.mgL", "Temp.C", "ORP.mV", "Resistivity.ohm", 
               "Phyco.ugL", "TChl.ugL", "Turb.NTU", "Colour_PCU", "Secchi_cm",
               "TN_TP", "TOC_TP", "TOC_TN", "DIN_SRP", "PP_TP", "DP_TP", 
               "SRP_TP", "NOx_TN", "NH4_TN",'TSI_mean2')
wq.dat.melt.all=melt(wq.dat[,c(idvars,paramvar.sum)],id.vars = idvars)
wq.dat.melt.all=subset(wq.dat.melt.all,is.na(value)==F)
# wq.dat.melt.all$value2=as.numeric(wq.dat.melt.all$value)

wq.dat.melt.all=merge(wq.dat.melt.all,
                      data.frame(expand.grid(Site=sites.vals,
                                             variable=paramvar.sum)),
                      c("Site","variable"),all.y=T)
                                 
                                

sum.stats=ddply(subset(wq.dat.melt.all,Site%in%sites.vals),c("Site","variable"),summarise,
                N.val=N.obs(value),
                med.value=median(value,na.rm=T),
      mean.value=mean(value,na.rm=T),
      SD.value=sd(value,na.rm=T),
      min.val=min(value,na.rm=T),max.val=max(value,na.rm=T))
sum.stats$min.val=with(sum.stats,ifelse(is.infinite(min.val),NA,min.val))
sum.stats$max.val=with(sum.stats,ifelse(is.infinite(max.val),NA,max.val))
# exvars=c("SolN.mgL","Urea.mgL","DON.mgL","TOC.mgL","SolOC.mgL","TOC_TP","TOC_TN")
#subset(sum.stats,variable%in%paramvars[!(paramvars%in%exvars)])%>%

sum.stats$min.val=with(sum.stats,ifelse(variable=="Temp.C"&round(min.val,1)==0,0,min.val))

vars1=c("TP.ugL", "SRP.ugL", "DP.ugL", "PP.ugL","DOP.ugL","Cond","ORP.mV","TSI_mean2")
vars2=c("DO.per","Temp.C","TN_TP", "TOC_TP", "TOC_TN", "DIN_SRP", "PP_TP", "DP_TP", 
        "SRP_TP", "NOx_TN", "NH4_TN","Colour_PCU")
vars3=c("Phyco.ugL","Resistivity.ohm")
sum.stats%>%
  flextable()%>%
  colformat_double(j=4:8,digits=2,na_str = "---")%>%
  colformat_double(j=4:8,i=~variable%in%vars1,digits=0,na_str = "---")%>%
  colformat_double(j=4:8,i=~variable%in%vars2,digits=1,na_str = "---")%>%
  colformat_double(j=4:8,i=~variable%in%vars3,digits=3,na_str = "---")%>%
  compose(j="variable",i=~variable=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DP.ugL",value=as_paragraph('DP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="PP.ugL",value=as_paragraph('PP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DOP.ugL",value=as_paragraph('DOP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('TN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="NOx.mgL",value=as_paragraph('NO\u2093 (mg L\u207B\u00B9)'))%>%
  # compose(j="variable",i=~variable=="NH4.mgL",value=as_paragraph('NH\u2084\u207A (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="NH4.mgL",value=as_paragraph('NH\u2084\u207A (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DIN.mgL",value=as_paragraph('DIN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TON.mgL",value=as_paragraph('TON (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TKN.mgL",value=as_paragraph('TKN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="Urea.mgL",value=as_paragraph('Urea (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DON.mgL",value=as_paragraph('DON (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="SolN.mgL",value=as_paragraph('Soluble N (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TOC.mgL",value=as_paragraph('TOC (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="SolOC.mgL",value=as_paragraph('DOC (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN_TP",value=as_paragraph('TN:TP (molar ratio)'))%>%
  compose(j="variable",i=~variable=="DIN_SRP",value=as_paragraph('DIN:SRP (molar ratio)'))%>%
  compose(j="variable",i=~variable=="TOC_TP",value=as_paragraph('TOC:TP (molar ratio)'))%>%
  compose(j="variable",i=~variable=="TOC_TN",value=as_paragraph('TOC:TN (molar ratio)'))%>%
  compose(j="variable",i=~variable=="PP_TP",value=as_paragraph('%PP of TP'))%>%
  compose(j="variable",i=~variable=="SRP_TP",value=as_paragraph('%SRP of TP'))%>%
  compose(j="variable",i=~variable=="DP_TP",value=as_paragraph('%DP of TP'))%>%
  compose(j="variable",i=~variable=="NH4_TN",value=as_paragraph('%NH\u2084 of TN'))%>%
  compose(j="variable",i=~variable=="NOx_TN",value=as_paragraph('%NO\u2093 of TN'))%>%
  compose(j="variable",i=~variable=="Chla.ugL",value=as_paragraph('Chlorophyll-a (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="Cond",value=as_paragraph('SPC (\u03BCS cm\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DO.per",value=as_paragraph('DO (% Sat)'))%>%
  compose(j="variable",i=~variable=="Colour_PCU",value=as_paragraph('Color (PCU)'))%>%
  compose(j="variable",i=~variable=="Turb.NTU",value=as_paragraph('Turbidity (NTU)'))%>%
  compose(j="variable",i=~variable=="Secchi_cm",value=as_paragraph('Secchi Depth (cm)'))%>%
  compose(j="variable",i=~variable=="Temp.C",value=as_paragraph('Water Temp (\u2103)'))%>%
  compose(j="variable",i=~variable=="Phyco.ugL",value=as_paragraph('Phycocyanin ',as_i("in-vivo"), ' (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TChl.ugL",value=as_paragraph('Chlorophyll ',as_i("in-vivo"), ' (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TDS.mgL",value=as_paragraph('TDS (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="ORP.mV",value=as_paragraph('ORP/Redox (mV)'))%>%
  compose(j="variable",i=~variable=="Resistivity.ohm",value=as_paragraph('Resistivity (\u2126)'))%>%
  compose(j="variable",i=~variable=="Sal",value=as_paragraph('Salinity (ppt)'))%>%
  compose(j="variable",i=~variable=="TSI_mean2",value=as_paragraph('TSI'))%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  # merge_v(j="param")%>%
  merge_v(j="Site")%>%
  set_header_labels("Site"="Site",
                    "variable" = "Parameter",
                    "N.val" = "Sample\nSize",
                    "med.value" = "Median",
                    "mean.value" = "Mean",
                    "SD.value" = "Std Dev",
                    "N.val"="N",
                    "min.val"="Min",
                    "max.val"="Max")%>%
  width(width=c(0.5,1.75,0.25,0.75,0.5,0.75,0.5,0.5))%>%
  #align(align="center",part="header")%>%
  align(j=1:2,align="left",part="all")%>%
  align(j=3:8,align="right",part="all")%>%
  padding(padding=1.5,part="all")%>%
  # hline(i=seq(2,54,2))%>%
  hline(i=38)%>%
  fix_border_issues()%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")# %>%print("docx")


# annual variance ---------------------------------------------------------
ann.var=ddply(wq.dat.melt,c("Site","CY","variable"),summarise,
              mean.value=mean(value,na.rm=T),
              SE.value=SE(value),
              sd.value=sd(value,na.rm = F),
              var.val=var(value,na.rm = F),
              N.val=N.obs(value),
              IQR.val=IQR(value,na.rm = F),
              CV.val=cv.per(value))


plot(sd.value~CY,subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),type="l");# same units as mean
plot(var.val~CY,subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),type="l"); # square of units
plot(IQR.val~CY,subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),type="l");# same units as mean
plot(CV.val~CY,subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),type="l");# 

plot(var.val~CY,subset(ann.var,Site=="Lake_Outlet"&variable=="TP.ugL"),type="l"); # square of units
plot(CV.val~CY,subset(ann.var,Site=="Lake_Outlet"&variable=="TP.ugL"),type="l")

cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]
# png(filename=paste0(plot.path,"PLSF_annualVar.png"),width=6.5,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,3,1.5,0.25));
layout(matrix(1:8,4,2,byrow = T),widths=c(1,1))

tmp.dat=dcast(subset(wq.dat.melt,Site%in%c("Godbout","Lake_Outlet")),
                     Site+Date+CY~variable,value.var="value",mean)
tmp.dat$CY.f=factor(tmp.dat$CY,levels=seq(2009,2020,1))
## TP boxplot
xlim.val=c(2009,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TP.ugL~CY.f,subset(tmp.dat,Site=="Godbout"),
        outline=F,ylim=ylim.val,col=cols[1],axes=F,ann=F)
x.pos=1:length(x$names)
axis_fun(1,x.pos[seq(1,length(x.pos),by.x)],x.pos,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"TP (\u03BCg L\u207B\u00B9)")
mtext(side=3,"Lake Inlet")

ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TP.ugL~CY.f,subset(tmp.dat,Site=="Lake_Outlet"),
          outline=F,ylim=ylim.val,col=cols[2],axes=F,ann=F)
x.pos=1:length(x$names)
axis_fun(1,x.pos[seq(1,length(x.pos),by.x)],x.pos,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Lake Outlet")

# TP variance 
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(sd.value~CY,ann.var,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F);
with(subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),pt_line(CY,sd.value,1,"dodgerblue1",1,21,"dodgerblue1"))
with(subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),
     pt_line(CY,IQR.val,2,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.col=adjustcolor("indianred1",0.5)))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topright",legend=c("St Dev","IQR"),
       pch=21,pt.bg=c("dodgerblue1",adjustcolor("indianred1",0.5)),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=c("black",adjustcolor("indianred1",0.5)),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=2,line=3,"TP (\u03BCg L\u207B\u00B9)")

ylim.val=c(0,250);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(sd.value~CY,ann.var,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F);
with(subset(ann.var,Site=="Lake_Outlet"&variable=="TP.ugL"),pt_line(CY,sd.value,1,"dodgerblue1",1,21,"dodgerblue1"))
with(subset(ann.var,Site=="Lake_Outlet"&variable=="TP.ugL"),
     pt_line(CY,IQR.val,2,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.col=adjustcolor("indianred1",0.5)))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

## TN boxplot
ylim.val=c(0,4);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TN.mgL~CY.f,subset(tmp.dat,Site=="Godbout"),
          outline=F,ylim=ylim.val,col=cols[1],axes=F,ann=F)
x.pos=1:length(x$names)
axis_fun(1,x.pos[seq(1,length(x.pos),by.x)],x.pos,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"TN (mg L\u207B\u00B9)")

x=boxplot(TN.mgL~CY.f,subset(tmp.dat,Site=="Lake_Outlet"),
          outline=F,ylim=ylim.val,col=cols[2],axes=F,ann=F)
x.pos=1:length(x$names)
axis_fun(1,x.pos[seq(1,length(x.pos),by.x)],x.pos,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

# TN variance 
ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(sd.value~CY,ann.var,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F);
with(subset(ann.var,Site=="Godbout"&variable=="TN.mgL"),pt_line(CY,sd.value,1,"dodgerblue1",1,21,"dodgerblue1"))
with(subset(ann.var,Site=="Godbout"&variable=="TN.mgL"),
     pt_line(CY,IQR.val,2,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.col=adjustcolor("indianred1",0.5)))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topright",legend=c("St Dev","IQR"),
       pch=21,pt.bg=c("dodgerblue1",adjustcolor("indianred1",0.5)),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=c("black",adjustcolor("indianred1",0.5)),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=2,line=3,"TN (mg L\u207B\u00B9)")
mtext(side=1,line=2,"Year")

plot(sd.value~CY,ann.var,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F);
with(subset(ann.var,Site=="Lake_Outlet"&variable=="TN.mgL"),pt_line(CY,sd.value,1,"dodgerblue1",1,21,"dodgerblue1"))
with(subset(ann.var,Site=="Lake_Outlet"&variable=="TN.mgL"),
     pt_line(CY,IQR.val,2,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.col=adjustcolor("indianred1",0.5)))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Year")
dev.off()

# png(filename=paste0(plot.path,"PLSF_annualVar_2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2,1.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,1))

xlim.val=c(2009,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(CV.val~CY,ann.var,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F);
with(subset(ann.var,Site=="Godbout"&variable=="TP.ugL"),pt_line(CY,CV.val,1,"dodgerblue1",1,21,"dodgerblue1"))
with(subset(ann.var,Site=="Lake_Outlet"&variable=="TP.ugL"),pt_line(CY,CV.val,1,"indianred1",1,21,"indianred1"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
legend("topleft",legend=c("Lake Inlet","Lake Outlet"),
       pch=21,pt.bg=c("dodgerblue1","indianred1"),pt.cex = 1.5,
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=3,"Total Phosphorus")
mtext(side=2,line=2,"Coefficient of Variance (%)")

plot(CV.val~CY,ann.var,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F);
with(subset(ann.var,Site=="Godbout"&variable=="TN.mgL"),pt_line(CY,CV.val,1,"dodgerblue1",1,21,"dodgerblue1"))
with(subset(ann.var,Site=="Lake_Outlet"&variable=="TN.mgL"),pt_line(CY,CV.val,1,"indianred1",1,21,"indianred1"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,"Total Nitrogen")
dev.off()

# Trend Analyses ----------------------------------------------------------
# head(wq.dat.melt)
# plot(value~CY,subset(wq.dat.melt,variable=="pH"))
# plot(value~CY,subset(wq.dat.melt,variable=="TChl.ugL"))
# plot(value~CY,subset(wq.dat.melt,variable=="Phyco.ugL"))
# plot(value~CY,subset(wq.dat.melt,variable=="Colour_PCU"))
# plot(value~CY,subset(wq.dat.melt,variable=="DO.per"))
# plot(value~CY,subset(wq.dat.melt,variable=="Secchi_cm"))

month.mean.dat=ddply(wq.dat.melt,c("Site","month","CY","variable"),summarise,
                     mean.value=mean(value,na.rm=T),
                     SE.value=SE(value),
                     sd.value=sd(value),
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
tmp=subset(wq.dat,Site=="Lake_Outlet"&is.na(TP.ugL)==F)
pt.rslt=pettitt.test(tmp$TP.ugL);pt.rslt
tmp[pt.rslt$estimate,]
data.frame(stat=as.numeric(pt.rslt$statistic),
           pvalue=as.numeric(pt.rslt$p.value),
           chge_row=as.numeric(pt.rslt$estimate))

# vars=c("TP.ugL","DP.ugL","SRP.ugL","PP.ugL","DOP.ugL",
#        "TN.mgL","NOx.mgL","NH4.mgL","DIN.mgL","TON.mgL","TKN.mgL",
#        "TN_TP","DIN_SRP")
# mmkh.trend=ddply(subset(wq.dat.melt,variable%in%vars),c("Site","variable"),summarise,
#                  N.val=N.obs(value),
#                  Z_correct=as.numeric(mmkh(value)[1]),
#                  new_pvalue=as.numeric(mmkh(value)[2]),
#                  Z_org=as.numeric(mmkh(value)[4]),
#                  org_pvalue=as.numeric(mmkh(value)[5]),
#                  tau=as.numeric(mmkh(value)[6]),
#                  sen_slope=as.numeric(mmkh(value)[7]))
# mmkh.trend

sites.vals=c("Godbout","Lake_Outlet")
paramvars=c("TP.ugL","PP.ugL", "DP.ugL", "SRP.ugL", "DOP.ugL",
            "TN.mgL","TON.mgL", "TKN.mgL","NOx.mgL","NH4.mgL","DIN.mgL","DON.mgL", 
            "TN_TP","DIN_SRP","DIN_TP","PP_TP","DP_TP","SRP_TP","NH4_TN","NOx_TN",
            "Cond","DO.per","Colour_PCU","Secchi_cm","Temp.C","pH","Phyco.ugL", "TChl.ugL","TSI_mean2")
pt.rslt.all=data.frame()
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat.melt,Site==sites.vals[i])
  for(j in 1:length(paramvars)){
    tmp.dat2=subset(tmp.dat,variable==paramvars[j])
    if(nrow(tmp.dat2)==0){
      rslt=data.frame(Site=sites.vals[i],param=paramvars[j],
                      stat=NA,
                      pvalue=NA,
                      chge_row=NA,
                      date.change=NA)
      pt.rslt.all=rbind(pt.rslt.all,rslt)
      next}
    pt=pettitt.test(tmp.dat2$value)
    date.change=if(as.numeric(pt$p.value)<0.05){tmp.dat2[min(pt$estimate),"CY.d"]}else{NA}
    rslt=data.frame(Site=sites.vals[i],param=paramvars[j],
                    stat=as.numeric(pt$statistic),
                    pvalue=as.numeric(pt$p.value),
                    chge_row=min(as.numeric(pt$estimate)),
                    date.change=date.change)
    pt.rslt.all=rbind(pt.rslt.all,rslt)
  }
}
pt.rslt.all
# subset(pt.rslt.all,param%in%vars)

trend.rslt.all=data.frame()
for(i in 1:length(sites.vals)){
  tmp.dat=subset(month.mean.dat,Site==sites.vals[i])
  for(j in 1:length(paramvars)){
    tmp.dat2=subset(tmp.dat,variable==paramvars[j])
    if(nrow(tmp.dat2)==0){
      rslt=data.frame(Site=sites.vals[i],param=paramvars[j],
                      N.val=length(tmp.dat2$mean.value),
                      chisq.stat=NA,
                      chisq.pval=NA,
                      z.stat=NA,
                      tau=NA,
                      z.pval=NA,
                      sen.slope=NA,
                      sen.int=NA)
      trend.rslt.all=rbind(trend.rslt.all,rslt)
      next}
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

subset(trend.rslt.all,param=="DIN_TP")
subset(trend.rslt.all,z.pval<0.05)

pt.trend.all=merge(trend.rslt.all,pt.rslt.all,c('Site',"param"))
pt.trend.all$date.change2=format(lubridate::date_decimal(pt.trend.all$date.change),"%b %d,%Y")

# pt.trend.all=subset(pt.trend.all,param%in%vars)
pt.trend.all$Site=factor(pt.trend.all$Site,levels=c("Godbout","Lake_Outlet"))
pt.trend.all$param=factor(pt.trend.all$param,levels=paramvars)
pt.trend.all=pt.trend.all[order(pt.trend.all$Site,pt.trend.all$param),]

subset(pt.trend.all,chisq.pval<0.05)

colvars=c("Site", "param", "N.val",
       # "chisq.stat", "chisq.pval", 
       "tau", "z.pval", "sen.slope",
       "stat","pvalue","date.change2")
pt.trend.all[,colvars]%>%
  flextable()%>%
  # colformat_double(j="chisq.stat",digits=1)%>%
  # colformat_double(j="chisq.pval",i=~chisq.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  # compose(j="chisq.pval",i=~chisq.pval<0.05,value=as_paragraph('< 0.05'))%>%
  colformat_double(j="tau",digits=2)%>%
  colformat_double(j="sen.slope",digits=3)%>%
  colformat_double(j="z.pval",i=~z.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="z.pval",i=~z.pval<0.05,value=as_paragraph('< 0.05'))%>%
  colformat_double(j="stat",digits=0,big.mark="")%>%
  colformat_double(j="z.pval",i=~z.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="z.pval",i=~z.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="z.pval",i=~z.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="z.pval",i=~z.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="z.pval",i=~z.pval<0.05)%>%
  colformat_char(j="date.change2",na_str=" --- ")%>%
  colformat_double(j="pvalue",i=~pvalue>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="pvalue",i=~pvalue<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="pvalue",i=~pvalue<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="pvalue",i=~pvalue<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="pvalue",i=~pvalue<0.05)%>%
  compose(j="param",i=~param=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="DP.ugL",value=as_paragraph('DP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="PP.ugL",value=as_paragraph('PP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="DOP.ugL",value=as_paragraph('DOP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="TN.mgL",value=as_paragraph('TN (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="NOx.mgL",value=as_paragraph('NO\u2093 (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="NH4.mgL",value=as_paragraph('NH\u2084 (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="DIN.mgL",value=as_paragraph('DIN (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="TON.mgL",value=as_paragraph('TON (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="TKN.mgL",value=as_paragraph('TKN (mg L\u207B\u00B9)'))%>%
  compose(j="param",i=~param=="TN_TP",value=as_paragraph('TN:TP\n(molar ratio)'))%>%
  compose(j="param",i=~param=="DIN_SRP",value=as_paragraph('DIN:SRP\n (molar ratio)'))%>%
  compose(j="param",i=~param=="PP_TP",value=as_paragraph('%PP of TP'))%>%
  compose(j="param",i=~param=="SRP_TP",value=as_paragraph('%SRP of TP'))%>%
  compose(j="param",i=~param=="DP_TP",value=as_paragraph('%DP of TP'))%>%
  compose(j="param",i=~param=="NH4_TN",value=as_paragraph('%NH4 of TN'))%>%
  compose(j="param",i=~param=="NOx_TN",value=as_paragraph('%NOx of TN'))%>%
  compose(j="param",i=~param=="Cond",value=as_paragraph('SPC'))%>%
  compose(j="param",i=~param=="DO.per",value=as_paragraph('DO % Sat'))%>%
  compose(j="param",i=~param=="Colour_PCU",value=as_paragraph('Colour'))%>%
  compose(j="param",i=~param=="Secchi_cm",value=as_paragraph('Secchi Depth'))%>%
  compose(j="param",i=~param=="Temp.C",value=as_paragraph('Water Temp'))%>%
  compose(j="param",i=~param=="Phyco.ugL",value=as_paragraph('Phycocyanin'))%>%
  compose(j="param",i=~param=="TChl.ugL",value=as_paragraph('Chlorophyll'))%>%
  compose(j="param",i=~param=="TSI_mean2",value=as_paragraph('TSI'))%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  # merge_v(j="param")%>%
  merge_v(j="Site")%>%
  set_header_labels("Site"="Site",
                    "param" = "Parameter",
                    "N.val" = "Sample\nSize",
                    "tau" = "Kendall's \u03C4",
                    "z.pval" = "\u03C1-value",
                    "sen.slope" = "Thiel-Sen Slope",
                    "stat"="Pettitt's\nStatistic",
                    "pvalue"="\u03C1-value",
                    "date.change2"="Date\nChange Point")%>%
  width(width=c(0.5,1.5,0.75,1,0.75,0.75,0.75,0.75,1))%>%
  align(align="center",part="header")%>%
  align(j=2:9,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  # hline(i=seq(2,54,2))%>%
  hline(i=29)%>%
  fix_border_issues()%>%
  footnote(j=3:6,ref_symbols = " 1 ",part="header",
           value=as_paragraph("Seasonal Mann-Kendall using month as season"))%>%
  footnote(j=7:9,ref_symbols = " 2 ",part="header",
           value=as_paragraph("Pettitt Change point analysis performed using weekly samples"))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")%>%print("docx")


subset(pt.trend.all,param=="TSI_mean2")[,colvars]%>%
  flextable()%>%
  colformat_double(j="tau",digits=2)%>%
  colformat_double(j="sen.slope",digits=3)%>%
  colformat_double(j="z.pval",i=~z.pval>0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="z.pval",i=~z.pval<0.05,value=as_paragraph('< 0.05'))%>%
  colformat_double(j="stat",digits=0,big.mark="")%>%
  colformat_double(j="z.pval",i=~z.pval>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="z.pval",i=~z.pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="z.pval",i=~z.pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="z.pval",i=~z.pval<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="z.pval",i=~z.pval<0.05)%>%
  colformat_char(j="date.change2",na_str=" --- ")%>%
  colformat_double(j="pvalue",i=~pvalue>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="pvalue",i=~pvalue<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="pvalue",i=~pvalue<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="pvalue",i=~pvalue<0.001,value=as_paragraph('< 0.001'))%>%
  italic(j="pvalue",i=~pvalue<0.05)%>%
  compose(j="param",i=~param=="TSI_mean2",value=as_paragraph('TSI'))%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  # merge_v(j="param")%>%
  merge_v(j="Site")%>%
  set_header_labels("Site"="Site",
                    "param" = "Parameter",
                    "N.val" = "Sample\nSize",
                    "tau" = "Kendall's \u03C4",
                    "z.pval" = "\u03C1-value",
                    "sen.slope" = "Thiel-Sen Slope",
                    "stat"="Pettitt's\nStatistic",
                    "pvalue"="\u03C1-value",
                    "date.change2"="Date\nChange Point")%>%
  width(width=c(0.5,1.5,0.75,1,0.75,0.75,0.75,0.75,1))%>%
  align(align="center",part="header")%>%
  align(j=2:9,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  # hline(i=seq(2,54,2))%>%
  hline(i=1)%>%
  fix_border_issues()%>%
  footnote(j=3:6,ref_symbols = " 1 ",part="header",
           value=as_paragraph("Seasonal Mann-Kendall using month as season"))%>%
  footnote(j=7:9,ref_symbols = " 2 ",part="header",
           value=as_paragraph("Pettitt Change point analysis performed using weekly samples"))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")#%>%print("docx")


# pettitt test confirm example
head(wq.dat.melt)
tmp.pt=subset(pt.rslt.all,Site=="Godbout"&param=="DOP.ugL")
tmp.dat=subset(wq.dat.melt,Site=="Godbout"&variable=='DOP.ugL')
tmp.dat$x.pt=with(tmp.dat,ifelse(CY.d<tmp.pt$date.change,1,2))

boxplot(value~x.pt,tmp.dat,outline=F)
kruskal.test(value~x.pt,tmp.dat)



# GAM Plot ----------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P.png"),width=12,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:40,5,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

# TP
{
ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.in$model$DOY,partial.resids.TP.in[,1],pch=19,col=inflow.col)
lines(fit~DOY,TP.in.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
# lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
# lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
# mtext(side=1,line=2,"DOY",cex=labs.cex)
mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)

ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.in$model$CY,partial.resids.TP.in[,2],pch=19,col=inflow.col)
lines(fit.CY~CY,TP.in.pdat,lwd=2)
lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
# lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
# lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
# mtext(side=1,line=2,"Year",cex=labs.cex)
mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
# mtext(side=2,line=1.5,"Effect",cex=labs.cex)

ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma1=with(TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TP.in.pdat$CY)),nrow=length(unique(TP.in.pdat$DOY))))
brk=10
breaks.val=classInt::classIntervals(TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
pal=hcl.colors(n=brk,alpha=0.75)
image(x=unique(TP.in.pdat$CY),y=unique(TP.in.pdat$DOY),z=t(tmp.ma1),
      breaks=breaks.val,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=unique(TP.in.pdat$CY),y=unique(TP.in.pdat$DOY),z=t(tmp.ma1),
        levels=breaks.val,nlevels=brk,
        add=T,drawlabels=F,lwd=0.75,col="white")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
mtext(side=2,line=1.75,"DOY",cex=labs.cex)
# mtext(side=1,line=2,"Year",cex=labs.cex)

legend_image=as.raster(matrix(rev(pal),ncol=1))
plot(0:1,0:1,ann=F,axes=F,type="n")
top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
text(x=x.max, y = c(bot.val,top.val), 
     labels =format(round(range(breaks.val),2)),
     cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)

ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.out$model$DOY,partial.resids.TP.out[,1],pch=19,col=outflow.col)
lines(fit~DOY,TP.out.DOY.sig,lwd=2)
lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
# mtext(side=1,line=2,"DOY",cex=labs.cex)
mtext(side=2,line=1.5,"Effect",cex=labs.cex)

ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(m.TP.out$model$CY,partial.resids.TP.out[,2],pch=19,col=outflow.col)
lines(fit.CY~CY,TP.out.pdat,lwd=2)
lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
# mtext(side=1,line=2,"Year",cex=labs.cex)
mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
# mtext(side=2,line=1.5,"Effect",cex=labs.cex)

ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma1=with(TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TP.out.pdat$CY)),nrow=length(unique(TP.out.pdat$DOY))))
brk=10
breaks.val=classInt::classIntervals(TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
pal=hcl.colors(n=brk,alpha=0.75)
image(x=unique(TP.out.pdat$CY),y=unique(TP.out.pdat$DOY),z=t(tmp.ma1),
      breaks=breaks.val,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=unique(TP.out.pdat$CY),y=unique(TP.out.pdat$DOY),z=t(tmp.ma1),
        levels=breaks.val,nlevels=brk,
        add=T,drawlabels=F,lwd=0.75,col="white")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
mtext(side=2,line=1.75,"DOY",cex=labs.cex)
# mtext(side=1,line=2,"Year",cex=labs.cex)

legend_image=as.raster(matrix(rev(pal),ncol=1))
plot(0:1,0:1,ann=F,axes=F,type="n")
top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
text(x=x.max, y = c(bot.val,top.val), 
     labels =format(round(range(breaks.val),2)),
     cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# PP
{
  ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.in$model$DOY,partial.resids.PP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,PP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.in$model$CY,partial.resids.PP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,PP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(PP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(PP.in.pdat$CY)),nrow=length(unique(PP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(PP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(PP.in.pdat$CY),y=unique(PP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(PP.in.pdat$CY),y=unique(PP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-4,4);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.out$model$DOY,partial.resids.PP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,PP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.out$model$CY,partial.resids.PP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,PP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(PP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(PP.out.pdat$CY)),nrow=length(unique(PP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(PP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(PP.out.pdat$CY),y=unique(PP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(PP.out.pdat$CY),y=unique(PP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# DP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.in$model$DOY,partial.resids.DP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.in$model$CY,partial.resids.DP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DP.in.pdat$CY)),nrow=length(unique(DP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DP.in.pdat$CY),y=unique(DP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DP.in.pdat$CY),y=unique(DP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.out$model$DOY,partial.resids.DP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.out$model$CY,partial.resids.DP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DP.out.pdat$CY)),nrow=length(unique(DP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DP.out.pdat$CY),y=unique(DP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DP.out.pdat$CY),y=unique(DP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
}

# SRP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.in$model$DOY,partial.resids.SRP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,SRP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
  
  ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.in$model$CY,partial.resids.SRP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,SRP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(SRP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP.in.pdat$CY)),nrow=length(unique(SRP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(SRP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(SRP.in.pdat$CY),y=unique(SRP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(SRP.in.pdat$CY),y=unique(SRP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.out$model$DOY,partial.resids.SRP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.out$model$CY,partial.resids.SRP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,SRP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(SRP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP.out.pdat$CY)),nrow=length(unique(SRP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(SRP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(SRP.out.pdat$CY),y=unique(SRP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(SRP.out.pdat$CY),y=unique(SRP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# DOP
{
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DOP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.in$model$DOY,partial.resids.DOP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DOP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DOP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DOP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DOP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.in$model$CY,partial.resids.DOP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DOP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DOP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DOP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DOP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DOP.in.pdat$CY)),nrow=length(unique(DOP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DOP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DOP.in.pdat$CY),y=unique(DOP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DOP.in.pdat$CY),y=unique(DOP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DOP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DOP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DOP.out.pdat$CY)),nrow=length(unique(DOP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DOP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DOP.out.pdat$CY),y=unique(DOP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DOP.out.pdat$CY),y=unique(DOP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P_v2.png"),width=10,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:22),6,4,byrow = T),heights=c(0.2,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

{
# TP
{
  plot(0:1,0:1,axes=F,ann=F,type="n")
  text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
  plot(0:1,0:1,axes=F,ann=F,type="n")
  text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
  par(mar=c(2,1.75,1,1))
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.in$model$DOY,partial.resids.TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.in$model$CY,partial.resids.TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.out$model$DOY,partial.resids.TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.out$model$CY,partial.resids.TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
}

# PP
{
  ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.in$model$DOY,partial.resids.PP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,PP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.in$model$CY,partial.resids.PP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,PP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)

  ylim.val=c(-4,4);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.out$model$DOY,partial.resids.PP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,PP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.out$model$CY,partial.resids.PP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,PP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

# DP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.in$model$DOY,partial.resids.DP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.in$model$CY,partial.resids.DP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.out$model$DOY,partial.resids.DP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.out$model$CY,partial.resids.DP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

# SRP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.in$model$DOY,partial.resids.SRP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,SRP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
  
  ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.in$model$CY,partial.resids.SRP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,SRP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.out$model$DOY,partial.resids.SRP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.out$model$CY,partial.resids.SRP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,SRP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

# DOP
{
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DOP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.in$model$DOY,partial.resids.DOP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DOP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DOP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DOP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DOP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.in$model$CY,partial.resids.DOP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DOP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DOP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DOP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DOP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_TSI.png"),width=10,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:6),2,4,byrow = T),heights=c(0.2,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

# TSI
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
    par(mar=c(2,1.75,1,1))
    
    ylim.val=c(-15,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,m.TSI.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TSI.in$model$DOY,partial.resids.TSI.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,m.TSI.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = m.TSI.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = m.TSI.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = m.TSI.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = m.TSI.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TSI\nEffect",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-20,20);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,m.TSI.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TSI.in$model$CY,partial.resids.TSI.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,m.TSI.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = m.TSI.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = m.TSI.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = m.TSI.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = m.TSI.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    ylim.val=c(-0.5,0.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TSI.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TSI.out$model$DOY,partial.resids.TSI.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TSI.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TSI.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TSI.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TSI.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TSI.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-0.5,0.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TSI.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TSI.out$model$CY,partial.resids.TSI.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TSI.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TSI.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TSI.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TSI.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TSI.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
}
  dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_N.png"),width=12,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:48,6,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
TN.in.pdat=subset(TN.in.pdat,CY>=2011)
TN.out.pdat=subset(TN.out.pdat,CY>=2011)
# TN
{
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.in$model$DOY,partial.resids.TN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"TN\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.in$model$CY,partial.resids.TN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TN.in.pdat$CY)),nrow=length(unique(TN.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TN.in.pdat$CY),y=unique(TN.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TN.in.pdat$CY),y=unique(TN.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.out$model$DOY,partial.resids.TN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.out$model$CY,partial.resids.TN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TN.out.pdat$CY)),nrow=length(unique(TN.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TN.out.pdat$CY),y=unique(TN.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TN.out.pdat$CY),y=unique(TN.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

TON.in.pdat=subset(TON.in.pdat,CY>=2013)
TON.out.pdat=subset(TON.out.pdat,CY>=2013)
# TON
{
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TON.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.in$model$DOY,partial.resids.TON.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TON.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TON.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TON.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"TON\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TON.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.in$model$CY,partial.resids.TON.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TON.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TON.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TON.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TON.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TON.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TON.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TON.in.pdat$CY)),nrow=length(unique(TON.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TON.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TON.in.pdat$CY),y=unique(TON.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TON.in.pdat$CY),y=unique(TON.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TON.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.out$model$DOY,partial.resids.TON.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TON.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TON.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TON.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TON.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.out$model$CY,partial.resids.TON.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TON.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TON.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TON.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TON.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TON.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TON.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TON.out.pdat$CY)),nrow=length(unique(TON.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TON.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TON.out.pdat$CY),y=unique(TON.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TON.out.pdat$CY),y=unique(TON.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

TKN.in.pdat=subset(TKN.in.pdat,CY>=2011)
TKN.out.pdat=subset(TKN.out.pdat,CY>=2011)
# TKN
{
  ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TKN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.in$model$DOY,partial.resids.TKN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TKN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TKN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TKN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"TKN\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TKN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.in$model$CY,partial.resids.TKN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TKN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TKN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TKN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TKN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TKN.in.pdat$CY)),nrow=length(unique(TKN.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TKN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TKN.in.pdat$CY),y=unique(TKN.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TKN.in.pdat$CY),y=unique(TKN.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TKN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.out$model$DOY,partial.resids.TKN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TKN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TKN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TKN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TKN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.out$model$CY,partial.resids.TKN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TKN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TKN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TKN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TKN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TKN.out.pdat$CY)),nrow=length(unique(TKN.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TKN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TKN.out.pdat$CY),y=unique(TKN.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TKN.out.pdat$CY),y=unique(TKN.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
}

NOx.in.pdat=subset(NOx.in.pdat,CY>=2011)
NOx.out.pdat=subset(NOx.out.pdat,CY>=2011)
# NOx
{
  ylim.val=c(-4,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.in$model$DOY,partial.resids.NOx.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NOx.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"NOx\nEffect",cex=labs.cex)
  
  ylim.val=c(-5,2.5);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.in$model$CY,partial.resids.NOx.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NOx.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NOx.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx.in.pdat$CY)),nrow=length(unique(NOx.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NOx.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NOx.in.pdat$CY),y=unique(NOx.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NOx.in.pdat$CY),y=unique(NOx.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-6,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.out$model$DOY,partial.resids.NOx.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NOx.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.out$model$CY,partial.resids.NOx.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NOx.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NOx.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx.out.pdat$CY)),nrow=length(unique(NOx.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NOx.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NOx.out.pdat$CY),y=unique(NOx.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NOx.out.pdat$CY),y=unique(NOx.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
}

NH4.in.pdat=subset(NH4.in.pdat,CY>=2013)
NH4.out.pdat=subset(NH4.out.pdat,CY>=2013)
# NH4
{
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.in$model$DOY,partial.resids.NH4.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NH4.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"NH\u2084\u207A\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.in$model$CY,partial.resids.NH4.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NH4.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NH4.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4.in.pdat$CY)),nrow=length(unique(NH4.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NH4.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NH4.in.pdat$CY),y=unique(NH4.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NH4.in.pdat$CY),y=unique(NH4.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.out$model$DOY,partial.resids.NH4.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NH4.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-5,3);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.out$model$CY,partial.resids.NH4.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NH4.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NH4.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4.out.pdat$CY)),nrow=length(unique(NH4.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NH4.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NH4.out.pdat$CY),y=unique(NH4.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NH4.out.pdat$CY),y=unique(NH4.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

DIN.in.pdat=subset(DIN.in.pdat,CY>=2013)
DIN.out.pdat=subset(DIN.out.pdat,CY>=2013)
# DIN
{
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.in$model$DOY,partial.resids.DIN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DIN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DIN\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.in$model$CY,partial.resids.DIN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DIN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DIN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN.in.pdat$CY)),nrow=length(unique(DIN.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DIN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DIN.in.pdat$CY),y=unique(DIN.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DIN.in.pdat$CY),y=unique(DIN.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.out$model$DOY,partial.resids.DIN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DIN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.out$model$CY,partial.resids.DIN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DIN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DIN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN.out.pdat$CY)),nrow=length(unique(DIN.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DIN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DIN.out.pdat$CY),y=unique(DIN.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DIN.out.pdat$CY),y=unique(DIN.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_N_v2.png"),width=12,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:26),7,4,byrow = T),heights=c(0.2,1,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
TN.in.pdat=subset(TN.in.pdat,CY>=2011)
TN.out.pdat=subset(TN.out.pdat,CY>=2011)
# TN
{
  plot(0:1,0:1,axes=F,ann=F,type="n")
  text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
  plot(0:1,0:1,axes=F,ann=F,type="n")
  text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
  par(mar=c(2,1.75,1,1))
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.in$model$DOY,partial.resids.TN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"TN\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.in$model$CY,partial.resids.TN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.out$model$DOY,partial.resids.TN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN.out$model$CY,partial.resids.TN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

TON.in.pdat=subset(TON.in.pdat,CY>=2013)
TON.out.pdat=subset(TON.out.pdat,CY>=2013)
# TON
{
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TON.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.in$model$DOY,partial.resids.TON.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TON.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TON.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TON.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"TON\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TON.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.in$model$CY,partial.resids.TON.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TON.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TON.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TON.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TON.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TON.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TON.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.out$model$DOY,partial.resids.TON.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TON.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TON.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TON.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TON.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TON.out$model$CY,partial.resids.TON.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TON.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TON.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TON.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TON.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TON.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

TKN.in.pdat=subset(TKN.in.pdat,CY>=2011)
TKN.out.pdat=subset(TKN.out.pdat,CY>=2011)
# TKN
{
  ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TKN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.in$model$DOY,partial.resids.TKN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TKN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TKN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TKN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"TKN\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TKN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.in$model$CY,partial.resids.TKN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TKN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TKN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TKN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TKN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.out$model$DOY,partial.resids.TKN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TKN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TKN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TKN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TKN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TKN.out$model$CY,partial.resids.TKN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TKN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TKN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TKN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

NOx.in.pdat=subset(NOx.in.pdat,CY>=2011)
NOx.out.pdat=subset(NOx.out.pdat,CY>=2011)
# NOx
{
  ylim.val=c(-4,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.in$model$DOY,partial.resids.NOx.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NOx.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"NOx\nEffect",cex=labs.cex)
  
  ylim.val=c(-5,2.5);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.in$model$CY,partial.resids.NOx.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NOx.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-6,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.out$model$DOY,partial.resids.NOx.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NOx.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx.out$model$CY,partial.resids.NOx.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NOx.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
}

NH4.in.pdat=subset(NH4.in.pdat,CY>=2013)
NH4.out.pdat=subset(NH4.out.pdat,CY>=2013)
# NH4
{
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.in$model$DOY,partial.resids.NH4.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NH4.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"NH\u2084\u207A\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.in$model$CY,partial.resids.NH4.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NH4.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.out$model$DOY,partial.resids.NH4.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NH4.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-5,3);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4.out$model$CY,partial.resids.NH4.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NH4.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
}

DIN.in.pdat=subset(DIN.in.pdat,CY>=2013)
DIN.out.pdat=subset(DIN.out.pdat,CY>=2013)
# DIN
{
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.in$model$DOY,partial.resids.DIN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DIN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DIN\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.in$model$CY,partial.resids.DIN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DIN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.out$model$DOY,partial.resids.DIN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DIN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN.out$model$CY,partial.resids.DIN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DIN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_ratios.png"),width=12,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:56,7,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
TN_TP.in.pdat=subset(TN_TP.in.pdat,CY>=2011)
TN_TP.out.pdat=subset(TN_TP.out.pdat,CY>=2011)
# TN_TP
{
  ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.in$model$DOY,partial.resids.TN_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TN_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"TN:TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.in$model$CY,partial.resids.TN_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TN_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TN_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TN_TP.in.pdat$CY)),nrow=length(unique(TN_TP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TN_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TN_TP.in.pdat$CY),y=unique(TN_TP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TN_TP.in.pdat$CY),y=unique(TN_TP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.out$model$DOY,partial.resids.TN_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TN_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.out$model$CY,partial.resids.TN_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TN_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TN_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TN_TP.out.pdat$CY)),nrow=length(unique(TN_TP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TN_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TN_TP.out.pdat$CY),y=unique(TN_TP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TN_TP.out.pdat$CY),y=unique(TN_TP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

DIN_SRP.in.pdat=subset(DIN_SRP.in.pdat,CY>=2013)
DIN_SRP.out.pdat=subset(DIN_SRP.out.pdat,CY>=2013)
# DIN_SRP
{
  ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN_SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.in$model$DOY,partial.resids.DIN_SRP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DIN_SRP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN_SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN_SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DIN:SRP\nEffect",cex=labs.cex)
  
  ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN_SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.in$model$CY,partial.resids.DIN_SRP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DIN_SRP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN_SRP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN_SRP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DIN_SRP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN_SRP.in.pdat$CY)),nrow=length(unique(DIN_SRP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DIN_SRP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DIN_SRP.in.pdat$CY),y=unique(DIN_SRP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DIN_SRP.in.pdat$CY),y=unique(DIN_SRP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN_SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.out$model$DOY,partial.resids.DIN_SRP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DIN_SRP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN_SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN_SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN_SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.out$model$CY,partial.resids.DIN_SRP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DIN_SRP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN_SRP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN_SRP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DIN_SRP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN_SRP.out.pdat$CY)),nrow=length(unique(DIN_SRP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DIN_SRP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DIN_SRP.out.pdat$CY),y=unique(DIN_SRP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DIN_SRP.out.pdat$CY),y=unique(DIN_SRP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# PP_TP
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.in$model$DOY,partial.resids.PP_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,PP_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"%PP of TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.in$model$CY,partial.resids.PP_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,PP_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(PP_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(PP_TP.in.pdat$CY)),nrow=length(unique(PP_TP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(PP_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(PP_TP.in.pdat$CY),y=unique(PP_TP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(PP_TP.in.pdat$CY),y=unique(PP_TP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.out$model$DOY,partial.resids.PP_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,PP_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.out$model$CY,partial.resids.PP_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,PP_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(PP_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(PP_TP.out.pdat$CY)),nrow=length(unique(PP_TP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(PP_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(PP_TP.out.pdat$CY),y=unique(PP_TP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(PP_TP.out.pdat$CY),y=unique(PP_TP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# SRP_TP
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.in$model$DOY,partial.resids.SRP_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,SRP_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"%SRP of TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.in$model$CY,partial.resids.SRP_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,SRP_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(SRP_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP_TP.in.pdat$CY)),nrow=length(unique(SRP_TP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(SRP_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(SRP_TP.in.pdat$CY),y=unique(SRP_TP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(SRP_TP.in.pdat$CY),y=unique(SRP_TP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.out$model$DOY,partial.resids.SRP_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,SRP_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.out$model$CY,partial.resids.SRP_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,SRP_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(SRP_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP_TP.out.pdat$CY)),nrow=length(unique(SRP_TP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(SRP_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(SRP_TP.out.pdat$CY),y=unique(SRP_TP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(SRP_TP.out.pdat$CY),y=unique(SRP_TP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# DP_TP
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.in$model$DOY,partial.resids.DP_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DP_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"%DP of TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.in$model$CY,partial.resids.DP_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DP_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DP_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DP_TP.in.pdat$CY)),nrow=length(unique(DP_TP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DP_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DP_TP.in.pdat$CY),y=unique(DP_TP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DP_TP.in.pdat$CY),y=unique(DP_TP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.out$model$DOY,partial.resids.DP_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DP_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.out$model$CY,partial.resids.DP_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DP_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DP_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DP_TP.out.pdat$CY)),nrow=length(unique(DP_TP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DP_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DP_TP.out.pdat$CY),y=unique(DP_TP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DP_TP.out.pdat$CY),y=unique(DP_TP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

NH4_TN.in.pdat=subset(NH4_TN.in.pdat,CY>=2013)
NH4_TN.out.pdat=subset(NH4_TN.out.pdat,CY>=2013)
# NH4_TN
{
  ylim.val=c(-5,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.in$model$DOY,partial.resids.NH4_TN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NH4_TN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"%NH\u2084 of TN\nEffect",cex=labs.cex)
  
  ylim.val=c(-5,15);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.in$model$CY,partial.resids.NH4_TN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NH4_TN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4_TN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4_TN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NH4_TN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4_TN.in.pdat$CY)),nrow=length(unique(NH4_TN.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NH4_TN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NH4_TN.in.pdat$CY),y=unique(NH4_TN.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NH4_TN.in.pdat$CY),y=unique(NH4_TN.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-10,10);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.out$model$DOY,partial.resids.NH4_TN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NH4_TN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-12,12);by.y=12;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.out$model$CY,partial.resids.NH4_TN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NH4_TN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4_TN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4_TN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NH4_TN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4_TN.out.pdat$CY)),nrow=length(unique(NH4_TN.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NH4_TN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NH4_TN.out.pdat$CY),y=unique(NH4_TN.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NH4_TN.out.pdat$CY),y=unique(NH4_TN.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

NOx_TN.in.pdat=subset(NOx_TN.in.pdat,CY>=2012)
NOx_TN.out.pdat=subset(NOx_TN.out.pdat,CY>=2012)
# NOx_TN
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.in$model$DOY,partial.resids.NOx_TN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NOx_TN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"%NOx of TN\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.in$model$CY,partial.resids.NOx_TN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NOx_TN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx_TN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx_TN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NOx_TN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx_TN.in.pdat$CY)),nrow=length(unique(NOx_TN.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NOx_TN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NOx_TN.in.pdat$CY),y=unique(NOx_TN.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NOx_TN.in.pdat$CY),y=unique(NOx_TN.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.out$model$DOY,partial.resids.NOx_TN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NOx_TN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-25,25);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.out$model$CY,partial.resids.NOx_TN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NOx_TN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx_TN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx_TN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(NOx_TN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx_TN.out.pdat$CY)),nrow=length(unique(NOx_TN.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(NOx_TN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(NOx_TN.out.pdat$CY),y=unique(NOx_TN.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(NOx_TN.out.pdat$CY),y=unique(NOx_TN.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_ratios_v2.png"),width=12,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:30),8,4,byrow = T),heights=c(0.2,1,1,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
TN_TP.in.pdat=subset(TN_TP.in.pdat,CY>=2011)
TN_TP.out.pdat=subset(TN_TP.out.pdat,CY>=2011)
# TN_TP
{
  plot(0:1,0:1,axes=F,ann=F,type="n")
  text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
  plot(0:1,0:1,axes=F,ann=F,type="n")
  text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
  par(mar=c(2,1.75,1,1))
  
  ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.in$model$DOY,partial.resids.TN_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TN_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"TN:TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.in$model$CY,partial.resids.TN_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TN_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TN_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.out$model$DOY,partial.resids.TN_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TN_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TN_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TN_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TN_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TN_TP.out$model$CY,partial.resids.TN_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TN_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TN_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TN_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

DIN_SRP.in.pdat=subset(DIN_SRP.in.pdat,CY>=2013)
DIN_SRP.out.pdat=subset(DIN_SRP.out.pdat,CY>=2013)
# DIN_SRP
{
  ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN_SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.in$model$DOY,partial.resids.DIN_SRP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DIN_SRP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN_SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN_SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"DIN:SRP\nEffect",cex=labs.cex)
  
  ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN_SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.in$model$CY,partial.resids.DIN_SRP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DIN_SRP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN_SRP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN_SRP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DIN_SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.out$model$DOY,partial.resids.DIN_SRP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DIN_SRP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DIN_SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DIN_SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DIN_SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DIN_SRP.out$model$CY,partial.resids.DIN_SRP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DIN_SRP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DIN_SRP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DIN_SRP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

# PP_TP
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.in$model$DOY,partial.resids.PP_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,PP_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"%PP of TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.in$model$CY,partial.resids.PP_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,PP_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.out$model$DOY,partial.resids.PP_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,PP_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP_TP.out$model$CY,partial.resids.PP_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,PP_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

# SRP_TP
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.in$model$DOY,partial.resids.SRP_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,SRP_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"%SRP of TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.in$model$CY,partial.resids.SRP_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,SRP_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.out$model$DOY,partial.resids.SRP_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,SRP_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP_TP.out$model$CY,partial.resids.SRP_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,SRP_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

# DP_TP
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.in$model$DOY,partial.resids.DP_TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DP_TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"%DP of TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.in$model$CY,partial.resids.DP_TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DP_TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP_TP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.out$model$DOY,partial.resids.DP_TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DP_TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP_TP.out$model$CY,partial.resids.DP_TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DP_TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP_TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

NH4_TN.in.pdat=subset(NH4_TN.in.pdat,CY>=2013)
NH4_TN.out.pdat=subset(NH4_TN.out.pdat,CY>=2013)
# NH4_TN
{
  ylim.val=c(-5,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.in$model$DOY,partial.resids.NH4_TN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NH4_TN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=2,line=1.5,"%NH\u2084\u207B of TN\nEffect",cex=labs.cex)
  
  ylim.val=c(-5,15);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.in$model$CY,partial.resids.NH4_TN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NH4_TN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4_TN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4_TN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  
  ylim.val=c(-10,10);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NH4_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.out$model$DOY,partial.resids.NH4_TN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NH4_TN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NH4_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NH4_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  
  ylim.val=c(-12,12);by.y=12;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NH4_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NH4_TN.out$model$CY,partial.resids.NH4_TN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NH4_TN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NH4_TN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NH4_TN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
}

NOx_TN.in.pdat=subset(NOx_TN.in.pdat,CY>=2012)
NOx_TN.out.pdat=subset(NOx_TN.out.pdat,CY>=2012)
# NOx_TN
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.in$model$DOY,partial.resids.NOx_TN.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,NOx_TN.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"%NOx of TN\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.in$model$CY,partial.resids.NOx_TN.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,NOx_TN.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx_TN.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx_TN.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,NOx_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.out$model$DOY,partial.resids.NOx_TN.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,NOx_TN.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = NOx_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = NOx_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-25,25);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,NOx_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.NOx_TN.out$model$CY,partial.resids.NOx_TN.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,NOx_TN.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = NOx_TN.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = NOx_TN.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
}
}
dev.off()


# png(filename=paste0(plot.path,"PLSF_GAM_weekly_otherparams.png"),width=12,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:56,7,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
Colour_PCU.in.pdat=subset(Colour_PCU.in.pdat,CY>=2017)
Colour_PCU.out.pdat=subset(Colour_PCU.out.pdat,CY>=2017)
# Colour_PCU
{
  ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Colour_PCU.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Colour_PCU.in$model$DOY,partial.resids.Colour_PCU.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,Colour_PCU.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Colour_PCU.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Colour_PCU.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Colour_PCU.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Colour_PCU.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Colour\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Colour_PCU.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Colour_PCU.in$model$CY,partial.resids.Colour_PCU.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,Colour_PCU.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Colour_PCU.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Colour_PCU.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Colour_PCU.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Colour_PCU.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Colour_PCU.in.pdat,matrix(fit.DOYCY,ncol=length(unique(Colour_PCU.in.pdat$CY)),nrow=length(unique(Colour_PCU.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Colour_PCU.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Colour_PCU.in.pdat$CY),y=unique(Colour_PCU.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Colour_PCU.in.pdat$CY),y=unique(Colour_PCU.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Colour_PCU.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Colour_PCU.out$model$DOY,partial.resids.Colour_PCU.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,Colour_PCU.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Colour_PCU.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Colour_PCU.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Colour_PCU.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Colour_PCU.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Colour_PCU.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Colour_PCU.out$model$CY,partial.resids.Colour_PCU.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,Colour_PCU.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Colour_PCU.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Colour_PCU.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Colour_PCU.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Colour_PCU.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Colour_PCU.out.pdat,matrix(fit.DOYCY,ncol=length(unique(Colour_PCU.out.pdat$CY)),nrow=length(unique(Colour_PCU.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Colour_PCU.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Colour_PCU.out.pdat$CY),y=unique(Colour_PCU.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Colour_PCU.out.pdat$CY),y=unique(Colour_PCU.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# Temp.C
{
  ylim.val=c(-10,15);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Temp.C.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Temp.C.in$model$DOY,partial.resids.Temp.C.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,Temp.C.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Temp.C.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Temp.C.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Temp.C.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Temp.C.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Water Temp\nEffect",cex=labs.cex)
  
  ylim.val=c(-5,5);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Temp.C.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Temp.C.in$model$CY,partial.resids.Temp.C.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,Temp.C.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Temp.C.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Temp.C.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Temp.C.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Temp.C.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Temp.C.in.pdat,matrix(fit.DOYCY,ncol=length(unique(Temp.C.in.pdat$CY)),nrow=length(unique(Temp.C.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Temp.C.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Temp.C.in.pdat$CY),y=unique(Temp.C.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Temp.C.in.pdat$CY),y=unique(Temp.C.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-10,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Temp.C.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Temp.C.out$model$DOY,partial.resids.Temp.C.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,Temp.C.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Temp.C.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Temp.C.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Temp.C.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Temp.C.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-5,5);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Temp.C.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Temp.C.out$model$CY,partial.resids.Temp.C.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,Temp.C.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Temp.C.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Temp.C.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Temp.C.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Temp.C.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Temp.C.out.pdat,matrix(fit.DOYCY,ncol=length(unique(Temp.C.out.pdat$CY)),nrow=length(unique(Temp.C.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Temp.C.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Temp.C.out.pdat$CY),y=unique(Temp.C.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Temp.C.out.pdat$CY),y=unique(Temp.C.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# Cond
{
  ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Cond.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Cond.in$model$DOY,partial.resids.Cond.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,Cond.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Cond.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Cond.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Cond.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Cond.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"SPC\nEffect",cex=labs.cex)
  
  ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Cond.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Cond.in$model$CY,partial.resids.Cond.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,Cond.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Cond.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Cond.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Cond.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Cond.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Cond.in.pdat,matrix(fit.DOYCY,ncol=length(unique(Cond.in.pdat$CY)),nrow=length(unique(Cond.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Cond.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Cond.in.pdat$CY),y=unique(Cond.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Cond.in.pdat$CY),y=unique(Cond.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-100,100);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Cond.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Cond.out$model$DOY,partial.resids.Cond.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,Cond.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Cond.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Cond.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Cond.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Cond.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-100,100);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Cond.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Cond.out$model$CY,partial.resids.Cond.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,Cond.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Cond.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Cond.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Cond.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Cond.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Cond.out.pdat,matrix(fit.DOYCY,ncol=length(unique(Cond.out.pdat$CY)),nrow=length(unique(Cond.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Cond.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Cond.out.pdat$CY),y=unique(Cond.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Cond.out.pdat$CY),y=unique(Cond.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# DO.per
{
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DO.per.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DO.per.in$model$DOY,partial.resids.DO.per.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DO.per.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DO.per.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DO.per.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DO.per.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DO.per.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DO sat.\nEffect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DO.per.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DO.per.in$model$CY,partial.resids.DO.per.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DO.per.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DO.per.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DO.per.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DO.per.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DO.per.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DO.per.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DO.per.in.pdat$CY)),nrow=length(unique(DO.per.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DO.per.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DO.per.in.pdat$CY),y=unique(DO.per.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DO.per.in.pdat$CY),y=unique(DO.per.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DO.per.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DO.per.out$model$DOY,partial.resids.DO.per.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DO.per.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DO.per.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DO.per.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DO.per.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DO.per.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DO.per.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DO.per.out$model$CY,partial.resids.DO.per.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DO.per.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DO.per.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DO.per.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DO.per.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DO.per.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DO.per.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DO.per.out.pdat$CY)),nrow=length(unique(DO.per.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DO.per.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DO.per.out.pdat$CY),y=unique(DO.per.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DO.per.out.pdat$CY),y=unique(DO.per.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

pH.in.pdat=subset(pH.in.pdat,CY>=2017)
pH.out.pdat=subset(pH.out.pdat,CY>=2017)
# pH
{
  ylim.val=c(-1,1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,pH.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.pH.in$model$DOY,partial.resids.pH.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,pH.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = pH.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = pH.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = pH.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = pH.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"pH\nEffect",cex=labs.cex)
  
  ylim.val=c(-1,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,pH.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.pH.in$model$CY,partial.resids.pH.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,pH.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = pH.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = pH.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = pH.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = pH.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(pH.in.pdat,matrix(fit.DOYCY,ncol=length(unique(pH.in.pdat$CY)),nrow=length(unique(pH.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(pH.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(pH.in.pdat$CY),y=unique(pH.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(pH.in.pdat$CY),y=unique(pH.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,pH.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.pH.out$model$DOY,partial.resids.pH.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,pH.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = pH.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = pH.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = pH.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = pH.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,pH.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.pH.out$model$CY,partial.resids.pH.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,pH.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = pH.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = pH.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = pH.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = pH.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(pH.out.pdat,matrix(fit.DOYCY,ncol=length(unique(pH.out.pdat$CY)),nrow=length(unique(pH.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(pH.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(pH.out.pdat$CY),y=unique(pH.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(pH.out.pdat$CY),y=unique(pH.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

Phyco.ugL.in.pdat=subset(Phyco.ugL.in.pdat,CY>=2016)
Phyco.ugL.out.pdat=subset(Phyco.ugL.out.pdat,CY>=2016)
# Phyco.ugL
{
  ylim.val=c(-1,1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Phyco.ugL.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Phyco.ugL.in$model$DOY,partial.resids.Phyco.ugL.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,Phyco.ugL.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Phyco.ugL.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Phyco.ugL.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Phyco.ugL.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Phyco.ugL.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Phycocyanin\nEffect",cex=labs.cex)
  
  ylim.val=c(-1,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Phyco.ugL.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Phyco.ugL.in$model$CY,partial.resids.Phyco.ugL.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,Phyco.ugL.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Phyco.ugL.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Phyco.ugL.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Phyco.ugL.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Phyco.ugL.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Phyco.ugL.in.pdat,matrix(fit.DOYCY,ncol=length(unique(Phyco.ugL.in.pdat$CY)),nrow=length(unique(Phyco.ugL.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Phyco.ugL.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Phyco.ugL.in.pdat$CY),y=unique(Phyco.ugL.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Phyco.ugL.in.pdat$CY),y=unique(Phyco.ugL.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,Phyco.ugL.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Phyco.ugL.out$model$DOY,partial.resids.Phyco.ugL.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,Phyco.ugL.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = Phyco.ugL.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = Phyco.ugL.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = Phyco.ugL.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = Phyco.ugL.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,Phyco.ugL.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.Phyco.ugL.out$model$CY,partial.resids.Phyco.ugL.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,Phyco.ugL.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = Phyco.ugL.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = Phyco.ugL.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = Phyco.ugL.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = Phyco.ugL.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(Phyco.ugL.out.pdat,matrix(fit.DOYCY,ncol=length(unique(Phyco.ugL.out.pdat$CY)),nrow=length(unique(Phyco.ugL.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(Phyco.ugL.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(Phyco.ugL.out.pdat$CY),y=unique(Phyco.ugL.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(Phyco.ugL.out.pdat$CY),y=unique(Phyco.ugL.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

TChl.ugL.in.pdat=subset(TChl.ugL.in.pdat,CY>=2016)
TChl.ugL.out.pdat=subset(TChl.ugL.out.pdat,CY>=2016)
# TChl.ugL
{
  ylim.val=c(-1,1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TChl.ugL.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TChl.ugL.in$model$DOY,partial.resids.TChl.ugL.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TChl.ugL.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TChl.ugL.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TChl.ugL.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TChl.ugL.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TChl.ugL.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Chlorophyll\nEffect",cex=labs.cex)
  
  ylim.val=c(-1,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TChl.ugL.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TChl.ugL.in$model$CY,partial.resids.TChl.ugL.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TChl.ugL.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TChl.ugL.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TChl.ugL.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TChl.ugL.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TChl.ugL.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Inflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TChl.ugL.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TChl.ugL.in.pdat$CY)),nrow=length(unique(TChl.ugL.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TChl.ugL.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TChl.ugL.in.pdat$CY),y=unique(TChl.ugL.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TChl.ugL.in.pdat$CY),y=unique(TChl.ugL.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TChl.ugL.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TChl.ugL.out$model$DOY,partial.resids.TChl.ugL.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TChl.ugL.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TChl.ugL.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TChl.ugL.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TChl.ugL.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TChl.ugL.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-1,1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TChl.ugL.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TChl.ugL.out$model$CY,partial.resids.TChl.ugL.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TChl.ugL.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TChl.ugL.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TChl.ugL.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TChl.ugL.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TChl.ugL.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TChl.ugL.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TChl.ugL.out.pdat$CY)),nrow=length(unique(TChl.ugL.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TChl.ugL.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TChl.ugL.out.pdat$CY),y=unique(TChl.ugL.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TChl.ugL.out.pdat$CY),y=unique(TChl.ugL.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}
}
dev.off()


# Concentration Plots -----------------------------------------------------

sites.vals=c("Godbout","Lake_Outlet")

# png(filename=paste0(plot.path,"PLSF_weekly_P.png"),width=6.5,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,1.5,0.25));
layout(matrix(1:10,5,2,byrow = T),widths=c(1,1))

yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,1000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
for(i in 1:length(sites.vals)){
  param.val="TP.ugL"
  axes.lab="TP (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TP.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)

  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  # axis_fun(1,xmaj,xmin,NA,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj/1000,2),digits=2),cex=yaxs.cex);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
  if(i==1){
    legend("topleft",legend = c("Weekly Samples","Trend","Pettitt Change Point"),
           pch=c(21,NA,NA),lty=c(NA,1,2),lwd=c(0.1,1,1),
           col=c("grey","black","red"),pt.bg=c("black",NA,NA),
           pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
  }
}

ylim.val=c(0,1000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="PP.ugL"
  axes.lab="PP (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,PP.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj/1000,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,400);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="DP.ugL"
  axes.lab="DP (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,DP.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj/1000,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,250);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="SRP.ugL"
  axes.lab="SRP (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,SRP.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj/1000,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="DOP.ugL"
  axes.lab="DOP (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,DOP.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj/1000,2),digits=2),cex=yaxs.cex);box(lwd=1)
  mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}

}
dev.off()

# png(filename=paste0(plot.path,"PLSF_weekly_N.png"),width=6.5,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,1.5,0.25));
layout(matrix(1:12,6,2,byrow = T),widths=c(1,1))

yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,10);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
for(i in 1:length(sites.vals)){
  param.val="TN.mgL"
  axes.lab="TN (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TN.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  # axis_fun(1,xmaj,xmin,NA,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
  if(i==1){
    legend("topleft",legend = c("Weekly Samples","Trend","Pettitt Change Point"),
           pch=c(21,NA,NA),lty=c(NA,1,2),lwd=c(0.1,1,1),
           col=c("grey","black","red"),pt.bg=c("black",NA,NA),
           pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
  }
}

ylim.val=c(0,10);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="TON.mgL"
  axes.lab="TON (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TON.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,10);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="TKN.mgL"
  axes.lab="TKN (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TKN.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="NOx.mgL"
  axes.lab="NOx (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,NOx.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="NH4.mgL"
  axes.lab="NH\u2084\u207A (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,NH4.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="DIN.mgL"
  axes.lab="DIN (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,DIN.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  
}
dev.off()


# png(filename=paste0(plot.path,"PLSF_weekly_otherparam.png"),width=6.5,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,1.5,0.25));
layout(matrix(1:14,7,2,byrow = T),widths=c(1,1))

yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,250);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
for(i in 1:length(sites.vals)){
  param.val="Colour_PCU"
  axes.lab="Colour (PCU)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Colour_PCU,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  # axis_fun(1,xmaj,xmin,NA,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
  if(i==1){
    legend("topleft",legend = c("Weekly Samples","Trend","Pettitt Change Point"),
           pch=c(21,NA,NA),lty=c(NA,1,2),lwd=c(0.1,1,1),
           col=c("grey","black","red"),pt.bg=c("black",NA,NA),
           pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
  }
}

ylim.val=c(0,50);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="Temp.C"
  axes.lab="Temp (\u2103)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Temp.C,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),repeated=F)
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,600);by.y=300;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="Cond"
  axes.lab="SPC (\u03BCS cm\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Cond,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="DO.per"
  axes.lab="DO (% Sat)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,DO.per,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(6,9);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="pH"
  axes.lab="pH (SU)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,pH,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.max=c(0.025,1)
by.ylim=c(0.01,0.5)
for(i in 1:length(sites.vals)){
  ylim.val=c(0,ylim.max[i]);by.y=by.ylim[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)  
  
  param.val="Phyco.ugL"
  axes.lab=expression(paste("Phyco"[italic(" in-vivo")]," (",mu,"g L"^"-1",")")) #(\u03BCg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Phyco.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

ylim.val=c(0,65);by.y=30;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.max=c(10,60)
by.ylim=c(5,30)
for(i in 1:length(sites.vals)){
  ylim.val=c(0,ylim.max[i]);by.y=by.ylim[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)  
  
  param.val="TChl.ugL"
  axes.lab=axes.lab=expression(paste("Chl"[italic(" in-vivo")]," (",mu,"g L"^"-1",")")) #"Chl (\u03BCg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TChl.ugL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  trend=mblm::mblm(value~CY.d,subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val))
  x.val=with(subset(wq.dat.melt,Site==sites.vals[i]&variable==param.val),seq(min(CY.d),max(CY.d),length.out=15))
  fit=predict(trend,newdata=data.frame(CY.d=x.val))
  lines(date.fun(lubridate::date_decimal(x.val)),fit,lwd=2)
  
  with(subset(pt.trend.all,Site==sites.vals[i]&param==param.val),abline(v=date.fun(lubridate::date_decimal(date.change)),col="red",lty=2))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_weekly_otherparam2.png"),width=6.5,height=8.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,1.5,0.25));
layout(matrix(1:12,6,2,byrow = T),widths=c(1,1))

yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,70);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
for(i in 1:length(sites.vals)){
  param.val="Turb.NTU"
  axes.lab="Turbidity (NTU)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Turb.NTU,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  # axis_fun(1,xmaj,xmin,NA,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
  if(i==1){
    legend("topleft",legend = c("Weekly Samples"),
           pch=c(21),lty=c(NA),lwd=c(0.1),
           col=c("grey"),pt.bg=c("black"),
           pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
  }
}

wq.dat$Sal.calc=with(wq.dat,SalinityCalc(Cond,Temp.C))
# range(with(wq.dat,SalinityCalc(Cond,Temp.C)),na.rm=T)
# range(wq.dat$Sal,na.rm=T)
# plot(wq.dat$Sal~with(wq.dat,SalinityCalc(Cond,Temp.C)),
#      ylab="Measured Salinity",
#      xlab="re-Calculated Salinity");abline(0,1,col="red")

ylim.val=c(0,0.3);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="Sal.calc"
  axes.lab="Salinity (PSU)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Sal.calc,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

# range(wq.dat$Resistivity.ohm,na.rm=T)
ylim.val=c(0,0.1);by.y=0.02;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="Resistivity.ohm"
  axes.lab="Resistivity (\u2126)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,Resistivity.ohm,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

# range(wq.dat$TDS.mgL,na.rm=T)
ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="TDS.mgL"
  axes.lab="TDS (mg L\u207B\u00B9)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TDS.mgL,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

# range(wq.dat$ORP.mV,na.rm=T)
ylim.val=c(-2000,6000);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="ORP.mV"
  axes.lab="Redox/ORP (V)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,ORP.mV,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj*0.001,1),digits=1),cex=yaxs.cex);box(lwd=1)
  # mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}

# range(wq.dat$TSI_mean,na.rm=T)
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="TSI_mean"
  axes.lab=expression(paste("TSI"[" mean"]," (unitless)"))
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TSI_mean,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  cols2=colorRampPalette(c("lightblue","forestgreen"))(4)
  abline(h=c(30,40,60,70),lty=2,lwd=2,col=cols2)
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=yaxs.cex);box(lwd=1)
  mtext(side=1,line=2,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_weekly_TSIMean.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,0.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,1))

yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(sites.vals)){
  param.val="TSI_mean"
  axes.lab=expression(paste("TSI"[" mean"]," (unitless)"))
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TSI_mean,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
  cols2=colorRampPalette(c("lightblue","forestgreen"))(4)
  abline(h=c(30,40,60,70),lty=2,lwd=2,col=cols2)
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=yaxs.cex);box(lwd=1)
  mtext(side=1,line=1.5,"Date",cex=labs.cex)
  if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}
dev.off()


# png(filename=paste0(plot.path,"PLSF_CY_TSIMean.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,1.5,0.5,1),oma=c(2,2.5,0.75,0.25));
layout(matrix(1:3,1,3,byrow = T),widths=c(1,1,0.25))

CY.yrs=seq(2010,2020,1)
axes.lab=expression(paste("TSI"[" mean"]," (unitless)"))
for(i in 1:length(sites.vals)){
tmp.dat=subset(wq.dat,Site==sites.vals[i]&CY%in%CY.yrs)
x=boxplot(TSI_mean~CY,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,ann=F,axes=F)
cols2=colorRampPalette(c("lightblue","forestgreen"))(4)
abline(h=c(30,40,60,70),lty=2,lwd=1,col=cols2)
boxplot(TSI_mean~CY,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,ann=F,axes=F,add=T,
        medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
box(lwd=1)
if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
mtext(side=1,line=1.75,"Year")
mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}

par(mar=c(1,0,0.5,1))
plot(0:1,0:1,axes=F,ann=F,type="n",ylim=ylim.val)
segments(-0.5,c(30,40,60,70),1,c(30,40,60,70),lty=2,lwd=1,col=cols2,xpd=NA)
text(x=0.5,y=c(30,40,60,70),
     c("Oligotrophic","Mesotrophic","Eutrophic","Hypererutrophic"),cex=0.8,pos=3,col=cols2,font=2,offset=0.1,xpd=NA)
dev.off()


# png(filename=paste0(plot.path,"PLSF_CY_TSIMean2.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,1.5,0.5,1),oma=c(2,2.5,0.75,0.25));
layout(matrix(1:3,1,3,byrow = T),widths=c(1,1,0.25))

CY.yrs=seq(2010,2020,1)
axes.lab=expression(paste("TSI"[" mean"]," (unitless)"))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat,Site==sites.vals[i]&CY%in%CY.yrs)
  x=boxplot(TSI_mean2~CY,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,ann=F,axes=F)
  cols2=colorRampPalette(c("lightblue","forestgreen"))(4)
  abline(h=c(30,40,60,70),lty=2,lwd=0.75,col="black")
  boxplot(TSI_mean2~CY,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  mtext(side=1,line=1.75,"Year")
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}

par(mar=c(1,0,0.5,1))
plot(0:1,0:1,axes=F,ann=F,type="n",ylim=ylim.val)
segments(-0.25,c(30,40,60,70),1,c(30,40,60,70),lty=2,lwd=0.75,col="black",xpd=NA)
text(x=0.5,y=c(30,40,60,70),
     c("Oligotrophic","Mesotrophic","Eutrophic","Hypererutrophic"),cex=0.8,pos=3,col="black",font=2,offset=0.1,xpd=NA)

dev.off()



wq.dat2=subset(wq.dat,CY%in%seq(2010,2020,1))
unique(subset(wq.dat2,is.na(TChl.ugL)==F)$Site)

wq.dat2$CY.f=as.factor(wq.dat2$CY)

unique(subset(wq.dat2,is.na(TSI_chl)==F)$Site)
# png(filename=paste0(plot.path,"PLSF_CY_TSIMean2.png"),width=6,height=6.5,units="in",res=200,type="windows",bg="white")
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,105);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,1.5,0.5,1),oma=c(2,2.5,0.75,0.25));
layout(matrix(1:10,5,2,byrow = T))

yaxs.cex=0.8
labs.cex=0.75
CY.yrs=seq(2010,2020,1)
xlim.vals2=c(0.5,length(CY.yrs)+0.5)
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]
axes.lab=expression(paste("TSI"[" Chl"]," (unitless)"))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat2,Site==sites.vals[i]&CY%in%CY.yrs)
  
  x=boxplot(TSI_chl~CY.f,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F)
  abline(h=c(30,40,60,70),lty=2,lwd=0.5,col="black")
  if(i==1){text(x=xlim.vals2[1]+1,y=c(30,40,60,70),
       c("Oligotrophic","Mesotrophic","Eutrophic","Hypererutrophic"),
       cex=0.75,pos=3,col="black",font=2,offset=0.1,xpd=NA)}
  boxplot(TSI_chl~CY.f,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  # axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),NA,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  # mtext(side=1,line=1.75,"Year")
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}
axes.lab=expression(paste("TSI"[" TP"]," (unitless)"))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat2,Site==sites.vals[i]&CY%in%CY.yrs)
  
  x=boxplot(TSI_TP~CY.f,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F)
  abline(h=c(30,40,60,70),lty=2,lwd=0.5,col="black")
  boxplot(TSI_TP~CY.f,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  # axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),NA,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  # mtext(side=1,line=1.75,"Year")
  # mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}
axes.lab=expression(paste("TSI"[" TN"]," (unitless)"))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat2,Site==sites.vals[i]&CY%in%CY.yrs)
  
  x=boxplot(TSI_TN~CY.f,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F)
  abline(h=c(30,40,60,70),lty=2,lwd=0.5,col="black")
  boxplot(TSI_TN~CY.f,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  # axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),NA,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  # mtext(side=1,line=1.75,"Year")
  # mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}
axes.lab=expression(paste("TSI"[" mean (TN,TP,Chl)"]))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat2,Site==sites.vals[i]&CY%in%CY.yrs)
  
  x=boxplot(TSI_mean~CY.f,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F)
  abline(h=c(30,40,60,70),lty=2,lwd=0.5,col="black")
  boxplot(TSI_mean~CY.f,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  # axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),NA,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  # mtext(side=1,line=1.75,"Year")
  # mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}
axes.lab=expression(paste("TSI"[" mean (TN,TP)"]))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat2,Site==sites.vals[i]&CY%in%CY.yrs)
  
  x=boxplot(TSI_mean2~CY.f,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F)
  abline(h=c(30,40,60,70),lty=2,lwd=0.5,col="black")
  boxplot(TSI_mean2~CY.f,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,xlim=xlim.vals2,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  mtext(side=1,line=1.75,"Year")
  # mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}
dev.off()


axes.lab=expression(paste("TSI"[" mean"]," (unitless)"))
for(i in 1:length(sites.vals)){
  tmp.dat=subset(wq.dat,Site==sites.vals[i]&CY%in%CY.yrs)
  x=boxplot(TSI_mean~CY,tmp.dat,outline=F,col=NA,border=NA,ylim=ylim.val,ann=F,axes=F)
  cols2=colorRampPalette(c("lightblue","forestgreen"))(4)
  abline(h=c(30,40,60,70),lty=2,lwd=1,col=cols2)
  boxplot(TSI_mean~CY,tmp.dat,outline=F,col=cols[i],ylim=ylim.val,ann=F,axes=F,add=T,
          medlwd=1,whisklwd=0.75,boxlwd=0.75,staplelwd=0.75)
  axis_fun(1,seq(1,length(CY.yrs),1)[seq(1,length(CY.yrs),by.x)],seq(1,length(CY.yrs),1),xmaj,line=-0.5);
  axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,axes.lab,cex=labs.cex)}
  mtext(side=1,line=1.75,"Year")
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[i],font=2)
}


dev.off()


# png(filename=paste0(plot.path,"PLSF_TSICompare2.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,1.5,0.25));

xlim.val=c(0,110);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=xlim.val;by.y=by.x;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(TSI_mean~TSI_mean2,wq.dat2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i");
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(0,1,col="red",lwd=1,lty=2)
points(TSI_mean~TSI_mean2,wq.dat2,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("black",0.75),lwd=0.1)
mod=lm(TSI_mean~TSI_mean2,subset(wq.dat2,CY>2015))
x.val=with(subset(wq.dat2,CY>2015),seq(min(TSI_mean2,na.rm=T),max(TSI_mean2,na.rm=T),length.out=15))
mod=stats::predict.lm(mod,data.frame(TSI_mean2=x.val),interval="confidence")
lines(x.val,mod[,1],lwd=2,col="red")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,format(round(ymaj,1),digits=1),cex=1);box(lwd=1)
mtext(side=2,line=2,expression(paste("TSI"[" mean (TN,TP,Chl)"])))
mtext(side=1,line=1.5,expression(paste("TSI"[" mean (TN,TP)"])))
legend("topleft",legend=c("1:1 line","2016 - 2020 data"),
       pch=c(NA),lty=c(2,1),lwd=c(1,2),
       col=c("red"),pt.bg=c(NA),
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()

# png(filename=paste0(plot.path,"PLSF_weekly_secchi.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,1.5,0.25));

yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

  param.val="Secchi_cm"
  axes.lab="Secchi Depth (cm)"
  plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
  # abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(wq.dat,Site==sites.vals[2]),pt_line(Date,Secchi_cm,2,cols[2],1,21,cols[2],cex=0.75,pt.col=cols[2]))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5,cex=yaxs.cex);
  # axis_fun(1,xmaj,xmin,NA,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex);box(lwd=1)
  mtext(side=2,line=2.5,axes.lab,cex=labs.cex)
  mtext(side=3,paste("Lake",c("Inlet","Outlet"))[2],font=2)
  mtext(side=1,line=2,"Date",cex=labs.cex)
    legend("topleft",legend = c("Weekly Samples"),
           pch=c(21),lty=c(NA),lwd=c(0.1),
           col=c("grey"),pt.bg=c("black"),
           pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()


## Stoichiometry ----------------------------------------------------------

# png(filename=paste0(plot.path,"PLSF_weekly_TNTP.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(1.5,2.5,0.5,0.25));
layout(matrix(1:2,2,1))
yaxs.cex=0.8
labs.cex=0.9
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]

ylim.val=c(0,400);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

param.val="TN_TP"
axes.lab="TN:TP (molar ratio)"
for(i in 1:length(sites.vals)){
plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
# abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(wq.dat,Site==sites.vals[i]),pt_line(Date,TN_TP,2,cols[i],1,21,cols[i],cex=0.75,pt.col=cols[i]))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.75,cex=yaxs.cex);

# if(i==1){axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex)}else{axis_fun(2,ymaj,ymin,NA)}
axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex)
box(lwd=1)
# if(i==1){mtext(side=2,line=2.5,axes.lab,cex=labs.cex)}
mtext(side=3,paste("Lake",c("Inlet ","Outlet "))[i],font=2,line=-1.25,adj=1)
if(i==1){legend("topleft",legend = c("Weekly Samples"),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       col=c("grey"),pt.bg=c("black"),
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)}
}
mtext(side=1,line=1.25,"Date",cex=labs.cex)
mtext(side=2,line=1.5,outer=T,axes.lab,cex=labs.cex)
dev.off()

sum(subset(wq.dat,Site==sites.vals[2])$TN_TP>20,na.rm=T)
wq.dat$TN_TP_gt20=wq.dat$TN_TP>20
wq.dat$TN_TP_lt20=wq.dat$TN_TP<20

ylim.val=c(0,1.5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
points(TN_TP_gt20~Date,subset(wq.dat,Site==sites.vals[2]))

sum(subset(wq.dat,Site==sites.vals[2])$DIN_TP>1.6,na.rm=T)
wq.dat$DIN_TP_gt=wq.dat$DIN_TP>1.6
wq.dat$DIN_TP_lt=wq.dat$DIN_TP<1.6

plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
points(DIN_TP_gt~Date,subset(wq.dat,Site==sites.vals[2]))


# png(filename=paste0(plot.path,"PLSF_weekly_outlet_TNTPfreq.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(1.5,2.5,0.5,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

plot(TP.ugL~Date,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
# with(subset(wq.dat,Site==sites.vals[2]),cumsum(ifelse(is.na(TN_TP_lt20),0,TN_TP_lt20)))
with(subset(wq.dat,Site==sites.vals[2]),
     lines(Date,cumsum(ifelse(is.na(TN_TP_lt20),0,TN_TP_lt20)),lwd=2,col="red"))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.75,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,format(round(ymaj,2),digits=2),cex=yaxs.cex)
box(lwd=1)
mtext(side=3,paste("Lake",c("Inlet ","Outlet "))[2],font=2,line=-1.25,adj=1)
mtext(side=1,line=1.25,"Date",cex=labs.cex)
mtext(side=2,line=1.5,"Cumulative Count\nTN:TP < 20")
dev.off()

# png(filename=paste0(plot.path,"PLSF_weekly_outlet_TNTPSeasonal.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(1.5,2.5,0.5,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,365);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)

plot(TN_TP~DOY,wq.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
with(subset(wq.dat,Site==sites.vals[2]),points(TN_TP~DOY,
                                               pch=21,bg=adjustcolor("dodgerblue1",0.5),
                                               lwd=0.1,col=adjustcolor("black",0.5)))
mod=loess(TN_TP~DOY,subset(wq.dat,Site==sites.vals[2]))
x.val=with(subset(wq.dat,Site==sites.vals[2]),seq(min(DOY),max(DOY),length.out=100))
pred=predict(mod,data.frame(DOY=x.val))
lines(x.val,pred,lwd=2,col=adjustcolor("red",0.5))
abline(h=20,col="red",lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.75,cex=yaxs.cex);
axis_fun(2,ymaj,ymin,ymaj,cex=yaxs.cex)
box(lwd=1)
mtext(side=3,paste("Lake",c("Inlet ","Outlet "))[2],font=2,line=-1.25,adj=1)
mtext(side=1,line=1.25,"DOY",cex=labs.cex)
mtext(side=2,line=1.75,"TN:TP (molar ratio)")
dev.off()



# Ice/Bloom period comparison ---------------------------------------------
## median bloom/no bloom and ice/no ice with KW test stats for inflow and outflow

## boxplot for all params

wq.dat.melt$bloom.sea=with(wq.dat.melt,ifelse(month%in%c(7:10),"Bloom","NoBloom"))
wq.dat.melt$bloom.sea=factor(wq.dat.melt$bloom.sea,levels=c("Bloom","NoBloom"))
wq.dat.melt$ice.sea=with(wq.dat.melt,ifelse(month%in%c(12,1:4),"Ice",'NoIce'))
wq.dat.melt$ice.sea=factor(wq.dat.melt$ice.sea,levels=c("NoIce","Ice"))

boxplot(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"),outline=F)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"))
boxplot(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"),outline=F)
kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))

boxplot(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"),outline=F)
kruskal.test(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Godbout"))
boxplot(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"),outline=F)
kruskal.test(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))

test=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable=="TP.ugL"&Site=="Lake_Outlet"))
test$statistic
test$p.value

paramvars
dcast(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars),variable+Site~bloom.sea,value.var = "value",median,na.rm=T)
ddply(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars),
      c("variable","Site"),summarise,
      stat=kruskal.test(value~bloom.sea)$statistic,
      pval=kruskal.test(value~bloom.sea)$p.value)

dcast(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars),variable+Site~ice.sea,value.var = "value",median,na.rm=T)
ddply(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars),
      c("variable","Site"),summarise,
      stat=kruskal.test(value~ice.sea)$statistic,
      pval=kruskal.test(value~ice.sea)$p.value)

# png(filename=paste0(plot.path,"PLSF_SeaComp_P.png"),width=6.5,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:20,5,4,byrow = T),widths=c(1,1))
xaxs.cex=0.8

ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="TP.ugL"
  axes.lab="TP (\u03BCg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0,500);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="PP.ugL"
  axes.lab="PP (\u03BCg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="DP.ugL"
  axes.lab="DP (\u03BCg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="SRP.ugL"
  axes.lab="SRP (\u03BCg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="DOP.ugL"
  axes.lab="DOP (\u03BCg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1,cex=xaxs.cex)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1,cex=xaxs.cex)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}
mtext(side=1,line=2,outer=T,"Seasons")
dev.off()

# png(filename=paste0(plot.path,"PLSF_SeaComp_N.png"),width=6.5,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:24,6,4,byrow = T),widths=c(1,1))
xaxs.cex=0.8

ylim.val=c(0,5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="TN.mgL"
  axes.lab="TN (mg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0,5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="TON.mgL"
  axes.lab="TON (mg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="TKN.mgL"
  axes.lab="TKN (mg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="NOx.mgL"
  axes.lab="NOx (mg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,0.3);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="NH4.mgL"
  axes.lab="NH\u2084\u207A (mg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="DIN.mgL"
  axes.lab="DIN (mg L\u207B\u00B9)"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1,cex=xaxs.cex)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1,cex=xaxs.cex)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}
mtext(side=1,line=2,outer=T,"Seasons")
dev.off()

# png(filename=paste0(plot.path,"PLSF_SeaComp_ratio.png"),width=6.5,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:28,7,4,byrow = T),widths=c(1,1))
xaxs.cex=0.8

ylim.val=c(0,220);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="TN_TP"
  axes.lab="TN:TP"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  mtext(side=3,paste("Lake" ,c("Inlet","Outlet"))[i])
}

ylim.val=c(0,600);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="DIN_SRP"
  axes.lab="DIN:SRP"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="PP_TP"
  axes.lab="%PP of TP"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

for(i in 1:2){
  param.val="SRP_TP"
  axes.lab="%SRP of TP"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

for(i in 1:2){
  param.val="DP_TP"
  axes.lab="%DP of TP"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,25);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="NH4_TN"
  axes.lab="%NH\u2084 of TN"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  # axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1)
  axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}

ylim.val=c(0,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:2){
  param.val="NOx_TN"
  axes.lab="%NOx of TN"
  x=boxplot(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
            outline=F,ylim=ylim.val,col=c("forestgreen","lightblue"),axes=F,ann=F)
  axis_fun(1,1:2,1:2,c('Bloom\n(Aug - Oct)',"No Bloom\n(Nov - July)"),padj=1,line=-1,cex=xaxs.cex)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~bloom.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
  if(i==1){mtext(side=2,line=2.5,axes.lab)}
}
for(i in 1:2){
  boxplot(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]),
          outline=F,ylim=ylim.val,col=c("white","sky blue"),axes=F,ann=F)
  axis_fun(1,1:2,1:2,c("No Ice\n(May - Nov)",'Ice\n(Dec - Apr)'),padj=1,line=-1,cex=xaxs.cex)
  # axis_fun(1,1:2,1:2,NA)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  stat=kruskal.test(value~ice.sea,subset(wq.dat.melt,variable==param.val&Site==sites.vals[i]))
  if(stat$p.value<0.05){text(1.5,ylim.val[2],"*",pos=1,cex=2)}
}
mtext(side=1,line=2,outer=T,"Seasons")
dev.off()

## table for just ice/no-ice
paramvars2=paramvars[paramvars!="Secchi_cm"]
med.val.ice=dcast(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars2),variable+Site~ice.sea,value.var = "value",median,na.rm=T)
KW.test.ice=ddply(subset(wq.dat.melt,Site%in%sites.vals&variable%in%paramvars2),
      c("variable","Site"),summarise,
      stat=kruskal.test(value~ice.sea)$statistic,
      pval=kruskal.test(value~ice.sea)$p.value)

med.val.ice=merge(med.val.ice,KW.test.ice,
                  c("variable","Site"),sort=F)
med.val.ice%>%
  flextable()%>%
  colformat_double(j="NoIce",digits=2)%>%
  colformat_double(j="Ice",digits=2)%>%
  colformat_double(j="stat",digits=2)%>%
  colformat_double(j="pval",digits=2)%>%
  compose(j="pval",i=~pval<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="pval",i=~pval<0.01,value=as_paragraph('< 0.01'))%>%
  compose(j="pval",i=~pval<0.001,value=as_paragraph('< 0.001'))%>%
  bold(j="pval",i=~pval<0.05)%>%
  compose(j="variable",i=~variable=="TP.ugL",value=as_paragraph('TP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="SRP.ugL",value=as_paragraph('SRP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DP.ugL",value=as_paragraph('DP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="PP.ugL",value=as_paragraph('PP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DOP.ugL",value=as_paragraph('DOP (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN.mgL",value=as_paragraph('TN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="NOx.mgL",value=as_paragraph('NO\u2093 (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="NH4.mgL",value=as_paragraph('NH\u2084 (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DIN.mgL",value=as_paragraph('DIN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TON.mgL",value=as_paragraph('TON (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TKN.mgL",value=as_paragraph('TKN (mg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TN_TP",value=as_paragraph('TN:TP\n(molar ratio)'))%>%
  compose(j="variable",i=~variable=="DIN_SRP",value=as_paragraph('DIN:SRP\n (molar ratio)'))%>%
  compose(j="variable",i=~variable=="PP_TP",value=as_paragraph('%PP of TP'))%>%
  compose(j="variable",i=~variable=="SRP_TP",value=as_paragraph('%SRP of TP'))%>%
  compose(j="variable",i=~variable=="DP_TP",value=as_paragraph('%DP of TP'))%>%
  compose(j="variable",i=~variable=="NH4_TN",value=as_paragraph('%NH\u2084 of TN'))%>%
  compose(j="variable",i=~variable=="NOx_TN",value=as_paragraph('%NOx of TN'))%>%
  compose(j="variable",i=~variable=="Cond",value=as_paragraph('SPC (\u03BCS cm\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="DO.per",value=as_paragraph('DO (% Sat)'))%>%
  compose(j="variable",i=~variable=="Colour_PCU",value=as_paragraph('Colour (PCU)'))%>%
  compose(j="variable",i=~variable=="Temp.C",value=as_paragraph('Water Temp (\u2103)'))%>%
  compose(j="variable",i=~variable=="Phyco.ugL",value=as_paragraph('Phycocyanin (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="variable",i=~variable=="TChl.ugL",value=as_paragraph('Chlorophyll (\u03BCg L\u207B\u00B9)'))%>%
  compose(j="Site",i=~Site=="Godbout",value=as_paragraph('Lake Inlet'))%>%
  compose(j="Site",i=~Site=="Lake_Outlet",value=as_paragraph('Lake Outlet'))%>%
  merge_v(j="variable")%>%
  set_header_labels("variable"="Parameter",
                    "Site"="Site",
                    "NoIce" = "No Ice",
                    "Ice" = "Ice",
                    "stat" = "\u03C7\u00B2",
                    "pval"="\u03C1-value")%>%
  add_header("NoIce"="Median Concentration",
             "Ice"="Median Concentration",
             "stat" = "Kruskal-Wallis",
             "pval"="Kruskal-Wallis")%>%
  merge_h(part="header")%>%align(align="center",part="header")%>%
  width(width=c(1.5,1,0.75,0.75,0.75,0.75))%>%
  align(align="center",part="header")%>%
  align(j=3:6,align="center",part="body")%>%
  padding(padding=1.5,part="all")%>%
  hline(i=seq(2,52,2))%>%
  vline(j=c(2,4),border=officer::fp_border(color="grey"))%>%
  fix_border_issues()%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")# %>%print("docx")



# Trend Detection ---------------------------------------------------------

tmp.dat=subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL")

sort(unique(tmp.dat$CY))
yrs=seq(2011,2020,1)
yrs2=yrs[3:length(yrs)]

trend.detect=data.frame()
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
  trend.detect=rbind(trend.detect,rslt)
  print(i)
}
trend.detect$yrs=trend.detect$CY-min(tmp.dat$CY)


tmp.dat=subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TN.mgL")
sort(unique(tmp.dat$CY))
yrs=seq(2012,2020,1)
yrs2=yrs[3:length(yrs)]

trend.detect.TN=data.frame()
for(i in 1:length(yrs2)){
  trend.test=with(subset(tmp.dat,CY%in%seq(2011,yrs2[i],1)),
                  kendallSeasonalTrendTest(mean.value,month,CY))
  rslt=data.frame(Site="Lake_Outlet",param="TN.mgL",CY=yrs2[i],
                  N.val=as.numeric(trend.test$sample.size["Total"]),
                  chisq.stat=as.numeric(trend.test$statistic[1]),
                  chisq.pval=as.numeric(trend.test$p.value[1]),
                  z.stat=as.numeric(trend.test$statistic[2]),
                  tau=as.numeric(trend.test$estimate[1]),
                  z.pval=as.numeric(trend.test$p.value[2]),
                  sen.slope=as.numeric(trend.test$estimate[2]),
                  sen.int=as.numeric(trend.test$estimate[3])
  )
  trend.detect.TN=rbind(trend.detect.TN,rslt)
  print(i)
}
trend.detect.TN$yrs=trend.detect.TN$CY-min(tmp.dat$CY)

col.rmp=viridis::plasma(4,direction = -1,alpha=0.5)
b2=c(1e-05, 0.001, 0.01, 0.05, 1)
bks=findInterval(trend.detect$z.pval,b2,rightmost.closed = F,left.open=T)

# png(filename=paste0(plot.path,"PLSF_TPOutletTrenddetect.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.5))

ylim.val=c(-8,20);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(sen.slope~yrs,trend.detect,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
# plot(tau~yrs,trend.detect,pch=21,bg=col.rmp[bks])
with(trend.detect,pt_line(yrs,sen.slope,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"Lake Outlet - Total Phosphorus")
mtext(side=2,line=2,"Thiel-Sen Slope (\u03BCg P L\u207B\u00B9 Yr\u207B\u00B9)")
mtext(side=1,line=2,"Years of Monitoring")
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


# png(filename=paste0(plot.path,"PLSF_TPOutletTrenddetect_tau.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.5))

ylim.val=c(-1,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(tau~yrs,trend.detect,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(trend.detect,pt_line(yrs,tau,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"Lake Outlet - Total Phosphorus")
mtext(side=2,line=2.5,"Mann-Kendall's \u03C4")
mtext(side=1,line=2,"Years of Monitoring")

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

# png(filename=paste0(plot.path,"PLSF_TPTNOutletTrenddetect_tau.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(3,2.5,1.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.5))

ylim.val=c(-1,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(tau~yrs,trend.detect,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(trend.detect,pt_line(yrs,tau,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
with(trend.detect.TN,pt_line(yrs,tau,1,adjustcolor("black",0.5),1.5,23,col.rmp[bks],1.5,0.1))
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"Lake Outlet")
mtext(side=2,line=2.5,"Mann-Kendall's \u03C4")
mtext(side=1,line=2,"Years of Monitoring")

plot(0:1,0:1,ann=F,axes=F,type="n")
b2=round(format(b2,scientific = F,nsmall=3),3)
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0("\u2265 ",b2[(l.b-1)])))
n.bks=length(b2) -1
top.val=0.8
bot.val=0.3
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

legend(0.5,0.25,legend=c("Total Phosphorus","Total Nitrogen"),
       lty=c(NA),lwd=c(0.1,0.1),col=c("black"),
       pch=c(21,23),pt.bg=c("grey"),
       pt.cex=1.25,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()




# library(emon)
# 
# par(mfrow=c(2,2))
# lin5 = generate.trend(nyears=10, change=5, type="linear")
# plot(lin5$i, lin5$mu)
# updown = generate.trend(nyears=15, change=5, type="updown", changeyear=8)
# plot(updown$i, updown$mu)
# 
# power.trend(xvalues=lin5$i, meanvalues=lin5$mu, distribution="Normal", sd=2,
#             method="linear regression", alpha=0.05, nsims=50)
# 
# power.example=NA
# for(i in 1:7){
# tmp=power.trend(xvalues=lin5$i, meanvalues=lin5$mu,rep=i, distribution="Normal", sd=2,
#             method="mk", alpha=0.05, nsims=50)
# power.example=rbind(power.example,tmp)
# }

# TP
tmp.dat.TP=subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TP.ugL")
yrs=seq(2011,2020,1)
yrs2=yrs[3:length(yrs)]

nsims=1000
pvalues=NA
power.trend.detect=data.frame()
set.seed(123)
for(i in 1:length(yrs2)){
  
  tmp.dat2=subset(tmp.dat.TP,CY%in%seq(yrs[1],yrs2[i],1))
  
  for (j in 1:nsims) {
      yval = rnorm(n=length(tmp.dat2$mean.value),
                   mean=tmp.dat2$mean.value,
                   sd=mean(tmp.dat2$sd.value,na.rm=T)/2)
      trend.test=with(tmp.dat2,
                      kendallSeasonalTrendTest(yval,month,CY))
      pvalues[j] = trend.test$p.value[2]
    }
    power = sum(pvalues < 0.05)/nsims
    rslt=data.frame(CY=yrs2[i],trend.power=power)
    power.trend.detect=rbind(power.trend.detect,rslt)
  }
power.trend.detect
power.trend.detect$yrs=power.trend.detect$CY-min(tmp.dat.TP$CY)

# TN
tmp.dat.TN=subset(month.mean.dat,Site=="Lake_Outlet"&variable=="TN.mgL")
ddply(tmp.dat.TN,c("CY"),summarise,N.val=N.obs(mean.value))
yrs=seq(2012,2020,1)
yrs2=yrs[3:length(yrs)]

nsims=1000
pvalues=NA
power.trend.detect.TN=data.frame()
set.seed(123)
for(i in 1:length(yrs2)){
  
  tmp.dat2=subset(tmp.dat.TN,CY%in%seq(yrs[1],yrs2[i],1))
  
  for (j in 1:nsims) {
    yval = rnorm(n=length(tmp.dat2$mean.value),
                 mean=tmp.dat2$mean.value,
                 sd=0.05)
    trend.test=with(tmp.dat2,
                    kendallSeasonalTrendTest(yval,month,CY))
    pvalues[j] = trend.test$p.value[2]
  }
  power = sum(pvalues < 0.05)/nsims
  rslt=data.frame(CY=yrs2[i],trend.power=power)
  power.trend.detect.TN=rbind(power.trend.detect.TN,rslt)
}
power.trend.detect.TN
power.trend.detect.TN$yrs=power.trend.detect.TN$CY-min(tmp.dat.TN$CY)



col.rmp=viridis::plasma(4,direction = -1,alpha=0.5)
b2=c(1e-05, 0.001, 0.01, 0.05, 1)
# png(filename=paste0(plot.path,"PLSF_TPOutletTrenddetect_v2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1.5),oma=c(3,2,2,0.25));
layout(matrix(1:6,2,3,byrow = T),widths=c(1,1,0.4),heights=c(0.60,1))
lab.ln=2.25

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(trend.power~yrs,power.trend.detect,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0.8,lty=2)
with(power.trend.detect,pt_line(yrs,trend.power,1,adjustcolor("black",0.5),1.5,21,"grey",1.5,0.1))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=lab.ln,"Power")
mtext(side=3,adj=0,line=1.25,"Lake Outlet")
mtext(side=3,adj=0,"Total Phosphorus",cex=0.8,font=3,col="grey50")

plot(trend.power~yrs,power.trend.detect.TN,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0.8,lty=2)
with(power.trend.detect.TN,pt_line(yrs,trend.power,1,adjustcolor("black",0.5),1.5,21,"grey",1.5,0.1))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=lab.ln,"Power")
mtext(side=3,adj=0,"Total Nitrogen",cex=0.8,font=3,col="grey50")

plot(0:1,0:1,ann=F,axes=F,type="n")

bks=findInterval(trend.detect$z.pval,b2,rightmost.closed = F,left.open=T)
ylim.val=c(-8,20);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(sen.slope~yrs,trend.detect,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
# plot(tau~yrs,trend.detect,pch=21,bg=col.rmp[bks])
with(trend.detect,pt_line(yrs,sen.slope,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=lab.ln,"Thiel-Sen Slope (\u03BCg P L\u207B\u00B9 Yr\u207B\u00B9)")
mtext(side=1,line=2,"Years of Monitoring")

bks=findInterval(trend.detect.TN$z.pval,b2,rightmost.closed = F,left.open=T)
ylim.val=c(-0.1,0.2);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(sen.slope~yrs,trend.detect.TN,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
# plot(tau~yrs,trend.detect,pch=21,bg=col.rmp[bks])
with(trend.detect.TN,pt_line(yrs,sen.slope,1,adjustcolor("black",0.5),1.5,21,col.rmp[bks],1.5,0.1))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=lab.ln,"Thiel-Sen Slope (mg N L\u207B\u00B9 Yr\u207B\u00B9)")
mtext(side=1,line=2,"Years of Monitoring")

par(mar=c(1,0,0.5,0))
plot(0:1,0:1,ann=F,axes=F,type="n")
b2=round(format(b2,scientific = F,nsmall=3),3)
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0("\u2265 ",b2[(l.b-1)])))
n.bks=length(b2) -1
top.val=0.8
bot.val=0.2
mid.v.val=bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0
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


# Trophic Index -----------------------------------------------------------

ddply(subset(wq.dat,is.na(Secchi_cm)==F),"Site",summarise,min.val=min(Date),max.val=max(Date))
ddply(subset(wq.dat,is.na(Chla.ugL)==F),"Site",summarise,min.val=min(Date),max.val=max(Date))
## Carlson TSI
# wq.dat$TSI_sd=with(wq.dat,10*(6-(log(Secchi_cm/100)/log(2)))) #Carlson 1977
wq.dat$TSI_sd=with(wq.dat,60-14.41*log(Secchi_cm/100))
range(wq.dat$TSI_sd,na.rm=T)

plot(Chla.ugL~TChl.ugL,wq.dat)

wq.dat$TSI_chla=with(wq.dat,9.81*log(Chla.ugL*1000)+30.6)
range(wq.dat$TSI_chla,na.rm=T)
range(subset(wq.dat,Site=="Lake_Outlet")$TSI_chla,na.rm=T)

wq.dat$TSI_chl=with(wq.dat,9.81*log(TChl.ugL)+30.6)
range(wq.dat$TSI_chl,na.rm=T)

wq.dat$TSI_TP=with(wq.dat,14.42*log(TP.ugL)+4.15)
range(wq.dat$TSI_TP,na.rm=T)

wq.dat$TSI_TN=with(wq.dat,54.45+14.43*log(TN.mgL))
range(wq.dat$TSI_TN,na.rm=T)

plot(TSI_chla~TSI_chl,subset(wq.dat,Site=="Lake_Outlet"))

plot(TSI_chl~Date,subset(wq.dat,Site=="Lake_Outlet"),type="b")
plot(TSI_TP~Date,subset(wq.dat,Site=="Lake_Outlet"),type="b")
plot(TSI_TN~Date,subset(wq.dat,Site=="Lake_Outlet"),type="b")


plot(TSI_sd~month,subset(wq.dat,Site=="Lake_Outlet"))
plot(TSI_sd~month,subset(wq.dat,Site=="Godbout"))

cols=colorRampPalette(c("lightblue","forestgreen"))(4)
pt.cols=c("goldenrod","lightgreen","dodgerblue","indianred1")
# png(filename=paste0(plot.path,"PLSF_TPOutlet_TSI.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,2,1,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

ylim.val=c(0,130);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2010-01-01","2021-08-01"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(TSI_chl~Date,wq.dat,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=c(30,40,60,70),lty=2,lwd=2,col=cols)
text(x=xlim.val[2]-lubridate::ddays(60),y=c(30,40,60,70),
     c("Oligo-","Meso-","Eu-","Hypereru-"),cex=0.5,pos=3,col=cols,font=2,offset=0.1)

with(subset(wq.dat,Site=="Lake_Outlet"),pt_line(Date,TSI_sd,2,pt.cols[1],1,21,pt.cols[1],0.75,0.01))
with(subset(wq.dat,Site=="Lake_Outlet"),pt_line(Date,TSI_chl,2,pt.cols[2],1,21,pt.cols[2],0.75,0.01))
with(subset(wq.dat,Site=="Lake_Outlet"),pt_line(Date,TSI_TP,2,pt.cols[3],1,21,pt.cols[3],0.75,0.01))
with(subset(wq.dat,Site=="Lake_Outlet"),pt_line(Date,TSI_TN,2,pt.cols[4],1,21,pt.cols[4],0.75,0.01))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"Lake Outlet")
mtext(side=2,line=2.5,"Trophic State Index")
mtext(side=1,line=1.5,"Date")

par(mar=c(1,0,0.5,0))
plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("TSI(SD)",expression(paste("TSI(CHL"[italic("in-vivo")],")")),"TSI(TP)","TSI(TN)"),
       lty=c(NA),lwd=c(0.1),col=c("black"),
       pch=c(21),pt.bg=c(pt.cols),
       pt.cex=1.25,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0,
       title.adj = 0,title="Trophic State Index\nVariables")
dev.off()



# weather -----------------------------------------------------------------
library(weathercan)
# stations_dl()
# example
# weather_dl(station_ids=42203,start = "2017-01-06", end = "2017-01-10")

# dates=date.fun(c("2010-10-01","2021-09-30"))
dates=date.fun(c("1970-01-01","2020-12-31"))

subset(stations(),station_name=="BROMPTONVILLE")
subset(stations(),station_ids==5322)
# subset(stations(),station_name=="BONSECOURS")
# subset(stations(),climate_id==7024440)
# weathercan:::get_html(station_id=5327,interval="day")

wx.dat=data.frame(weather_dl(station_ids=c(5327,5322),start = dates[1], end = dates[2],interval="day",trim=F))
wx.dat$date=date.fun(wx.dat$date)
wx.dat$CY=as.numeric(format(wx.dat$date,"%Y"))

wx.dat$Tprecip=rowSums(wx.dat[,c("total_rain","total_snow")],na.rm=T)

wx.dat.da=ddply(wx.dat,c("date","CY"),summarise,
                mean.min_temp=mean(min_temp,na.rm=T),
                mean.total_precip=mean(total_precip,na.rm=T),
                mean.total_rain=mean(total_rain,na.rm=T),
                mean.total_snow=mean(total_snow,na.rm=T))

# wx.dat.mean=ddply(subset(wx.dat,station_id==5327),c("CY"),summarise,
#                   mean.min_temp=mean(min_temp,na.rm=T),
#                   sum.total_precip=sum(Tprecip,na.rm=T)*0.1,
#                   RF.nas=sum(is.na(total_precip)))
wx.dat.mean=ddply(wx.dat.da,c("CY"),summarise,
                  mean.min_temp=mean(mean.min_temp,na.rm=T),
                  sum.total_precip=sum(mean.total_precip,na.rm=T)*0.1,
                  prep.nas=sum(is.na(mean.total_precip)),
                  sum.RF=sum(mean.total_rain,na.rm=T)*0.1,
                  sum.SF=sum(mean.total_snow,na.rm=T)*0.1)



subset(wx.dat.da,CY==2020)

# png(filename=paste0(plot.path,"PLSF_wx.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,2.5,1,0.25));
layout(matrix(1:2,2,1,byrow = T),widths=c(1,1))

ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1970,2020);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(mean.min_temp~CY,wx.dat.mean,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
abline(h=0)
with(wx.dat.mean,pt_line(CY,mean.min_temp,1,"dodgerblue1",1,21,"dodgerblue1"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.25,"Mean Daily Minimum\nAir Temp (\u2103)")

ylim.val=c(0,170);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(mean.min_temp~CY,wx.dat.mean,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(wx.dat.mean,pt_line(CY,sum.SF,1,"grey",1,21,"ivory"))
with(wx.dat.mean,pt_line(CY,sum.RF,1,"dodgerblue1",1,21,"dodgerblue1"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topright",legend=c("Snow","Rain"),
       lty=c(1,1),lwd=c(1,1),col=c("grey","dodgerblue1"),
       pch=c(NA),pt.bg=c(NA),
       pt.cex=1,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0,text.col="white")
legend("topright",legend=c("Snow","Rain"),
       lty=c(NA,NA),lwd=c(0.01,0.01),col=c("black","black"),
       pch=c(21),pt.bg=c("ivory","dodgerblue1"),
       pt.cex=1,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0)
mtext(side=2,line=2.25,"Total Precipitation\n(cm yr\u207B\u00B9)")
mtext(side=1,line=1.5,"Date (Calendar Year)")
dev.off()

unique(wx.dat$station_name)
unique(wx.dat$station_id)


ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(mean.min_temp~CY,wx.dat.mean,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(wx.dat.mean,pt_line(CY,sum.SF,1,"grey",1,21,"ivory"))
with(wx.dat.mean,pt_line(CY,sum.RF,1,"dodgerblue1",1,21,"dodgerblue1"))
# with(wx.dat.mean,pt_line(CY,sum.SF+sum.RF,1,"grey",1,21,"grey"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)


head(wx.dat.mean)
range(wx.dat.mean$CY)
with(wx.dat.mean,cor.test(CY,mean.min_temp,method="kendall"))
with(wx.dat.mean,cor.test(CY,sum.total_precip,method="kendall"))

pettitt.test(wx.dat.mean$sum.total_precip)
wx.dat.mean[42,]

# etc ---------------------------------------------------------------------

wq.dat.melt
wq.dat.melt$WY=WY(wq.dat.melt$Date,WY.type="Fed")
CY.geomean=ddply(wq.dat.melt,c("Site","CY","variable"),summarise,
                 N.val=N.obs(value),
                 GM=exp(mean(log(value),na.rm=T)))

plot(GM~CY,subset(CY.geomean,Site=="Lake_Outlet"&variable=="TP.ugL"),type="b",ylim=c(0,200))
lines(GM~CY,subset(CY.geomean,Site=="Godbout"&variable=="TP.ugL"),type="b",col='Red')


tmp.TPGM=subset(CY.geomean,Site%in%c("Godbout","Lake_Outlet")&variable=="TP.ugL"&CY>=2010)
# png(filename=paste0(plot.path,"PLSF_TPAGM.png"),width=4,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2,2.5,0.5,0.25));
cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]
ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(GM~CY,tmp.TPGM,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(subset(tmp.TPGM,Site=="Godbout"),pt_line(CY,GM,2,cols[1],1,21,cols[1],cex=1.5))
with(subset(tmp.TPGM,Site=="Lake_Outlet"),pt_line(CY,GM,2,cols[2],1,21,cols[2],cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"GM TP (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=2,"Calendar Year")
legend("topleft",legend = c("Inlet","Outlet"),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       col=c("black"),pt.bg=cols,
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()


wq.dat$WY=WY(wq.dat$Date,WY.type = "Fed")

tmp=ddply(wq.dat,c("WY","Site"),summarise,TSI_mean=mean(TSI_mean,na.rm=T))

plot(TSI_mean~WY,tmp,ylim=c(0,100),xlim=xlim.val)




# exploring hystersis ---------------------------------------------------------------
## check out https://rpubs.com/tbiggs/GEOG576_Exercise_8_QC_hysteresis
## https://cran.r-project.org/web/packages/hysteresis/vignettes/index.html
library(smwrBase)
tmp=subset(wq.dat,Site=="Lake_Outlet")
tmp$TP.hyst=hysteresis(tmp$TP.ugL)

plot(TP.hyst~TP.ugL,tmp,type="l")


tmp.mean=ddply(tmp,c("CY"),summarise,mean.val=mean(TP.ugL,na.rm=T))
tmp.mean=subset(tmp.mean,CY%in%seq(2010,2020,1))
tmp.mean$TP.hyst=hysteresis(tmp.mean$mean.val)



plot(TP.hyst~mean.val,tmp.mean,type="l")
with(tmp.mean,text(mean.val,TP.hyst,CY))



tmp2=subset(wq.dat,Site=="Godbout")

tmp.mean2=ddply(tmp2,c("CY"),summarise,mean.val=mean(TP.ugL,na.rm=T))
tmp.mean2$TP.hyst=hysteresis(tmp.mean2$mean.val)


plot(TP.hyst~mean.val,tmp.mean2,type="l")
with(tmp.mean2,text(mean.val,TP.hyst,CY))


# png(filename=paste0(plot.path,"PLSF_LakeOulet_hyster.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,2),oma=c(3,2,1.5,0.25));

xlim.val=c(100,210);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(TP.hyst~mean.val,tmp.mean,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(tmp.mean,pt_line(mean.val,TP.hyst,2,"dodgerblue1",1,21,"dodgerblue1"))
with(tmp.mean,text(mean.val,TP.hyst,CY,pos=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Hysteresis effect")
mtext(side=1,line=2,"Annual Mean TP (\u03BCg L\u207B\u00B9)")
mtext(side=3,adj=1,"Lake Outlet")
dev.off()