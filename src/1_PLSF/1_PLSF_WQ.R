## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
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
dat=read.xlsx(paste0(data.path,"PLSF Database-12 Years (v2021-07-07).xlsx"))
dat$Date=date.fun(convertToDate(dat$Date))

## Parsing main dataset into seperate files
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
           "Turb.NUT", "Colour_PCU")
colnames(wq.dat)=wq.vars

wq.dat$CY=as.numeric(format(wq.dat$Date,"%Y"))
wq.dat$DOY=as.numeric(format(wq.dat$Date,"%j"))
wq.dat$month=as.numeric(format(wq.dat$Date,"%m"))
wq.dat$time=as.numeric(wq.dat$Date)/100; # From .../PLSF/code/biophysical parameters.RMD
wq.dat$WY=WY(wq.dat$Date,WY.type='Fed')

### Need MDL values for parameters
summary(wq.dat)
unique(wq.dat$SRP.mgL)
tmp=as.numeric(wq.dat$SRP.mgL)
range(tmp[tmp!=0],na.rm=T)

# assuming an MDL of 0.002
wq.dat$SRP.ugL=with(wq.dat,as.numeric(ifelse(SRP.mgL=="LOD"|SRP.mgL==0,"0.002",SRP.mgL)))*1000

# Total Phosphorus --------------------------------------------------------
wq.dat$TP.ugL=wq.dat$TP.mgL*1000
wq.dat$DP.ugL=wq.dat$DP.mgL*1000

plot(TP.ugL~SRP.ugL,wq.dat);abline(0,1)
plot(DP.ugL~SRP.ugL,wq.dat);abline(0,1)

# Reversal Evaluation
wq.dat$TPReversal=with(wq.dat,ifelse(is.na(SRP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(SRP.ugL>(TP.ugL*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
sum(wq.dat$TPReversal,na.rm=T)
subset(wq.dat,TPReversal==1)
plot(TP.ugL~SRP.ugL,wq.dat,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(wq.dat$TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

subset(wq.dat,DP.ugL>300)
subset(wq.dat,TP.ugL>4000)


TP.tmp=dcast(wq.dat,Date~Site,value.var = "TP.ugL",mean)

plot(Godbout~Date,TP.tmp,log="y")
points(In_Lake~Date,TP.tmp,pch=21,bg="blue")
points(Lake_Outlet~Date,TP.tmp,pch=21,bg="red")

# png(filename=paste0(plot.path,"PLSF_TPConc.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1,1,0.25),oma=c(2,3,0.25,0.5));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(5,10000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=date.fun(c("2008-12-01","2020-10-01"));xmaj=seq(xlim.val[1],xlim.val[2],"2 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")
plot(Godbout~Date,TP.tmp,log="y",type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TP.tmp,pt_line(Date,Godbout,2,"grey",1,21,"grey",cex=0.75,pt.lwd = 0.01))
with(TP.tmp,pt_line(Date,Lake_Outlet,2,"indianred1",1,21,"indianred1",cex=0.75,pt.lwd = 0.01))
with(TP.tmp,pt_line(Date,In_Lake,2,"dodgerblue1",1,21,"dodgerblue1",cex=0.75,pt.lwd = 0.01))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Total Phosphorus (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=1.5,"Date (Month-Year)")
legend("topleft",legend=c("Inlet","Outlet","In-Lake"),
       pch=c(21),pt.bg=c("grey","indianred1","dodgerblue1"),
       lty=c(NA),lwd=c(0.1),col=c("black"),
       pt.cex=1,cex=0.75,ncol=1,bty="n",
       y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ann.TP=ddply(wq.dat,c("CY","Site"),summarise,
             GM.TP=exp(mean(log(TP.ugL),na.rm=T)),
             N.val=N.obs(TP.ugL),
             SEGM.TP=GM.TP*(sd(log(TP.ugL),na.rm=T)/sqrt(N.val-1)))

ylim.val=c(10,500);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(2008,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(GM.TP~CY,ann.TP,log="y",type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(ann.TP,Site=="Godbout"),pt_line(CY,GM.TP,2,"grey",1,21,"grey",cex=1))
with(subset(ann.TP,Site=="Lake_Outlet"),pt_line(CY,GM.TP,2,"indianred1",1,21,"indianred1",cex=1))
with(subset(ann.TP,Site=="In_Lake"),pt_line(CY,GM.TP,2,"dodgerblue1",1,21,"dodgerblue1",cex=1))
# with(subset(ann.TP,Site=="Godbout"),pt_line_error(CY,GM.TP,SEGM.TP,2,"grey",1,21,"grey",length=0.02,cex=1))
# with(subset(ann.TP,Site=="Lake_Outlet"),pt_line_error(CY,GM.TP,SEGM.TP,2,"indianred1",1,21,"indianred1",length=0.02,cex=1))
# with(subset(ann.TP,Site=="In_Lake"),pt_line_error(CY,GM.TP,SEGM.TP,2,"dodgerblue1",1,21,"dodgerblue1",length=0.02,cex=1))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Total Phosphorus (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=2,"Calendar Year")
mtext(side=3,adj=0,line=-1," Annual Geometric Mean",cex=0.75)
# mtext(side=3,adj=0,line=-1.25," Geometric Mean \u00B1 Geometric Standard Error",cex=0.75)
dev.off()


# GAM ---------------------------------------------------------------------
library(mgcv)
library(gratia)
library(DHARMa)

library(ggplot2)
## Adpated from time_series_outlet
TP_out=subset(wq.dat,Site=="Lake_Outlet")
TP_out=subset(TP_out,CY>=2011)


## Not sure DOY and time are right model terms since GAMS and GAMMs are additive
mod <- gamm(TP.ugL ~ s(DOY, bs = "cc") + s(time, bs = "cr"),
            data = TP_out, method = "REML",
            correlation = corAR1(form = ~ 1 | CY),
            knots = list(DOY = c(0, 366)))

mod_sqTP <- gamm(sqrt(TP.ugL) ~ s(DOY, bs = "cc") + s(time, bs = "cr"),
                 data = TP_out, method = "REML",
                 correlation = corARMA(form = ~ 1 | CY,p=2),
                 knots = list(DOY = c(0, 366))
                 )

summary(mod_sqTP$gam)
draw(mod_sqTP$gam, residuals = TRUE)
appraise(mod_sqTP$gam)

# addd by PJ
layout(matrix(1:4,2,2))
gam.check(mod_sqTP$gam)
layout(matrix(1:2,1,2))
plot(mod_sqTP$gam,residuals=T,pch=21,bg="red")

layout(matrix(1:2, ncol = 2))
acf(resid(mod_sqTP$lme), lag.max = 36, main = "ACF")
pacf(resid(mod_sqTP$lme), lag.max = 36, main = "pACF")


pred.org=predict(mod_sqTP$gam,type="terms")
partial.resids.mod_sqTP<-pred.org+residuals(mod_sqTP$gam)
hist(partial.resids.mod_sqTP[1])


#plot the seasonal and temporal trends over the data
#extract model predictions
pred <- predict(mod_sqTP$gam, newdata = TP_out, type = "terms")
#trend line for time
pTP <- attr(pred, "constant") + pred[,2]
#trend line for seasonality
pTP2<-attr(pred, "constant") + pred[,1]

#plot for seasonal component
ggplot(data = TP_out, aes(DOY,TP.ugL)) + 
  geom_line()+ 
  geom_line(aes(x=DOY, y= pTP2), colour = "red") + 
  scale_x_continuous(limits= c(0, 365), breaks = c(seq(0, 365, 30))) + 
  xlab("DOY") + ylab("Total P") + ggtitle("TP Outlet")

#plot for transformed dependent variable
ggplot(data = TP_out, aes(DOY, sqrt(TP.ugL))) + 
  geom_line()+ geom_point() + 
  geom_line(aes(x=DOY, y= pTP2), colour = "red") + 
  scale_x_continuous(limits= c(0, 365), breaks = c(seq(0, 365, 30))) + 
  xlab("DOY") + ylab("Total P") + ggtitle("TP Outlet") +
  scale_y_continuous()



# -------------------------------------------------------------------------
## Daily sample GAM
TP_out=subset(wq.dat,Site=="Lake_Outlet")



# Like GAMM prior analysis
mod1 <- gam(log(TP.ugL) ~ s(DOY, bs = "cc") + s(time, bs = "cr",k=400),
                 data = TP_out, method = "REML",
                 knots = list(DOY = c(0, 366)))
summary(mod1)
layout(matrix(1:4,2,2))
gam.check(mod1,pch=21,bg="grey")

# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
testResiduals(simulateResiduals(mod1))
testUniformity(simulateResiduals(mod1))
testDispersion(simulateResiduals(mod1))

acf(mod1$residuals)

mod1 <- gam(log(TP.ugL) ~ s(DOY, bs = "cc") + s(CY, k=11),
            data = TP_out, method = "REML",
            knots = list(DOY = c(0, 366)))
summary(mod1)
layout(matrix(1:4,2,2))
gam.check(mod1,pch=21,bg="grey")


## monthly sample
unique(wq.dat$Site)
TP_out=ddply(subset(wq.dat,Site=="Lake_Outlet"),
             c("CY","month"),summarise, 
             mean.TP=mean(TP.ugL,na.rm=T),
             med.TP=median(TP.ugL,na.rm=T))

plot(mean.TP~CY,TP_out,log="y")

plot(mean.TP~month,TP_out,log="y")
plot(med.TP~month,TP_out,log="y")

plot(TP.ugL~month,subset(wq.dat,Site=="Lake_Outlet"),log="y")

# plot(TP.ugL~CY,subset(wq.dat,Site=="Godbout"),log="y")

boxplot(TP.ugL~CY,subset(wq.dat,Site=="Godbout"),log="y")
boxplot(TP.ugL~CY,subset(wq.dat,Site=="Lake_Outlet"),log="y")

layout(matrix(1:12,1,12))
for(i in 1:12){
  plot(mean.TP~CY,subset(TP_out,month==i),ylim=c(10,500))
  mtext(side=3,month.abb[i])
}

length(unique(TP_out$CY))
length(unique(TP_out$month))
mod1.month=gam(log(mean.TP)~
                 s(month,bs="cc",k=9)+
                 s(CY,k=11)+
                 ti(month, CY, bs = c('cc','tp'), k = c(9, 11)),
               data=TP_out,nthreads=12)
summary(mod1.month)

layout(matrix(1:4,2,2))
plot(mod1.month,pch=21,residuals = T)
dev.off()

layout(matrix(1:4,2,2))
gam.check(mod1.month,pch=21,bg="grey")

dev.off
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# testResiduals(simulateResiduals(mod1.month))
testUniformity(simulateResiduals(mod1.month))
testDispersion(simulateResiduals(mod1.month))

acf(mod1.month$residuals)
pacf(mod1.month$residuals)

library(flextable)
as_flextable(mod1.month)

pred.org=predict(mod1.month,type="terms")
partial.resids.mod1<-pred.org+residuals(mod1.month)

hist(partial.resids.mod1[,1]);# month
hist(partial.resids.mod1[,2]);# year
hist(partial.resids.mod1[,3]);# month-year interaction

## significance test 

# period of change -------------------------------------------------------
## see https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
              tmpf)
source(tmpf)


pdat=expand.grid(month=seq(1,12,0.25),
                 CY=seq(2010,2020,0.25))
mod.fit=predict(mod1.month,newdata=pdat,type="terms",se.fit = T)
tmp.fit=mod.fit$fit
colnames(tmp.fit)=paste("fit",c("month","CY","monCY"),sep=".")
tmp.SE=mod.fit$se.fit
colnames(tmp.SE)=paste("SE",c("month","CY","monCY"),sep=".")

pdat=cbind(pdat,tmp.fit,tmp.SE)
head(pdat)


df.res <- df.residual(mod1.month)
crit.t <- qt(0.025, df.res, lower.tail = FALSE)

pdat <- transform(pdat,
                      upper.month = fit.month + (crit.t * SE.month),
                      lower.month = fit.month - (crit.t * SE.month),
                      upper.CY = fit.CY + (crit.t * SE.CY),
                      lower.CY = fit.CY - (crit.t * SE.CY),
                      upper.monCY = fit.monCY + (crit.t * SE.monCY),
                      lower.monCY = fit.monCY - (crit.t * SE.monCY))

m.CY.d <- derivatives(mod1.month,newdata=pdat,
                      term='s(CY)',type = "central",interval="confidence",ncores=12)

m.mod.CY.dsig <- signifD(pdat$fit.CY,
                     d=m.CY.d$derivative,
                     pdat$upper.CY,pdat$lower.CY)
pdat$dsig.CY.incr=unlist(m.mod.CY.dsig$incr)
pdat$dsig.CY.decr=unlist(m.mod.CY.dsig$decr)

unique(subset(pdat,is.na(dsig.CY.incr)==F)$CY)
unique(subset(pdat,is.na(dsig.CY.decr)==F)$CY)

m.month.d <- derivatives(mod1.month,newdata=pdat,
                      term='s(month)',type = "central",interval="confidence",ncores=12)
m.mod.month.dsig <- signifD(pdat$fit.month,
                         d=m.month.d$derivative,
                         pdat$upper.month,pdat$lower.month)
pdat$dsig.month.incr=unlist(m.mod.month.dsig$incr)
pdat$dsig.month.decr=unlist(m.mod.month.dsig$decr)

unique(subset(pdat,is.na(dsig.month.incr)==F)$month)
unique(subset(pdat,is.na(dsig.month.decr)==F)$month)

month.sig=ddply(pdat,c('month'),summarise,
                fit=mean(fit.month),
                UCI=mean(upper.month,na.rm=T),
                LCI=mean(lower.month,na.rm=T),
                dsig.incr=mean(dsig.month.incr,na.rm=T),
                dsig.decre=mean(dsig.month.decr,na.rm=T))


## edit plots
## add timeseries of interaction effect and 3d interaction plot

# png(filename=paste0(plot.path,"PLSF_FAM_monthTP_v2.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,1,1.5),oma=c(1,1.5,0.25,0.25));
layout(matrix(c(1:4),2,2,byrow = T))

ylim.val=c(-1,1.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,12);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit~month,month.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(mod1.month$model$month,partial.resids.mod1[,1],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(month.sig,shaded.range(month,LCI,UCI,"grey",lty=1))
lines(fit~month,month.sig,lwd=2)
lines(UCI ~ month, data = month.sig, lty = "dashed")
lines(LCI ~ month, data = month.sig, lty = "dashed")
lines(dsig.incr ~ month, data = month.sig, col = "red", lwd = 3,lty=1)
lines(dsig.decre ~ month, data = month.sig, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(month)")
mtext(side=1,line=2,"Month")
mtext(side=2,line=2,"Effect")

ylim.val=c(-1,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(fit.CY~CY,pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
points(mod1.month$model$CY,partial.resids.mod1[,2],pch=19,col=adjustcolor("dodgerblue1",0.5))
# with(pdat,shaded.range(CY,lower.CY,upper.CY,"grey",lty=1))
lines(fit.CY~CY,pdat,lwd=2)
lines(upper.CY ~ CY, data = pdat, lty = "dashed")
lines(lower.CY ~ CY, data = pdat, lty = "dashed")
lines(dsig.CY.incr ~ CY, data = pdat, col = "red", lwd = 3,lty=1)
lines(dsig.CY.decr ~ CY, data = pdat, col = "blue", lwd = 3,lty=1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Lake Outlet",font=3)
mtext(side=3,adj=0,"s(Year)")
mtext(side=1,line=2,"Year")

ylim.val=c(2010,2020);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=c(1,12);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma1=with(pdat,matrix(fit.monCY,ncol=length(unique(pdat$CY)),nrow=length(unique(pdat$month))))
brk=50
breaks.val=classInt::classIntervals(pdat$fit.monCY,style="equal",n=brk)
pal=hcl.colors(n=brk,alpha=0.75)
image(x=unique(pdat$month),y=unique(pdat$CY),z=tmp.ma1,
      breaks=breaks.val$brks,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=unique(pdat$month),y=unique(pdat$CY),z=tmp.ma1,add=T,drawlabels=F,lwd=1.25)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"ti(Month,Year)")
mtext(side=1,line=2.25,"Month")
mtext(side=2,line=3,"Year")

legend_image=as.raster(matrix(rev(pal),ncol=1))
par(xpd=NA,mar=c(2,1,1,0))
plot(c(0,1),c(0,1),type = 'n', axes = F,ann=F)
rasterImage(legend_image, 0, 0.25, 0.3,0.75)
text(x=0.3, y = seq(0.25,0.75,length.out=2), labels = format(round(range(breaks.val$brks),1)),cex=1,pos=4)
text(0.15,0.76,"Effect",pos=3,xpd=NA)

legend(0.8,0.8,legend=c("Fitted","95% CI","Significant Change\n(Increasing)","Significant Change\n(Decreasing)","Partial Residual"),
       lty=c(1,2,1,1,0),col=c("black","black","red","blue",adjustcolor("grey",0.25)),
       pch=c(NA,NA,NA,NA,21),pt.bg=c(NA,NA,NA,NA,adjustcolor("grey",0.25)),
       pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()




# par(mar=c(0.1,0.1,0.1,0.1))
# par(mar=c(2,0.1,1,0.1))
bbox.lims=bbox(region.mask)
b2=seq(-2,2,0.1)
pal2=colorRampPalette(c("blue","grey90","red"))(length(b2)-1)
plot(ENP,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],border=NA)
image(TP.GAM.UTM,add=T,breaks=b2,col = pal2)
contour(TP.GAM.UTM,breaks = b2, add = TRUE,col="white",lwd=0.4,labcex=0.5)
plot(shore,add=T,lwd=0.1)
plot(ENP,add=T)
plot(sites.shp2,pch=19,col=adjustcolor("black",0.5),cex=0.5,add=T)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)
mtext(side=3,adj=0,"ti(UTMX,UTMY)")

plot(0:1,0:1,ann=F,axes=F,type="n")
# b2=format(b2)
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0(">",b2[(l.b-1)])))
n.bks=length(b2)-1
top.val=0.8
bot.val=0.2
mid.v.val=bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
legend_image=as.raster(matrix(rev(pal2),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
text(x=x.max, y = c(bot.val,mid.v.val,top.val), labels = format(c(min(b2),0,max(b2))),cex=0.75,adj=0,pos=4,offset=0.5)
# bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
# rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal2),lty=0)
# text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,"s(UTMX,UTMY)\nEffect",adj=0,cex=0.8,pos=3,xpd=NA)
dev.off()
