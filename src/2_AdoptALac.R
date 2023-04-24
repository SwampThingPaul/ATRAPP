## Title:      Adopt a lake analysis
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

# Tables
library(flextable)
library(magrittr)


#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/AdaptALac/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


MDL_func=function(data,MDL,rng.val=TRUE){
   tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
   tmp=ifelse(tmp<MDL,MDL/2,tmp)
   if(rng.val==TRUE){print(range(tmp,na.rm=T))}
   return(tmp)
}

# -------------------------------------------------------------------------

dat=read.xlsx(paste0(data.path,"Adopt a Lake 2018-2021 (PJ).xlsx"))
names(dat)
vars=c("ID", "Date", "Point", "TN.µg.N/L","TP.µg.P/L", "µg.MCtot/L","Température_de_l'eau",
       "Orthophosphates_µg_P/l","Azote_ammoniacal_mg_N/l","Nitrites/nitrates_mg_N/l" )
dat=dat[,vars]
colnames(dat)=c("ID", "Date", "Point","TN","TP","MC","WTemp","SRP","NH4","NOx")
head(dat)

summary(dat)

NOx.MDL=0.0004
NH4.MDL=0.0014
TP.MDL=0.7;# ugL
SRP.MDL=TP.MDL
TN.MDL=0.004

range(dat$SRP,na.rm=T)
dat$SRP=MDL_func(dat$SRP,SRP.MDL)
range(dat$NH4,na.rm=T)
dat$NH4=MDL_func(dat$NH4,NH4.MDL)
range(dat$NOx,na.rm=T)
dat$NOx=MDL_func(dat$NOx,NH4.MDL)


N.mw=14.0067
P.mw=30.973762
C.mw=12.0107

dat$TP.mM=with(dat,(TP/1000)/P.mw)
dat$TN.mM=with(dat,TN/N.mw)
dat$TN_TP=with(dat,TN.mM/TP.mM)

bks=c(0.0001,1,10,50,3000)
dat$MC_cat=findInterval(dat$MC,bks)

dat[dat$MC==0,"MC"]=NA

subset(dat,MC_cat==4)

MCcat_mean=ddply(dat,"MC_cat",summarise,
      mean.TN=mean(TN,na.rm=T),SE.TN=SE(TN),N.TN=N.obs(TN),
      mean.TP=mean(TP,na.rm=T),SE.TP=SE(TP),N.TP=N.obs(TP),
      mean.TNTP=mean(TN_TP,na.rm=T),SE.TNTP=SE(TN_TP),
      mean.SRP=mean(SRP,na.rm=T),SE.SRP=SE(SRP),
      mean.NH4=mean(NH4,na.rm=T),SE.NH4=SE(NH4),
      mean.NOx=mean(NOx,na.rm=T),SE.NOx=SE(NOx),
      min.MC=min(MC,na.rm=T),
      max.MC=max(MC,na.rm=T))

barplot(MCcat_mean$mean.TN)
barplot(MCcat_mean$mean.TP,log="y")

boxplot(TN~MC_cat,dat,outline=F)
boxplot(TP~MC_cat,dat,outline=F)

# png(filename=paste0(plot.path,"AAL_MCcats.png"),width=5.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,2,1,byrow = T))

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("ND","<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0")

x=barplot(MCcat_mean$mean.TN,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("dodgerblue1",0.5))
with(MCcat_mean,errorbars(x,mean.TN,SE.TN,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1))
axis_fun(1,x,x,NA);box(lwd=1)
mtext(side=2,line=2,"TN (mg L\u207B\u00B9)",cex=1)
text(x,rep(0,5),MCcat_mean$N.TN,pos=3,cex=0.75)

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean$mean.TP,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("indianred1",0.5))
with(MCcat_mean,errorbars(x,mean.TP,SE.TP,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj/1000,nsmall=1))
axis_fun(1,x,x,xlabs,las=1,cex=0.8,line=-0.5);box(lwd=1)
mtext(side=2,line=2,"TP (mg L\u207B\u00B9)",cex=1)
mtext(side=1,line=2.5,"Microcystin Concentration (\u03BCg L\u207B\u00B9)\nCategory")
dev.off()

MCcat_mean=MCcat_mean[order(-MCcat_mean$MC_cat),]

# png(filename=paste0(plot.path,"AAL_MCcats_v2.png"),width=5.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,2,1,byrow = T))

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=rev(c("ND","<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0"))

x=barplot(MCcat_mean$mean.TN,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("dodgerblue1",0.5))
with(MCcat_mean,errorbars(x,mean.TN,SE.TN,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1))
axis_fun(1,x,x,NA);box(lwd=1)
mtext(side=2,line=2,"TN (mg L\u207B\u00B9)",cex=1)
text(x,rep(0,5),MCcat_mean$N.TN,pos=3,cex=0.75)

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean$mean.TP,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("indianred1",0.5))
with(MCcat_mean,errorbars(x,mean.TP,SE.TP,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj/1000,nsmall=1))
axis_fun(1,x,x,xlabs,las=1,cex=0.8,line=-0.5);box(lwd=1)
mtext(side=2,line=2,"TP (mg L\u207B\u00B9)",cex=1)
mtext(side=1,line=2.5,"Microcystin Concentration (\u03BCg L\u207B\u00B9)\nCategory")
dev.off()

MCcat_mean2=ddply(subset(dat,TP<1900),"MC_cat",summarise,
                 mean.TN=mean(TN,na.rm=T),SE.TN=SE(TN),N.TN=N.obs(TN),
                 mean.TP=mean(TP,na.rm=T),SE.TP=SE(TP),N.TP=N.obs(TP),
                 mean.TNTP=mean(TN_TP,na.rm=T),SE.TNTP=SE(TN_TP),
                 mean.SRP=mean(SRP,na.rm=T),SE.SRP=SE(SRP),N.SRP=N.obs(SRP),
                 mean.NH4=mean(NH4,na.rm=T),SE.NH4=SE(NH4),N.NH4=N.obs(NH4),
                 mean.NOx=mean(NOx,na.rm=T),SE.NOx=SE(NOx),N.NOx=N.obs(NOx),
                 min.MC=min(MC,na.rm=T),
                 max.MC=max(MC,na.rm=T))
# png(filename=paste0(plot.path,"AAL_MCcats_v3.png"),width=5.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,2,1,byrow = T))

xlabs=c("ND","<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0")
ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean2$mean.TN,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("dodgerblue1",0.5))
with(MCcat_mean2,errorbars(x,mean.TN,SE.TN,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1))
axis_fun(1,x,x,NA);box(lwd=1)
mtext(side=2,line=2.25,"TN (mg L\u207B\u00B9)",cex=1)
text(x,rep(0,5),MCcat_mean2$N.TN,pos=3,cex=0.75)

ylim.val=c(0,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean2$mean.TP,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("indianred1",0.5))
with(MCcat_mean2,errorbars(x,mean.TP,SE.TP,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0))
axis_fun(1,x,x,xlabs,las=1,cex=0.8,line=-0.5);box(lwd=1)
mtext(side=2,line=2.25,"TP (\u03BCg L\u207B\u00B9)",cex=1)
mtext(side=1,line=2.5,"Microcystin Concentration (\u03BCg L\u207B\u00B9)\nCategory")
text(x,rep(0,5),MCcat_mean2$N.TP,pos=3,cex=0.75)
dev.off()

# png(filename=paste0(plot.path,"AAL_MCcats_stoich.png"),width=5.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));

xlabs=c("ND","<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0")
ylim.val=c(0,120);by.y=30;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean2$mean.TNTP,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("olivedrab2",0.5))
with(MCcat_mean2,errorbars(x,mean.TNTP,SE.TNTP,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0))
axis_fun(1,x,x,xlabs,las=1,cex=0.8,line=-0.5);box(lwd=1)
mtext(side=2,line=2.25,"TN:TP (molar ratio)",cex=1)
mtext(side=1,line=2.5,"Microcystin Concentration (\u03BCg L\u207B\u00B9)\nCategory")
text(x,rep(0,5),MCcat_mean2$N.TP,pos=3,cex=0.75)
abline(h=20,lty=2,col="red")
dev.off()

# png(filename=paste0(plot.path,"AAL_MCcats_SRP_DIN.png"),width=5.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:3,3,1,byrow = T))

xlabs=c("ND","<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0")
ylim.val=c(0,0.4);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean2$mean.NH4,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("dodgerblue1",0.5))
with(MCcat_mean2,errorbars(x,mean.NH4,SE.NH4,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1))
axis_fun(1,x,x,NA);box(lwd=1)
mtext(side=2,line=2.5,"NH\u2084 (mg L\u207B\u00B9)",cex=1)
text(x,rep(0,5),MCcat_mean2$N.NH4,pos=3,cex=0.75)

ylim.val=c(0,0.2);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean2$mean.NOx,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("darkolivegreen1",0.5))
with(MCcat_mean2,errorbars(x,mean.NOx,SE.NOx,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1))
axis_fun(1,x,x,NA);box(lwd=1)
mtext(side=2,line=2.5,"NO\u2093 (mg L\u207B\u00B9)",cex=1)
text(x,rep(0,5),MCcat_mean2$N.NOx,pos=3,cex=0.75)

ylim.val=c(0,30);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(MCcat_mean2$mean.SRP,ylim=ylim.val,ann=F,axes=F,space=0,col=adjustcolor("indianred1",0.5))
with(MCcat_mean2,errorbars(x,mean.SRP,SE.SRP,"black",length=0.05))
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0))
axis_fun(1,x,x,xlabs,las=1,cex=0.8,line=-0.5);box(lwd=1)
mtext(side=2,line=2.5,"SRP (\u03BCg L\u207B\u00B9)",cex=1)
mtext(side=1,line=3,"Microcystin Concentration (\u03BCg L\u207B\u00B9)\nCategory")
text(x,rep(0,5),MCcat_mean2$N.SRP,pos=3,cex=0.75)
dev.off()



pt.cex.grd=c(0.5,1.25,2,2.5,3)
col.rmp=viridis::magma(5,alpha=0.4)
dat=subset(dat,!(Point%in%c("Lac Fortune_1","Lac Fortune_2")))
# png(filename=paste0(plot.path,"AAL_MCcats_TNTP.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

ylim.val=c(0.01,3);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(0.00005,0.1);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TN.mM~TP.mM,dat,log="xy",type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# abline(0,1)
points(TN.mM~TP.mM,dat,pch=21,cex=pt.cex.grd[dat$MC_cat+1],bg=col.rmp[dat$MC_cat+1],lwd=0.1)
lines(x=c(0.01,10)/P.mw,y=(c(0.01,10)/P.mw)*20,col="red",lty=2,lwd=2)
# text(0.0025,0.055,"20:1",cex=0.7,adj=1,font=3,)
axis_fun(1,xmaj,xmin,formatC(xmaj,format="fg"),line=-0.5);
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"TP (mM)")
mtext(side=2,line=2.5,"TN (mM)")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.vals=c("ND",paste(c("<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0"),"\u03BCg L\u207B\u00B9 MC"))
legend("center",legend=c(leg.vals,"20:1 Redfield Ratio"),
       pch=c(rep(21,5),NA),lty=c(rep(NA,5),2),lwd=c(rep(0.1,5),2),
       col=c(rep("black",5),"red"),pt.bg=c(col.rmp,NA),
       pt.cex=c(pt.cex.grd,NA),ncol=1,cex=0.75,bty="n",
       y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title = "Microcystin Concentration\nCategory",
       title.adj = 0)
dev.off()

0.045*N.mw
(0.0008*P.mw)*100

dat$TP.mgL=dat$TP/1000
# png(filename=paste0(plot.path,"AAL_MCcats_TNTP_conc.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

ylim.val=c(0.1,25);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(0.001,2);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TN~TP.mgL,dat,log="xy",type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# abline(0,1)
points(TN~TP.mgL,dat,pch=21,cex=pt.cex.grd[dat$MC_cat+1],bg=col.rmp[dat$MC_cat+1],lwd=0.1)
x.vals=c(0.001,10)/P.mw
lines(x=x.vals*P.mw,y=(x.vals*20)*P.mw,col="red",lty=2,lwd=2)
# text(0.0025,0.055,"20:1",cex=0.7,adj=1,font=3,)
axis_fun(1,xmaj,xmin,format(xmaj,scientific=F,nsmall=1),line=-0.5);
axis_fun(2,ymaj,ymin,format(ymaj,scientific=F,nsmall=1));box(lwd=1)
mtext(side=1,line=2,"TP (mg L\u207B\u00B9)")
mtext(side=2,line=2.5,"TN (mg L\u207B\u00B9)")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.vals=c("ND",paste(c("<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0"),"\u03BCg L\u207B\u00B9 MC"))
legend("center",legend=c(leg.vals,"20:1 Redfield Ratio"),
       pch=c(rep(21,5),NA),lty=c(rep(NA,5),2),lwd=c(rep(0.1,5),2),
       col=c(rep("black",5),"red"),pt.bg=c(col.rmp,NA),
       pt.cex=c(pt.cex.grd,NA),ncol=1,cex=0.75,bty="n",
       y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title = "Microcystin Concentration\nCategory",
       title.adj = 0)
dev.off()


# png(filename=paste0(plot.path,"AAL_MCcats_TNTP_concv2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

ylim.val=c(0.1,25);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(0.001,2);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TN~TP.mgL,dat,log="xy",type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# abline(0,1)
points(TN~TP.mgL,dat,pch=21,cex=pt.cex.grd[dat$MC_cat+1],bg=col.rmp[dat$MC_cat+1],lwd=0.1)
x.vals=c(0.001,10)/P.mw
lines(x=x.vals*P.mw,y=(x.vals*20)*P.mw,col="red",lty=2,lwd=2)
# text(0.0025,0.055,"20:1",cex=0.7,adj=1,font=3,)
axis_fun(1,xmaj,xmin,format(xmaj,scientific=F,nsmall=1),line=-0.5);
axis_fun(2,ymaj,ymin,format(ymaj,scientific=F,nsmall=1));box(lwd=1)
mtext(side=1,line=2,"TP (mg L\u207B\u00B9)")
mtext(side=2,line=2.5,"TN (mg L\u207B\u00B9)")
# points(TN~TP.mgL,subset(dat,MC>1),pch=21,cex=3,bg="black",lwd=0.1)
abline(h=0.67,v=0.024,lty=2,col="dodgerblue1")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.vals=c("ND",paste(c("<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0"),"\u03BCg L\u207B\u00B9 MC"))
legend("center",legend=c(leg.vals,"20:1 Redfield Ratio"),
       pch=c(rep(21,5),NA),lty=c(rep(NA,5),2),lwd=c(rep(0.1,5),2),
       col=c(rep("black",5),"red"),pt.bg=c(col.rmp,NA),
       pt.cex=c(pt.cex.grd,NA),ncol=1,cex=0.75,bty="n",
       y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title = "Microcystin Concentration\nCategory",
       title.adj = 0)
dev.off()

min(subset(dat,MC>1)$TP)
min(subset(dat,MC>1)$TP.mgL)
min(subset(dat,MC>1)$TN)

dat$SRP.mgL=dat$SRP/1000
# png(filename=paste0(plot.path,"AAL_MCcats_SRPNH4_conc.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

ylim.val=c(0.0009,10);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");ymaj.lab=c("0.0001","0.001","0.01","0.1","1.0","10.0")
xlim.val=c(0.0001,0.200);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor");xmaj.lab=c("0.0001","0.001","0.01","0.1","1.0")
plot(TN~TP.mgL,dat,log="xy",type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# abline(0,1)
points(NH4~SRP.mgL,dat,pch=21,cex=pt.cex.grd[dat$MC_cat+1],bg=col.rmp[dat$MC_cat+1],lwd=0.1)
x.vals=c(0.00001,10)/P.mw
lines(x=x.vals*P.mw,y=(x.vals*16)*P.mw,col="red",lty=2,lwd=2)
lines(x=x.vals*P.mw,y=(x.vals*20)*P.mw,col="red",lty=2,lwd=2)
axis_fun(1,xmaj,xmin,as.character(xmaj.lab),line=-0.5);
axis_fun(2,ymaj,ymin,as.character(ymaj.lab));box(lwd=1)
mtext(side=1,line=2,"SRP (mg L\u207B\u00B9)")
mtext(side=2,line=3.5,"NH\u2084 (mg L\u207B\u00B9)")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.vals=c("ND",paste(c("<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0"),"\u03BCg L\u207B\u00B9 MC"))
legend("center",legend=c(leg.vals,"16:1 Redfield Ratio"),
       pch=c(rep(21,5),NA),lty=c(rep(NA,5),2),lwd=c(rep(0.1,5),2),
       col=c(rep("black",5),"red"),pt.bg=c(col.rmp,NA),
       pt.cex=c(pt.cex.grd,NA),ncol=1,cex=0.75,bty="n",
       y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title = "Microcystin Concentration\nCategory",
       title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"AAL_MCcats_SRPNOx_conc.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,1.5),oma=c(3,2,0.5,0.25));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

ylim.val=c(0.0009,10);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");ymaj.lab=c("0.0001","0.001","0.01","0.1","1.0","10.0")
xlim.val=c(0.0001,0.200);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor");xmaj.lab=c("0.0001","0.001","0.01","0.1","1.0")
plot(TN~TP.mgL,dat,log="xy",type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# abline(0,1)
points(NOx~SRP.mgL,dat,pch=21,cex=pt.cex.grd[dat$MC_cat+1],bg=col.rmp[dat$MC_cat+1],lwd=0.1)
x.vals=c(0.00001,10)/P.mw
lines(x=x.vals*P.mw,y=(x.vals*16)*P.mw,col="red",lty=2,lwd=2)
axis_fun(1,xmaj,xmin,xmaj.lab,line=-0.5);
axis_fun(2,ymaj,ymin,ymaj.lab);box(lwd=1)
mtext(side=1,line=2,"SRP (mg L\u207B\u00B9)")
mtext(side=2,line=3.5,"NO\u2093 (mg L\u207B\u00B9)")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.vals=c("ND",paste(c("<1.0","1.0 - 10.0","10.0 - 50.0","> 50.0"),"\u03BCg L\u207B\u00B9 MC"))
legend("center",legend=c(leg.vals,"16:1 Redfield Ratio"),
       pch=c(rep(21,5),NA),lty=c(rep(NA,5),2),lwd=c(rep(0.1,5),2),
       col=c(rep("black",5),"red"),pt.bg=c(col.rmp,NA),
       pt.cex=c(pt.cex.grd,NA),ncol=1,cex=0.75,bty="n",
       y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title = "Microcystin Concentration\nCategory",
       title.adj = 0)
dev.off()
