## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Metals data
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

## Other Functions
MDL_func=function(data,MDL,rng.val=TRUE){
  tmp=as.numeric(ifelse(data=="LOD"|data==0,MDL/2,data))
  tmp=ifelse(tmp<MDL,MDL/2,tmp)
  if(rng.val==TRUE){print(range(tmp,na.rm=T))}
  return(tmp)
}

# MDLs --------------------------------------------------------------------
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
Hardness.MDL=1.0

mdls=data.frame(variable=c("Al","Sb","As","Ba","Cd","Ca","Cr","Co","Cu","Fe","Mg","Mn","Mo","Ni","Pb","K","Se","Ag",'Na',"Hardness","Zn"),
                MDL=c(Al.MDL,Sb.MDL,As.MDL,Ba.MDL,Cd.MDL,Ca.MDL,Cr.MDL,Co.MDL,Cu.MDL,Fe.MDL,Mg.MDL,Mn.MDL,Mo.MDL,Ni.MDL,Pb.MDL,K.MDL,Se.MDL,Ag.MDL,Na.MDL,Hardness.MDL,Zn.MDL))
# 100.1/40.1 Ca Hardness
# 100.1/24.3 Mg Hardness
# Hardness = 2.497 (Ca) + 4.119 (Mg)
# <60mg/L is soft water; 60 - 120 mod hard; 120 - 180 Hard; >180 vhard
# -------------------------------------------------------------------------

dat=read.xlsx(paste0(data.path,"Metals PLSF Paul.xlsx"),sheet=1,startRow=3)
colnames(dat)=c("ID","Date","SITE",
                "Al","Sb","As","Ba","Cd","Ca","Cr","Co","Cu","Fe","Mg","Mn","Mo","Ni","Pb","K","Se","Ag",'Na',"Hardness","Zn")
dat$Date=date.fun(dat$Date)

dat.melt=melt(dat,id.vars=c("ID","Date","SITE"))
dat.melt$HalfMDL=with(dat.melt,as.numeric(ifelse(substr(value,1,1)=="<",substr(value,2,50),value)))
dat.melt=merge(dat.melt,mdls,"variable")
dat.melt$HalfMDL=with(dat.melt,ifelse(HalfMDL<=MDL,MDL/2,HalfMDL))

dat.melt=subset(dat.melt,is.na(HalfMDL)==F)
range(subset(dat.melt,variable=="Ba")$value)

ddply(subset(dat.melt,SITE=="PLSF-OUTLET"),c('SITE',"variable"),summarise,
      N.val=N.obs(HalfMDL),
      nondetect=sum(HalfMDL<=MDL,na.rm=T))
ddply(subset(dat.melt,SITE=="PLSF-IL"),c('SITE',"variable"),summarise,
      N.val=N.obs(HalfMDL),
      nondetect=sum(HalfMDL<=MDL,na.rm=T))

plot(HalfMDL~Date,subset(dat.melt,SITE=="PLSF-OUTLET"&variable=="Fe"))
plot(HalfMDL~Date,subset(dat.melt,SITE=="PLSF-OUTLET"&variable=="K"))

plot(HalfMDL~Date,subset(dat.melt,SITE=="PLSF-IL"&variable=="Fe"))

# Cu, Ni, Pb, Zn 18 - 22 sample most if not all ND
metals.outlet=c("Al","Ba","Ca","Fe","Mg","Mn","K","Na")

dat.melt2=subset(dat.melt,SITE=="PLSF-OUTLET"&variable%in%metals.outlet)
fill.date=seq(date.fun("2018-11-01"),date.fun("2020-04-01"),"1 month")
fill=data.frame(expand.grid(Date2=fill.date,variable=metals.outlet))
fill$MonYr=format(fill$Date2,"%Y-%m")

dat.melt2$MonYr=format(dat.melt2$Date,"%Y-%m")
dat.melt2=merge(dat.melt2,fill,c("MonYr","variable"),all.y=T)
dat.melt2$Date2=with(dat.melt2,date.fun(ifelse(is.na(Date)==T,as.character(Date2),as.character(Date))))
dat.melt2=dat.melt2[order(dat.melt2$variable,dat.melt2$Date2),]

# png(filename=paste0(plot.path,"PLSF_OutletMetals.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,2),oma=c(3,2,1.5,0.25));
layout(matrix(1:8,4,2,byrow = F))

col.val="#A69D7566"
max.lim.var=data.frame(variable=c("Al","Ba","Ca","Fe","Mg","Mn","K","Na"),
           max.lim=c(0.5,0.02,22,1,10,0.2,2.5,10))
xlim.val=date.fun(c("2018-11-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")
for(i in 1:length(metals.outlet)){
  ylim.val=c(0,subset(max.lim.var,variable==metals.outlet[i])$max.lim)
  by.y=subset(max.lim.var,variable==metals.outlet[i])$max.lim/2
  ymaj=ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp.dat=subset(dat.melt2,variable==metals.outlet[i])
  
  plot(HalfMDL~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(tmp.dat,pt_line(Date,HalfMDL,2,"grey",1,21,col.val,1.25,0.01))
  abline(h=subset(mdls,variable==metals.outlet[i])$MDL,lty=2,col="red")
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(i%in%c(4,8)){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=2,line=3.5,paste(metals.outlet[i]," (mg L\u207B\u00B9)"))
  if(i==5){mtext(side=3,adj=1,"Lake Outlet",font=3)}
}
mtext(side=1,line=1.25,"Date (Month-Year)",outer=T)
dev.off()


sum.stats.outlet=ddply(subset(dat.melt,SITE=="PLSF-OUTLET"),c('SITE',"variable"),summarise,
      N.val=N.obs(HalfMDL),nondetect=sum(HalfMDL<=MDL,na.rm=T),
      med.value=median(HalfMDL,na.rm=T),
      mean.value=mean(HalfMDL,na.rm=T),
      SD.value=sd(HalfMDL,na.rm=T),
      min.val=min(HalfMDL,na.rm=T),max.val=max(HalfMDL,na.rm=T)
      )

library(PeriodicTable)
data(periodicTable)
metal.fullName=data.frame(variable=c("Al","Sb","As","Ba","Cd","Ca","Cr","Co","Cu","Fe","Mg","Mn","Mo","Ni","Pb","K","Se","Ag","Na","Zn"))
metal.fullName=merge(metal.fullName,periodicTable[,c("symb","name","numb")],by.x="variable",by.y="symb",all.x=T)
sum.stats.outlet=merge(sum.stats.outlet,metal.fullName,"variable")
sum.stats.outlet=sum.stats.outlet[order(sum.stats.outlet$numb),]

vars=c("name", "N.val", "nondetect", "med.value", "mean.value", 
       "SD.value", "min.val", "max.val")
vars1=c("Calcium")
vars2=c("Sodium","Magnesium","Potassium","Lead")
vars3=c("Molybdenum","Cadmium")
sum.stats.outlet[,vars]%>%
  flextable()%>%
  colformat_double(j=4:8,digits=3,na_str = "---")%>%
  colformat_double(j=4:8,i=~name%in%vars1,digits=1,na_str = "---")%>%
  colformat_double(j=4:8,i=~name%in%vars2,digits=2,na_str = "---")%>%
  colformat_double(j=4:8,i=~name%in%vars3,digits=4,na_str = "---")%>%
  set_header_labels("name"="Parameter",
                    "N.val"="N",
                    "nondetect"="N \u2264 MDL",
                    "med.value" = "Median",
                    "mean.value" = "Mean",
                    "SD.value" = "Std Dev",
                    "min.val"="Min",
                    "max.val"="Max")%>%
  width(width=c(1,0.5,0.75,0.75,0.5,0.75,0.5,0.5))%>%
  align(j=1,align="left",part="all")%>%
  align(j=2:8,align="center",part="all")%>%
  padding(padding=1.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")# %>%print("docx")
  
  
                          

### 
dat.melt3=subset(dat.melt,SITE=="PLSF-IL"&variable%in%metals.outlet)
fill.date=seq(date.fun("2017-05-01"),date.fun("2020-09-01"),"1 month")
fill=data.frame(expand.grid(Date2=fill.date,variable=metals.outlet))
fill$MonYr=format(fill$Date2,"%Y-%m")

dat.melt3$MonYr=format(dat.melt3$Date,"%Y-%m")
dat.melt3=merge(dat.melt3,fill,c("MonYr","variable"),all.y=T)
dat.melt3$Date2=with(dat.melt3,date.fun(ifelse(is.na(Date)==T,as.character(Date2),as.character(Date))))
dat.melt3=dat.melt3[order(dat.melt3$variable,dat.melt3$Date2),]

subset(dat.melt3,variable=="K"&HalfMDL>5)
# dat.melt3$HalfMDL=with(dat.melt3,ifelse(variable=="K"&HalfMDL>7.5,NA,HalfMDL))
ddply(dat.melt3,"variable",summarise,max.val=max(HalfMDL,na.rm=T))

# png(filename=paste0(plot.path,"PLSF_InLakeMetals_v2.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,2),oma=c(3,2,1.5,0.25));
layout(matrix(1:8,4,2,byrow = F))

col.val=wesanderson::wes_palette("Zissou1",1)
max.lim.var=data.frame(variable=c("Al","Ba","Ca","Fe","Mg","Mn","K","Na"),
                       max.lim=c(0.35,0.02,30,1.5,15,0.4,3,10))
xlim.val=date.fun(c("2017-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")
for(i in 1:length(metals.outlet)){
  ylim.val=c(0,subset(max.lim.var,variable==metals.outlet[i])$max.lim)
  by.y=subset(max.lim.var,variable==metals.outlet[i])$max.lim/2
  ymaj=ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp.dat=subset(dat.melt3,variable==metals.outlet[i])
  
  plot(HalfMDL~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
  with(tmp.dat,pt_line(Date,HalfMDL,2,"grey",1,21,col.val,1.25,0.01))
  abline(h=subset(mdls,variable==metals.outlet[i])$MDL,lty=2,col="red")
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  if(i%in%c(4,8)){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=2,line=3.5,paste(metals.outlet[i]," (mg L\u207B\u00B9)"))
  if(i==5){mtext(side=3,adj=1,"In-Lake",font=3)}
  if(i==1){
    legend("topleft",legend=c("Monthly Samples","MDL"),
           pch=c(21),pt.bg=c(col.val,NA),pt.cex=c(1,NA),
           lty=c(NA,2),lwd=c(0.01,1),col=c("black","red"),
           ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=1.25,"Date (Month-Year)",outer=T)
dev.off()


tmp.dat=subset(dat.melt,SITE=="PLSF-IL"&variable%in%c("Hardness","Ca","Mg"))
tmp.dat=dcast(tmp.dat,Date~variable,value.var = "HalfMDL")
tmp.dat$hard.recalc=with(tmp.dat,(2.497*Ca) + (4.119*Mg))

with(tmp.dat,data.frame(Site="In Lake",name="Hardness",
                        N.val=N.obs(hard.recalc),nondetect=sum(hard.recalc<=Hardness.MDL,na.rm=T),
                        med.value=median(hard.recalc,na.rm=T),
                        mean.value=mean(hard.recalc,na.rm=T),
                        SD.value=sd(hard.recalc,na.rm=T),
                        min.val=min(hard.recalc,na.rm=T),
                        max.val=max(hard.recalc,na.rm=T)))%>%
  flextable()%>%
  colformat_double(j=5:9,digits=1,na_str = "---")%>%
  set_header_labels("name"="Parameter",
                    "N.val"="N",
                    "nondetect"="N \u2264 MDL",
                    "med.value" = "Median",
                    "mean.value" = "Mean",
                    "SD.value" = "Std Dev",
                    "min.val"="Min",
                    "max.val"="Max")%>%
  width(width=c(0.75,1,0.5,0.75,0.75,0.5,0.75,0.5,0.5))%>%
  align(j=1,align="left",part="all")%>%
  align(j=3:9,align="center",part="all")%>%
  padding(padding=1.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=11,part="header")%>%print("docx")


fill.date=seq(date.fun("2017-05-01"),date.fun("2020-09-01"),"1 month")
fill=data.frame(expand.grid(Date2=fill.date,variable="Hardness"))
fill$MonYr=format(fill$Date2,"%Y-%m")

tmp.dat$MonYr=format(tmp.dat$Date,"%Y-%m")
tmp.dat=merge(tmp.dat,fill,c("MonYr"),all.y=T)

# png(filename=paste0(plot.path,"PLSF_InLakeHardness.png"),width=5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,0.5),oma=c(3,2,0.5,0.25));

col.val=wesanderson::wes_palette("Zissou1",1)
xlim.val=date.fun(c("2017-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")
ylim.val=c(0,150);by.y=50;ymaj=ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Hardness~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(tmp.dat,pt_line(Date,Hardness,2,"grey",1,21,col.val,1,0.01))
with(tmp.dat,pt_line(Date,hard.recalc,2,adjustcolor("grey",0.5),1,21,adjustcolor("indianred1",0.5),1,0.01,pt.col = adjustcolor("black",0.5)))
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
mtext(side=2,line=3.5,"Hardness (mg L\u207B\u00B9)")
mtext(side=3,adj=1,"In-Lake",font=3)
mtext(side=1,line=1.25,"Date (Month-Year)",outer=T)
legend("topright",legend=c("Provided","Recalculated"),
       pch=c(21),pt.bg=c(col.val,adjustcolor("indianred1",0.5)),pt.cex=c(1),
       lty=c(NA),lwd=c(0.01),col=c("grey"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5)

dev.off()



par(family="serif",mar=c(1,3.5,0.5,0.5),oma=c(3,2,0.5,0.25));

tmp.dat=subset(dat.melt,SITE=="PLSF-IL"&variable%in%c("Hardness","Ca","Mg",'Ba',"Mn"))
tmp.dat=dcast(tmp.dat,Date~variable,value.var = "HalfMDL")
tmp.dat$hard.recalc=with(tmp.dat,(2.497*Ca) + (4.119*Mg))
tmp.dat$Ba_WQS=with(tmp.dat,exp(1.0629*log(hard.recalc)+2.2354)/1000)
tmp.dat$Mn_WQS=with(tmp.dat,exp(0.8784*log(hard.recalc)+4.2889)/1000)

with(tmp.dat,sum(Ba>Ba_WQS))
with(tmp.dat,sum(Mn>Mn_WQS))
