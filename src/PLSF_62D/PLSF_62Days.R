## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             62-day sampling
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

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_62day/","/Export/","/Data/","/GIS"))
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
# units = mg/L
TP.MDL=0.7*0.001
SRP.MDL=TP.MDL
DP.MDL=TP.MDL
DOP.MDL=NA; # DP-SRP
PP.MDL=NA; # TP-DP
NOx.MDL=0.0004
NH4.MDL=0.0014
TN.MDL=0.004
SolN.MDL=0.004
Urea.MDL=0.03
TKN.MDL=NA# TN.MDL; # Calculated value TN+NH4
DON.MDL=NA; # Calculated value DN-(DIN+urea)
TOC.MDL=0.15
SolOC.MDL=0.15
Turb.MDL=2
Color.MDL=15
Chla.MDL=0.1

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
Hard.MDL=NA; # Calculated as 2.497 (Ca) + 4.119 (Mg)
# Data --------------------------------------------------------------------
## From Nico
dat=read.csv(paste0(data.path,"PLSF_metadata_dailyandplus2019.csv"))
dat$Date=date.fun(dat$Date)
range(dat$Date)
## Make MDL/units table
field.param=c("Dissolved_oxygen_DO", "Dissolved_oxygen_DO_.",
              "Total_dissolved_solids","Specific_conductivity",
              "Phycocyanine._intake_Bedford_high_intesity",
              "Secchi", "Ambient_temperature", "Temperature_water", 
              "Average_wind_speed","pH")
calc.param=c("Dissolved_organic_P", "Particulate_P_PP",
             "Total_nitrogen_Kjeldahl","Soluble_organic_nitrogen_.DON",
             "Total_hardness_CaCO3")

MDL.tab=data.frame(variable=c("Aluminum", "Antimony", "Arsenic", "Amonia_nitrogen", "Soluble_nitrogen_DN", 
                              "Soluble_organic_nitrogen_.DON", "Total_nitrogen_Kjeldahl", "Total_nitrogen_TN", 
                              "Barium", "Cadmium", "Calcium", "Total_soluble_organic_carbon", 
                              "Total._organic_carbon", "Chrome", "Cobalt", "Specific_conductivity", 
                              "Copper", "Iron", "Magnesium", "Manganese", "Molybdenum", "Nickel", 
                              "Nitrite_nitrate", "SRP", "Dissolved_oxygen_DO", "Dissolved_oxygen_DO_.", 
                              "Dissolved_organic_P", "Dissolved_P_DP", "Particulate_P_PP", 
                              "Total_P_TP", "Lead", "Potassium", "Selenium", "Silver", "Sodium", 
                              "Total_dissolved_solids", "Total_hardness_CaCO3", "Turbidity", 
                              "Urea", "Zinc", "Chlorophylle_a", "Phycocyanine._intake_Bedford_high_intesity", 
                              "Secchi", "Ambient_temperature", "Temperature_water", "Average_wind_speed", 
                              "pH"),
                   MDL=c(Al.MDL,Sb.MDL,As.MDL,NH4.MDL/1000,SolN.MDL,
                         DON.MDL,TKN.MDL,TN.MDL,
                         Ba.MDL,Cd.MDL,Ca.MDL,SolOC.MDL,
                         TOC.MDL,Cr.MDL,Co.MDL,NA,
                         Cu.MDL,Fe.MDL,Mg.MDL,Mn.MDL,Mo.MDL,Ni.MDL,
                         NOx.MDL,SRP.MDL,NA,NA,
                         DOP.MDL,DP.MDL,PP.MDL,
                         TP.MDL,Pb.MDL,K.MDL,Se.MDL,Ag.MDL,Na.MDL,
                         NA,Hard.MDL,Turb.MDL,
                         Urea.MDL,Zn.MDL,Chla.MDL,NA,
                         NA,NA,NA,NA,NA),
                   Units=c("mgL","mgL","mgL","ugL","mgL",
                           "mgL","mgL","mgL",
                           "mgL","mgL","mgL","mgL",
                           "mgL","mgL","mgL","uScm",
                           "mgL","mgL","mgL","mgL","mgL","mgL",
                           "mgL","ugL","mgL","PerSat",
                           "ugL","ugL","ugL",
                           "ugL","mgL","mgL","mgL","mgL","mgL",
                           "mgL","mgL","NTU",
                           "mgL","mgL","ugL","ugL",
                           "cm","DegC","DegC","ms",
                           "SU"),
                   param=c("Al","Sb","As","NH4","DN",
                           "DON","TKN","TN",
                           "Ba","Cd","Ca","DOC",
                           "TOC","Cr","Co","SPC",
                           "Cu","Fe","Mg","Mn","Mo","Ni",
                           "NOx","SRP","DO","DO_PerSat",
                           "DOP","DP","PP",
                           "TP","Pb","K","Se","Ag","Na",
                           "TDS","Hard","Turb",
                           "Urea","Zn","Chla","Phyco",
                           "SDD","ATemp","WTemp","WindSp",
                           "pH"))
MDL.tab$group=NA
MDL.tab[MDL.tab$variable%in%field.param,"group"]="field"
MDL.tab[MDL.tab$variable%in%calc.param,"group"]="calc"
MDL.tab$group=with(MDL.tab,ifelse(is.na(group)==T,"lab",group))
# write.csv(MDL.tab,paste0(export.path,"metadata_MDLTable_62d.csv"),row.names = F)

### 
vars=names(dat)# [5:ncol(dat)]
dat.melt=melt(dat,id.vars = vars[1:4])
dat.melt=subset(dat.melt,is.na(value)==F)
dat.melt=merge(dat.melt,MDL.tab,"variable")

subset(dat.melt,value==0)
dat.melt$HalfMDL=with(dat.melt,ifelse(group=="field",value,ifelse(value<=MDL,MDL/2,value)))

plot(HalfMDL~value,subset(dat.melt,param=="TN"));abline(0,1)
### 
subset(MDL.tab,group=="calc")

dat.melt$param.unit=with(dat.melt,paste(param,Units,sep="."))
dat.xtab2=dcast(subset(dat.melt,!(group%in%c("calc"))),Date~param.unit,value.var = "HalfMDL",mean)

## Calcualted values
dat.xtab2$PP.ugL=with(dat.xtab2,TP.ugL-DP.ugL)
dat.xtab2$DOP.ugL=with(dat.xtab2,DP.ugL-SRP.ugL)

dat.xtab2$DIN.mgL=with(dat.xtab2,NOx.mgL+(NH4.ugL/1000))
dat.xtab2$TKN.mgL=with(dat.xtab2,TN.mgL+(NH4.ugL/1000))
dat.xtab2$DON.mgL=with(dat.xtab2,ifelse(is.na(Urea.mgL)==T,DN.mgL-DIN.mgL,DN.mgL-(DIN.mgL+Urea.mgL)))

# dat.xtab2$POC.mgL=with(dat.xtab2,TOC.mgL-DOC.mgL)

## Reversal screening
dat.xtab2$TPReversal=with(dat.xtab2,ifelse(is.na(SRP.ugL)==T|is.na(TP.ugL)==T,0,ifelse(SRP.ugL>(TP.ugL*1.3),1,0)));
dat.xtab2$TNReversal=with(dat.xtab2,ifelse(is.na(DIN.mgL)==T|is.na(TN.mgL)==T,0,ifelse(DIN.mgL>(TN.mgL*1.3),1,0)));

sum(dat.xtab2$TPReversal); # should be zero
sum(dat.xtab2$TNReversal); # should be zero

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN.mgL~DIN.mgL,dat.xtab2,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP.ugL~SRP.ugL,dat.xtab2,ylab="TP (ug L\u207B\u00b9)",xlab="SRP (ug L\u207B\u00b9)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

# If reversal values not 0 then use script
# dat.xtab2$TP=with(dat.xtab2,ifelse(TPReversal==1,NA,TP))
# dat.xtab2$SRP=with(dat.xtab2,ifelse(TPReversal==1,NA,SRP))
# dat.xtab2$TN=with(dat.xtab2,ifelse(TNReversal==1,NA,TN))
# dat.xtab2$DIN=with(dat.xtab2,ifelse(TNReversal==1,NA,DIN))

## Stoichiometry
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107

dat.xtab2$TP.mM=with(dat.xtab2,(TP.ugL/1000)/P.mw)
dat.xtab2$TN.mM=with(dat.xtab2,TN.mgL/N.mw)
dat.xtab2$SRP.mM=with(dat.xtab2,(SRP.ugL/1000)/P.mw)
dat.xtab2$DIN.mM=with(dat.xtab2,DIN.mgL/N.mw)
dat.xtab2$TOC.mM=with(dat.xtab2,TOC.mgL/C.mw)
dat.xtab2$DOC.mM=with(dat.xtab2,DOC.mgL/C.mw)

dat.xtab2$TN_TP=with(dat.xtab2,TN.mM/TP.mM)
dat.xtab2$TOC_TP=with(dat.xtab2,TOC.mM/TP.mM)
dat.xtab2$TOC_TN=with(dat.xtab2,TOC.mM/TN.mM)
dat.xtab2$DIN_SRP=with(dat.xtab2,DIN.mM/SRP.mM)
dat.xtab2$DOC_SRP=with(dat.xtab2,DOC.mM/SRP.mM)
dat.xtab2$DOC_DIN=with(dat.xtab2,DOC.mM/DIN.mM)

consec.event=date.fun(c("2019-06-18","2019-08-19"))
diff(consec.event)

plot(Chla.ugL~Date,dat.xtab2)

## Dataset comparison Chla
### 
dat2=read.xlsx(paste0(data.path,"PLSF Database-12 Years (v2021-07-07).xlsx"))
dat2$Date=date.fun(convertToDate(dat2$Date))

# Water quality specific parameters
wq.dat2=dat2[,c(1:20,364:374,387)]
names(wq.dat2)

# all WQ Vars
wq.vars=c("Date", "Site", "ENKI", "N_P", "TP.mgL", 
          "PP.calc.mgL", "DP.mgL","SRP.mgL", "DOP.calc.mgL", 
          "TN.mgL", "TKN.mgL", "NH4.mgL","NOx.mgL", "Urea.mgL", "DON.mgL", 
          "SolN.mgL", "SolOC.mgL", "TOC.mgL", 
          "pH", "Chla.ugL", "Cond", "DO.per", 
          "TDS.mgL", "Temp.C", "ORP.mV", "Sal",
          "Resistivity.ohm","Phyco.ugL", "TChl.ugL", 
          "Turb.NTU", "Colour_PCU","Secchi_cm")
colnames(wq.dat2)=wq.vars
wq.dat2$CY=as.numeric(format(wq.dat2$Date,"%Y"))
## double check data.frame(vals=names(wq.dat2),rename=wq.vars)

wq.dat2=subset(wq.dat2,Site=="In_Lake"&CY==2019)

plot(Chla.ugL~Date,wq.dat2)
plot(TChl.ugL~Date,wq.dat2)

tmp=merge(dat.xtab2[,c("Date","Chla.ugL")],
      wq.dat2[,c("Date","Chla.ugL")],
      "Date")
tmp$Chla.ugL.y=as.numeric(tmp$Chla.ugL.y)
plot(Chla.ugL.x~Chla.ugL.y,tmp,ylab="Chl-a from Nico",xlab=c("Chl-a from Barry"),pch=21,bg="red")
abline(0,1)
tmp$Chla.ugL.x-tmp$Chla.ugL.y



### Figures 

# png(filename=paste0(plot.path,"PLSF_62DayNut.png"),width=6.5,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,2),oma=c(2.5,5,1.5,2));
layout(matrix(1:4,4,1,byrow = T))

xlim.val=date.fun(c("2019-04-30","2019-10-29"));xmaj=seq(xlim.val[1],xlim.val[2],"4 weeks");xmin=seq(xlim.val[1],xlim.val[2],"1 week")
y1.lab.line=3.5
y2.lab.line=2.5
# pigment
ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val2=c(0,0.6);by.y2=0.2;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)
cols=c('forestgreen',"#66C2A4")
plot(Chla.ugL~Date,dat.xtab2,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
consec.period.y=c(ylim.val[1]-100,ylim.val[2]+ylim.val[2]*0.25)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
with(subset(dat.xtab2,is.na(Chla.ugL)==F),
     pt_line(Date,Chla.ugL,2,cols[1],1,21,cols[1]))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,ymaj)
par(new=T)
plot(Chla.ugL~Date,dat.xtab2,type="n",ann=F,axes=F,ylim=ylim.val2,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(subset(dat.xtab2,is.na(Phyco.ugL)==F),
     pt_line(Date,Phyco.ugL,2,cols[2],1,21,cols[2]))
axis_fun(4,ymaj2,ymin2,ymaj2);box(lwd=1)
mtext(side=2,line=y1.lab.line,"Chl-a (\u03BCg L\u207B\u00B9)")
mtext(side=4,line=y2.lab.line,"Phyco (\u03BCg L\u207B\u00B9)")
legend("topleft",legend=c("Chlorophyll-a","Phycocyanin"),
       pch=c(21),pt.bg=c(cols),pt.cex=c(1.5),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
# P
ylim.val=c(1,1000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
# cols=viridis::viridis(3,alpha=0.7)
cols=scico::scico(n=3,palette="imola")
plot(TP.ugL~Date,dat.xtab2,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
consec.period.y=c(ylim.val[1]-ylim.val[1]*0.5,ylim.val[2]+ylim.val[2]*0.25)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
with(subset(dat.xtab2,is.na(TP.ugL)==F),
     pt_line(Date,TP.ugL,2,cols[1],1,21,cols[1]))
with(subset(dat.xtab2,is.na(DP.ugL)==F),
     pt_line(Date,DP.ugL,2,cols[2],1,21,cols[2]))
with(subset(dat.xtab2,is.na(SRP.ugL)==F),
     pt_line(Date,SRP.ugL,2,cols[3],1,21,cols[3]))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("bottomleft",legend=c("TP","DP","SRP"),
       pch=c(21),pt.bg=c(cols),pt.cex=c(1.5),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=3,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
mtext(side=2,line=y1.lab.line,"Phosphorus (\u03BCg L\u207B\u00B9)")
# N
ylim.val=c(0.005,20);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
# cols=viridis::viridis(3,alpha=0.7)
cols=scico::scico(n=3,palette="berlin")
plot(TN.mgL~Date,dat.xtab2,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
consec.period.y=c(ylim.val[1]-ylim.val[1]*0.5,ylim.val[2]+ylim.val[2]*0.25)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
with(subset(dat.xtab2,is.na(TN.mgL)==F),
     pt_line(Date,TN.mgL,2,cols[1],1,21,cols[1]))
with(subset(dat.xtab2,is.na(DN.mgL)==F),
     pt_line(Date,DN.mgL,2,cols[2],1,21,cols[2]))
with(subset(dat.xtab2,is.na(DIN.mgL)==F),
     pt_line(Date,DIN.mgL,2,cols[3],1,21,cols[3]))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F,nsmall=1));box(lwd=1)
legend("bottomleft",legend=c("TN","DN","DIN"),
       pch=c(21),pt.bg=c(cols),pt.cex=c(1.5),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=3,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
mtext(side=2,line=y1.lab.line,"Nitrogen (mg L\u207B\u00B9)")

# total stoich
# N
ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val2=c(0,1500);by.y2=500;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)
cols=wesanderson::wes_palette("Zissou1",3,"continuous")
plot(TN_TP~Date,dat.xtab2,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
consec.period.y=c(ylim.val[1]-100,ylim.val[2]+ylim.val[2]*0.25)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
with(subset(dat.xtab2,is.na(TN_TP)==F),
     pt_line(Date,TN_TP,2,cols[1],1,21,cols[1]))
with(subset(dat.xtab2,is.na(TOC_TN)==F),
     pt_line(Date,TN_TP,2,cols[2],1,21,cols[2]))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d")); axis_fun(2,ymaj,ymin,ymaj)
par(new=T)
plot(TOC_TP~Date,dat.xtab2,type="n",ann=F,axes=F,ylim=ylim.val2,xlim=xlim.val)
# abline(h=ymaj2,v=xmaj2,lty=3,col="grey",lwd=0.75)
with(subset(dat.xtab2,is.na(TOC_TP)==F),
     pt_line(Date,TOC_TP,2,cols[3],1,21,cols[3]))
axis_fun(4,ymaj2,ymin2,ymaj2);box(lwd=1)
mtext(side=2,line=y1.lab.line,"TN:TP or TOC:TN\n(molar ratio)")
mtext(side=4,line=y2.lab.line,"TOC:TP (molar ratio)")
legend("topleft",legend=c("TN:TP","TOC:TN","TOC:TP"),
       pch=c(21),pt.bg=c(cols),pt.cex=c(1.5),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
mtext(side=1,line=2,"Date (M-D-2019)")
dev.off()


plot(Chla.ugL~TN_TP,dat.xtab2)
plot(Chla.ugL~TOC_TP,dat.xtab2)
plot(Chla.ugL~TOC_TN,dat.xtab2)

plot(Chla.ugL~DIN_SRP,dat.xtab2)
plot(Chla.ugL~DOC_SRP,dat.xtab2)
plot(Chla.ugL~DOC_DIN,dat.xtab2)

plot(TOC_TP~TOC_TN,dat.xtab2)

plot(DIN_SRP~Date,dat.xtab2)


# Toxin data --------------------------------------------------------------
tox.mdl=read.xlsx(paste0(data.path,"LOD and LOQ toxins for Paul.xlsx"),sheet=2)

tox.dat=read.table(paste0(data.path,"PLSF_toxins_dailyandplus2019.txt"))
tox.dat$Date=as.POSIXct(tox.dat$Date)

idvars=c('ID',"Date","Point","Site")
paramvars=names(tox.dat)[6:39]

tox.ls=strsplit(paramvars,"_")
tox.ls=data.frame(variable=paramvars,matrix=sapply(tox.ls,"[",1),tox=ifelse(sapply(tox.ls,"[",2)=="Cyn",
                                               sapply(tox.ls,"[",2),
                                               paste(sapply(tox.ls,"[",2),sapply(tox.ls,"[",3),sep="-")))
tox.ls=merge(tox.ls,tox.mdl,by.x=c('matrix','tox'),by.y=c('matrix','toxin'))

tox.dat.melt=melt(tox.dat,id.vars = idvars)
tox.dat.melt=merge(tox.dat.melt,tox.ls,"variable")

# if toxin detected value = 1 (removed for statistics)
tox.dat.melt$value=with(tox.dat.melt,ifelse(value==1,NA,value))
tox.dat.melt$HalfMDL=with(tox.dat.melt,ifelse(value<MDL|value==0,MDL/2,value))



plot(HalfMDL~Date,subset(tox.dat.melt,matrix=="extra"&tox=="ANA-a"))
plot(HalfMDL~Date,subset(tox.dat.melt,matrix=="intra"&tox=="ANA-a"))

plot(HalfMDL~Date,subset(tox.dat.melt,matrix=="intra"&tox=="HANA-a"))
plot(HalfMDL~Date,subset(tox.dat.melt,matrix=="extra"&tox=="HANA-a"))


toxins.grps=list(c("AP-A","AP-B"),
                 c(paste("dmMC",c("LR","RR"),sep="-")),
                 c(paste("MC",c("HiIR","HtyR","LA","LF","LR","LW","LY","RR","WR","YR"),sep="-")))
tox.rng=ddply(tox.dat.melt,c("matrix","tox"),summarise,
      min.val=min(HalfMDL,na.rm=T),
      max.val=max(HalfMDL,na.rm=T))

floor(round(tox.rng$min.val-tox.rng$min.val*0.2,-1))
ceiling(round(tox.rng$max.val,))

xlim.val=date.fun(c("2019-04-30","2019-10-29"));xmaj=seq(xlim.val[1],xlim.val[2],"4 weeks");xmin=seq(xlim.val[1],xlim.val[2],"1 week")

# png(filename=paste0(plot.path,"PLSF_62DayTox.png"),width=6.5,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,1.5),oma=c(2.5,4,1.5,2));
layout(matrix(1:9,3,3,byrow = T),widths = c(1,1,0.3))
j=1
ma.val="extra"
tox.ls.plt=subset(tox.rng,matrix==ma.val&tox%in%toxins.grps[[j]])
tox.ls.plt=subset(tox.ls.plt,min.val!=max.val)
ylim.val=c(floor(round(min(tox.ls.plt$min.val)-min(tox.ls.plt$min.val)*0.2,-1)),
           ceiling(max(tox.ls.plt$max.val)))
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

cols=viridisLite::turbo(nrow(tox.ls.plt),alpha=0.5)
plot(HalfMDL~Date,tox.dat.melt,ylim=ylim.val,log="y",type="n",ann=F,axes=F)
consec.period.y=c(ylim.val[1]-9,ylim.val[2]+ylim.val[2]*0.25)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
for(i in 1:nrow(tox.ls.plt)){
  with(subset(tox.dat.melt,matrix==ma.val&tox==tox.ls.plt$tox[i]),
       pt_line(Date,HalfMDL,2,cols[i],1,21,cols[i],pt.col=adjustcolor("grey",0.5),pt.lwd = 0.01))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,NA);box(lwd=1)
mtext(side=3,"Extracellular")

ma.val="intra"
tox.ls.plt=subset(tox.rng,matrix==ma.val&tox%in%toxins.grps[[j]])
tox.ls.plt=subset(tox.ls.plt,min.val!=max.val)
ylim.val=c(round(min(tox.ls.plt$min.val)-min(tox.ls.plt$min.val)*0.3,2),
           ceiling(max(tox.ls.plt$max.val)))
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

cols=viridisLite::turbo(nrow(tox.ls.plt),alpha=0.5)
plot(HalfMDL~Date,tox.dat.melt,ylim=ylim.val,log="y",type="n",ann=F,axes=F)
consec.period.y=c(ylim.val[1]-0.07,ylim.val[2]+ylim.val[2]*0.7)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
for(i in 1:nrow(tox.ls.plt)){
  with(subset(tox.dat.melt,matrix==ma.val&tox==tox.ls.plt$tox[i]),
       pt_line(Date,HalfMDL,2,cols[i],1,21,cols[i],pt.col=adjustcolor("grey",0.5),pt.lwd = 0.01))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,NA);box(lwd=1)
mtext(side=3,"Intracellular")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=toxins.grps[[j]],
       pch=c(21),pt.bg=c(cols),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5
)

j=2
ma.val="extra"
tox.ls.plt=subset(tox.rng,matrix==ma.val&tox%in%toxins.grps[[j]])
tox.ls.plt=subset(tox.ls.plt,min.val!=max.val)
ylim.val=c(floor(round(min(tox.ls.plt$min.val)-min(tox.ls.plt$min.val)*0.2,-1)),
           ceiling(max(tox.ls.plt$max.val)))
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

cols=viridisLite::turbo(nrow(tox.ls.plt),alpha=0.5)
plot(HalfMDL~Date,tox.dat.melt,ylim=ylim.val,log="y",type="n",ann=F,axes=F)
consec.period.y=c(ylim.val[1]-9,ylim.val[2]+ylim.val[2]*0.7)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
for(i in 1:nrow(tox.ls.plt)){
  with(subset(tox.dat.melt,matrix==ma.val&tox==tox.ls.plt$tox[i]),
       pt_line(Date,HalfMDL,2,cols[i],1,21,cols[i],pt.col=adjustcolor("grey",0.5),pt.lwd = 0.01))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,NA);box(lwd=1)

ma.val="intra"
tox.ls.plt=subset(tox.rng,matrix==ma.val&tox%in%toxins.grps[[j]])
tox.ls.plt=subset(tox.ls.plt,min.val!=max.val)
ylim.val=c(round(min(tox.ls.plt$min.val)-min(tox.ls.plt$min.val)*0.3,2),
           ceiling(max(tox.ls.plt$max.val)))
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

cols=viridisLite::turbo(nrow(tox.ls.plt),alpha=0.5)
plot(HalfMDL~Date,tox.dat.melt,ylim=ylim.val,log="y",type="n",ann=F,axes=F)
consec.period.y=c(ylim.val[1]-0.06,ylim.val[2]+ylim.val[2]*0.7)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
for(i in 1:nrow(tox.ls.plt)){
  with(subset(tox.dat.melt,matrix==ma.val&tox==tox.ls.plt$tox[i]),
       pt_line(Date,HalfMDL,2,cols[i],1,21,cols[i],pt.col=adjustcolor("grey",0.5),pt.lwd = 0.01))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,NA);box(lwd=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=toxins.grps[[j]],
       pch=c(21),pt.bg=c(cols),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5
)

j=3
ma.val="extra"
tox.ls.plt=subset(tox.rng,matrix==ma.val&tox%in%toxins.grps[[j]])
tox.ls.plt=subset(tox.ls.plt,min.val!=max.val)
ylim.val=c(floor(round(min(tox.ls.plt$min.val)-min(tox.ls.plt$min.val)*0.2,-1)),
           ceiling(max(tox.ls.plt$max.val)))
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

cols=viridisLite::turbo(nrow(tox.ls.plt),alpha=0.5)
plot(HalfMDL~Date,tox.dat.melt,ylim=ylim.val,log="y",type="n",ann=F,axes=F)
consec.period.y=c(ylim.val[1]-9,ylim.val[2]+ylim.val[2]*0.7)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
for(i in 1:nrow(tox.ls.plt)){
  with(subset(tox.dat.melt,matrix==ma.val&tox==tox.ls.plt$tox[i]),
       pt_line(Date,HalfMDL,2,cols[i],1,21,cols[i],pt.col=adjustcolor("grey",0.5),pt.lwd = 0.01))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5,cex=0.8);box(lwd=1)
mtext(side=1,line=2,"Date (M-D-2019)")

ma.val="intra"
tox.ls.plt=subset(tox.rng,matrix==ma.val&tox%in%toxins.grps[[j]])
tox.ls.plt=subset(tox.ls.plt,min.val!=max.val)
ylim.val=c(round(min(tox.ls.plt$min.val)-min(tox.ls.plt$min.val)*0.3,2),
           ceiling(max(tox.ls.plt$max.val)))
ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")

cols=viridisLite::turbo(nrow(tox.ls.plt),alpha=0.5)
plot(HalfMDL~Date,tox.dat.melt,ylim=ylim.val,log="y",type="n",ann=F,axes=F)
consec.period.y=c(ylim.val[1]-0.07,ylim.val[2]+ylim.val[2]*0.7)
polygon(sort(rep(consec.event,2)),
        c(consec.period.y,rev(consec.period.y)),col=adjustcolor("grey",0.5),border=NA)
for(i in 1:nrow(tox.ls.plt)){
  with(subset(tox.dat.melt,matrix==ma.val&tox==tox.ls.plt$tox[i]),
       pt_line(Date,HalfMDL,2,cols[i],1,21,cols[i],pt.col=adjustcolor("grey",0.5),pt.lwd = 0.01))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5,cex=0.8);box(lwd=1)
mtext(side=1,line=2,"Date (M-D-2019)")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=toxins.grps[[j]],
       pch=c(21),pt.bg=c(cols),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5
)

mtext(side=2,outer=T,line=2,"Toxin Concentration (ng L\u207B\u00B9)")
dev.off()

# Genomic Data ------------------------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7373998/
# https://www.reneshbedre.com/blog/expression_units.html

gene.dat=read.table(paste0(data.path,"PLSF_filt_fin_daily.tsv"), header = TRUE)


## relative abundance
gene.dat2=gene.dat

x.val=apply(gene.dat2[,3:ncol(gene.dat2)],1,sum)
gene.dat2[,3:ncol(gene.dat2)]=sweep(gene.dat2[,3:ncol(gene.dat2)],1,x.val,"/")

for(i in 3:ncol(gene.dat2)){
  gene.dat2[,i]=(gene.dat2[,i]/sum(gene.dat2[,i],na.rm=T))*100
}
gene.dat2[,3]
sort(gene.dat2[,3],decreasing = T)

head(order(gene.dat2[,3],decreasing = T))
head(order(gene.dat2[,15],decreasing = T))

name.var=gene.dat2[order(rowSums(gene.dat[,3:ncol(gene.dat2)]),decreasing = T),"name"]
ms.names=which(substr(name.var,1,11)=="Microcystis")

# reordered
name.var=c(name.var[ms.names],name.var[substr(name.var,1,11)!="Microcystis"])

spp.name.group=data.frame(name=name.var,
           name2=c(name.var[1:6],rep("other",length(name.var[7:length(name.var)]))))

gene.dat.melt=melt(gene.dat2,id.vars = c("name","taxonomy_id"))
gene.dat.melt$variable=as.numeric(substr(gene.dat.melt$variable,2,10))
dat$ID=as.numeric(dat$ID)
gene.dat.melt=merge(gene.dat.melt,dat[,c("ID","Date")],by.x="variable",by.y="ID")
gene.dat.melt2=gene.dat.melt
gene.dat.melt=merge(gene.dat.melt,spp.name.group,"name")

ddply(gene.dat.melt,c("Date","name2"),summarise,total.sum=sum(value,na.rm=T))

rel.gene.dat=dcast(gene.dat.melt,Date~name2,value.var = "value",sum,na.rm=T)
rel.gene.dat=rel.gene.dat[,c(1,8:2)]

fill.date=data.frame(Date=seq(min(dat$Date),max(dat$Date),"1 day"),fill=1)
rel.gene.dat=merge(rel.gene.dat,fill.date,"Date",all.y=T)


cols=c("ivory",viridis::cividis(6,alpha=0.75))
# cols=adjustcolor(wesanderson::wes_palette("Zissou1",7,"continuous"),0.75)
# png(filename=paste0(plot.path,"PLSF_62Day_Genomic.png"),width=10,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,2),oma=c(2,2,0.5,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.5))

xlim.val=date.fun(c("2019-04-30","2019-10-29"));xmaj=seq(xlim.val[1],xlim.val[2],"4 weeks");xmin=seq(xlim.val[1],xlim.val[2],"1 week")
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(t(rel.gene.dat[,2:(ncol(rel.gene.dat)-1)]),space=0,col=cols,ann=F,axes=F,ylim=ylim.val)
# abline(v=x[which(fill.date$Date%in%consec.event)],col="black",lwd=2)
axis_fun(1,which(fill.date$Date%in%xmaj),which(fill.date$Date%in%xmin),format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);

# axis(3,x[which(fill.date$Date%in%consec.event)])
box(lwd=1)
mtext(side=1,line=1.5,"Date (M-D-2019)")
mtext(side=2,line=2,"Relative Abundance")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0,legend=row.names(t(rel.gene.dat[,2:(ncol(rel.gene.dat)-1)])),
       pch=c(22),pt.bg=c(cols),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5
       )
dev.off()


## Different order
name.var=gene.dat2[order(rowSums(gene.dat[,3:ncol(gene.dat2)]),decreasing = T),"name"]

spp.split=strsplit(name.var,"_")
spp.name.group=data.frame(name=name.var,Genus.name=sapply(spp.split,"[",1),Spp.name=sapply(spp.split,"[",2))
spp.name.group$group=with(spp.name.group,ifelse(!(Genus.name%in%c("Planktothrix","Microcystis","Anabaena","Dolichospermum")),"Other",Genus.name))

ddply(spp.name.group,"Genus.name",summarise,N.val=N.obs(Genus.name))

gene.dat.melt2=merge(gene.dat.melt2,spp.name.group,"name",all.x=T)

ddply(gene.dat.melt2,c("Date","group"),summarise,total.sum=sum(value,na.rm=T))

rel.gene.dat2=dcast(gene.dat.melt2,Date~group,value.var = "value",sum,na.rm=T)
vars=c("Date", "Other","Planktothrix","Anabaena", "Dolichospermum", "Microcystis")
rel.gene.dat2=rel.gene.dat2[,vars]

rel.gene.dat2=merge(rel.gene.dat2,fill.date,"Date",all.y=T)


cols=c("grey",rev(RColorBrewer::brewer.pal(4, "BuGn")))
cols=c("grey",colorRampPalette(c("forestgreen","darkolivegreen3","lightblue"))(4))
cols=c("grey",rev(RColorBrewer::brewer.pal(4, "Dark2")))
# cols=c("grey",LimnoPalettes::limno_palette("Bloom2",4,"continuous"))

# png(filename=paste0(plot.path,"PLSF_62Day_Genomic_v2.png"),width=10,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,2),oma=c(2,2,0.5,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow = T),heights=c(1,0.5))

xlim.val=date.fun(c("2019-04-30","2019-10-29"));xmaj=seq(xlim.val[1],xlim.val[2],"4 weeks");xmin=seq(xlim.val[1],xlim.val[2],"1 week")
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(t(rel.gene.dat2[,2:(ncol(rel.gene.dat2)-1)]),space=0,col=cols,ann=F,axes=F,ylim=ylim.val)
abline(v=x[which(fill.date$Date%in%consec.event)],col="black",lwd=2)
axis_fun(1,which(fill.date$Date%in%xmaj),which(fill.date$Date%in%xmin),format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);
# axis(3,x[which(fill.date$Date%in%consec.event)])
box(lwd=1)
mtext(side=1,line=1.5,"Date (M-D-2019)")
mtext(side=2,line=2,"Relative Abundance")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0,legend=row.names(t(rel.gene.dat2[,2:(ncol(rel.gene.dat2)-1)])),
       pch=c(22),pt.bg=c(cols),pt.cex=c(2),
       lty=c(NA),lwd=c(0.01),col=c("black"),
       ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj = 0,title="Genus"
)
dev.off()

###
head(gene.dat.melt)

gene.dat.xtab2=dcast(gene.dat.melt,Date~name,value.var = "value",mean)
head(gene.dat.xtab2,2L)

# https://rdrr.io/cran/labdsv/man/hellinger.html
library(labdsv)
tmp=hellinger(gene.dat.xtab2[,2:ncol(gene.dat.xtab2)])
rowSums(tmp[,2:ncol(tmp)])

tmp[,1]

tmp2=vegan::decostand(gene.dat.xtab2[,2:ncol(gene.dat.xtab2)],method="hellinger")
tmp2[,1]
# https://www.davidzeleny.net/anadat-r/doku.php/en:confusions
# vltava.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)
# vltava.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-env.txt')
# 
# library (vegan)
# 
# spe.hell <- decostand (log1p (vltava.spe), method = 'hellinger')
# tbRDA <- rda (spe.hell ~ pH + SOILDPT + ELEVATION, data = vltava.env[,1:18])
# 
# par (mfrow = c(1,2))
# ordiplot (tbRDA)
# ef <- envfit (tbRDA ~ pH + SOILDPT + ELEVATION, data = vltava.env, display = 'lc')
# ef12 <- envfit (tbRDA ~ pH + SOILDPT + ELEVATION, data = vltava.env)
# ef123 <- envfit (tbRDA ~ pH + SOILDPT + ELEVATION, data = vltava.env, choices = 1:3)
# ordiplot (tbRDA, type = 'n')
# plot (ef, col = 'blue')
# plot (ef12, col = 'red')
# plot (ef123, col = 'green')
# legend ('bottomright', lwd = 1, col = c('blue', 'red', 'green'), legend = c('envfit on linear combination of scores', 'envfit  on sample scores (1st & 2nd axis)', 'envfit on sample scores (1st, 2nd and 3rd axis)'), bty = 'n')

# might be useful
# https://microbiome.github.io/tutorials/
# package not installing.

## RDA explained
## https://pmassicotte.github.io/stats-denmark-2019/07_rda.html#/
## https://r.qcbs.ca/workshop10/book-en/partial-redundancy-analysis.html

# PCA ---------------------------------------------------------------------
library(vegan)
library(REdaS)

head(dat.xtab2)
head(rel.gene.dat2)

attributes(dat.xtab2$Date)
attributes(rel.gene.dat2$Date)

wq.gene=merge(dat.xtab2,rel.gene.dat2,"Date")

wq.gene.period=subset(wq.gene,Date%in%seq(consec.event[1],consec.event[2],"1 days"))

plot(TN_TP~Anabaena,wq.gene.period)
plot(TN_TP~Dolichospermum,wq.gene.period)


wq.gene.period[,c("NOx.mgL","NH4.ugL","Urea.mgL","DIN.mgL","TP.ugL","SRP.ugL","TN.mgL")]
vars=c("Date","DO_PerSat.PerSat","TN.mgL","TP.ugL","SRP.ugL","TN_TP", 
       "Anabaena", "Dolichospermum","Microcystis")

pca.dat=wq.gene.period[,vars]

#How many rows of data do we have?
nrow(pca.dat)
nrow(na.omit(pca.dat))
pca.dat=na.omit(pca.dat)

# KMOS
KMOS(pca.dat[,vars[2:length(vars)]])

# Bartlett's Test Of Sphericity
bart_spher(pca.dat[,vars[2:length(vars)]])

## PCA
pca.dat.pca=rda(pca.dat[,vars[2:length(vars)]],scale = T)

eig <- pca.dat.pca$CA$eig
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca

layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2,0.75,1),oma=c(2,1,0.25,0.5));

ylim.val=c(0,6);by.y=1;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2)
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n")
abline(h=ymaj,lty=3,col="grey")
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n",add=T)
abline(h=1,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.5,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
#mtext(side=1,line=1.5,"Principal Components")
mtext(side=2,line=1.5,"Eigenvalue")

ylim.val=c(0,120);by.y=25;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2);#set y limit and delineates the major and minor ticks
x=barplot(eig.pca$variance,ylim=ylim.val,col="white",border=0,yaxt="n")# inital plot to get the measurements
abline(h=ymaj,lty=3,col="grey")#makes vertical lines from y axis
x=barplot(eig.pca$variance,ylim=ylim.val,col="grey",yaxt="n",add=T)# the real plot that matters
lines(x,eig.pca$cumvariance,col="indianred1",lwd=2)# adds the cumulative variance for each factor
points(x,eig.pca$cumvariance,pch=21,bg="indianred1",cex=1.25)
abline(h=80,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.5,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
mtext(side=1,line=1.5,"Principal Components")
mtext(side=2,line=1.75,"Percentage of Variances")
legend.text=c("Absolute","Cumulative");#helper vaiable for legend
pt.col=c("grey","indianred1")#helper vaiable for legend
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col=c("black",pt.col[2]),lty=c(0,1),lwd=1.5,pt.cex=1,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col="black",lty=0,lwd=0.5,pt.cex=1,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)

scrs<-scores(pca.dat.pca,display=c("sites","species"),choices=c(1,2,3));
cols=c("grey","indianred1","forestgreen")

labs=rownames(scrs$species)
labs=c("DO", "TN", "TP", "SRP", "TN:TP", 
       "% Anabaena", "% Dolichospermum", "% Microcystis")

# png(filename=paste0(plot.path,"Quick_PCA_62D.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:2,1,2),widths=c(1,0.5))
par(family="serif",mar=c(1,2,0.75,1),oma=c(2,2,0.25,0.5));

xlim.val=c(-2,2);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-2,2);by.y=1;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA);
abline(h=0,v=0,lty=3,col="grey");
points(scrs$sites[,c(1,2)],pch=21,bg="grey",col="black",cex=1,lwd=0.5); #plots the points
arrows(0,0,scrs$species[,1],scrs$species[,2],length = 0.05, angle = 15, code = 2,col="indianred1",lwd=1.5);# makes the arrows
with(scrs,text(species[,1],species[,2],labels=labs,cex=0.75,font=3))
# pos=ifelse(species[,1]<0&species[,2]<0,2,
#           ifelse(species[,1]<0&species[,2]>0,2,
#                 ifelse(species[,1]>0,4,3)))));#adds labels to the arrows; 
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1); #adds x axis ticks
axis_fun(2,ymaj,ymin,format(ymaj),1); #adds y axis ticks
mtext(side=1,line=1.8,paste0("PCA 1 (",round(eig.pca$variance[1],1),"%)"));#adds x axis label with percent variance
mtext(side=2,line=2.25,paste0("PCA 2 (",round(eig.pca$variance[2],1),"%)"));#adds y axis label with percent variance

dev.off()


## 
plot(TN_TP~Anabaena,wq.gene.period)
plot(TN_TP~Dolichospermum,wq.gene.period)
plot(TN_TP~Microcystis,wq.gene.period)

with(wq.gene.period,cor.test(TN_TP,Anabaena,method="spearman"))
with(wq.gene.period,cor.test(TN_TP,Dolichospermum,method="spearman"))
with(wq.gene.period,cor.test(TN_TP,Microcystis,method="spearman"))
