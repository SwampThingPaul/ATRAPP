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

# Genomic Data ------------------------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7373998/
# https://www.reneshbedre.com/blog/expression_units.html

gene.dat=read.table(paste0(data.path,"PLSF_filt_fin_daily.tsv"), header = TRUE)

spp.split=strsplit(gene.dat$name,"_")
spp.name.group=data.frame(name=gene.dat$name,Genus.name=sapply(spp.split,"[",1),Spp.name=sapply(spp.split,"[",2))

## to fix "[name]" values
tmp=strsplit(spp.name.group$Genus.name,"\\[|\\]")
spp.name.group$Genus.name=ifelse(nchar(sapply(tmp,"[",1))==0,sapply(tmp,"[",2),sapply(tmp,"[",1))
spp.name.group$group=with(spp.name.group,ifelse(!(Genus.name%in%c("Planktothrix","Microcystis","Anabaena","Dolichospermum")),"Other",Genus.name))
spp.name.group$group=with(spp.name.group,ifelse(group%in%c("Anabaena","Dolichospermum"),"Anabaena_Dolicho",group))

gene.dat.melt=melt(gene.dat,id.vars = c("name","taxonomy_id"))
gene.dat.melt$variable=as.numeric(substr(gene.dat.melt$variable,2,10))
dat$ID=as.numeric(dat$ID)
gene.dat.melt=merge(gene.dat.melt,dat[,c("ID","Date")],by.x="variable",by.y="ID")
gene.dat.melt=merge(gene.dat.melt,spp.name.group,"name")

gene.dat.melt2=dcast(gene.dat.melt,Date~group,value.var = "value",sum,na.rm=T)
gene.dat.melt2.hell=cbind(Date=gene.dat.melt2$Date,vegan::decostand(gene.dat.melt2[,2:ncol(gene.dat.melt2)],method="hellinger"))


## Combined WQ and Genetic data
wq.gene=merge(dat.xtab2,gene.dat.melt2.hell,"Date")
wq.gene=subset(wq.gene,Date%in%seq(consec.event[1],consec.event[2],"1 days"))

# wq.vars=c("DO_PerSat.PerSat","TN.mgL","TP.ugL","SRP.ugL","NH4.ugL","TN_TP") 
wq.vars=c("DO_PerSat.PerSat",
          "TP.ugL","TN.mgL",
          "SRP.ugL","NH4.ugL",
          "TN_TP","SDD.cm",
          "TDS.mgL","SPC.uScm") 
wq.vars=c("DO_PerSat.PerSat",
          "TN.mgL",
          "SRP.ugL","NH4.ugL",
          "TN_TP","SDD.cm") 
spp.vars=c("Planktothrix","Microcystis","Anabaena_Dolicho","Other")

pca.dat=na.omit(wq.gene[,c(wq.vars,spp.vars)])

# plot(TN.mgL~Microcystis,pca.dat)
# plot(NH4.ugL~Microcystis,pca.dat)
# plot(TN_TP~Microcystis,pca.dat)
# 
# languageR::pairscor.fnc(pca.dat)

## https://pmassicotte.github.io/stats-denmark-2019/07_rda.html#/
## https://r.qcbs.ca/workshop10/book-en/partial-redundancy-analysis.html
library(vegan)
# rda1=rda(pca.dat[,spp.vars]~DO_PerSat.PerSat+TN.mgL+TP.ugL+SRP.ugL+NH4.ugL+TN_TP,data=pca.dat[,wq.vars])
rda1=rda(pca.dat[,spp.vars]~.,data=pca.dat[,wq.vars])
summary(rda1)

# https://sites.google.com/site/mb3gustame/constrained-analyses/rda
plot(rda1)

RsquareAdj(rda1)

# global significance
anova.cca(rda1)

# axis significance
anova.cca(rda1, by="axis")

# term significance
anova.cca(rda1,by="terms")

#Linear dependencies 
sqrt(vif.cca(rda1))

# https://r.qcbs.ca/workshop10/book-en/variation-partitioning.html
# variation partitioning
var1="SDD.cm"
spe.part=varpart(pca.dat[,spp.vars],pca.dat[,wq.vars[wq.vars!=var1]],pca.dat[,var1])
spe.part

plot(spe.part)


scores(rda1)
summary(rda1)
