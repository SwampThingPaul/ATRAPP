## Title:      Petit-lac-Saint-François - biotia changes
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
library(vegan)

library(flextable)
library(magrittr)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_Paper2/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

# -------------------------------------------------------------------------
## From "Phyto-Cyano-Zoop-Proto" PLSF database
bio.dat.head=read.xlsx(paste0(data.path,"PLSF Database Phyto-Cyano-Zoop-Proto_v2.xlsx"))[1:2,]
bio.dat.head=t(bio.dat.head)
bio.dat.head=cbind(data.frame(spp=rownames(bio.dat.head)),bio.dat.head)
colnames(bio.dat.head)=c("spp","col1","col2")
unique(bio.dat.head$col1)
bio.dat.head$col1=with(bio.dat.head,ifelse(col1=="Concentration","Conc",
                                           ifelse(col1=="Total biovolume","biovol",
                                                  ifelse(col1=="Total biomass","biomass",NA))))
unique(bio.dat.head$col2)
unit.xwalk=data.frame(col2=c(NA, "cells/ml", "µm3/mL", "#/L", "µg/L"),
                      col2a=c(NA,"cells.mL","um3.mL","num.L","ug.L"))
bio.dat.head$col2=with(bio.dat.head,ifelse(col2=="cells/ml","cells.mL",
                                           ifelse(col2=="µm3/mL","um3.mL",
                                                  ifelse(col2=="#/L","num.L",
                                                         ifelse(col2== "µg/L","ug.L",NA)))))
# bio.dat.head=merge(bio.dat.head,unit.xwalk,"col2",all.x=T,sort=F)
bio.dat.head$head.val=with(bio.dat.head,ifelse(spp=="Date","Date",paste(spp,col1,col2,sep="_")))

spp.check=strsplit(bio.dat.head$spp,"\\.")
# write.csv(data.frame(val=unique(sapply(spp.check,"[",1))),paste0(export.path,"PLSF_botia_genera.csv"),row.names = F)

bio.dat.head$spp.cat=NA
bio.dat.head[4:29,"spp.cat"]="phytoplankton"; # based on class
bio.dat.head[30:168,"spp.cat"]="cyanobacteria"; # cyano bacteria only to species level
bio.dat.head[175:178,"spp.cat"]="zooplankton"
bio.dat.head[182:183,"spp.cat"]="protozoa"
subset(bio.dat.head,is.na(spp.cat)==F&col1=="biovol")$head.val

spp.gen.cat=ddply(bio.dat.head,c("spp","spp.cat"),summarise,N.val=N.obs(spp))
spp.gen.cat=subset(spp.gen.cat,is.na(spp.cat)==F)

bio.dat=read.xlsx(paste0(data.path,"PLSF Database Phyto-Cyano-Zoop-Proto.xlsx"),startRow = 4,colNames=F)
colnames(bio.dat)=bio.dat.head$head.val
bio.dat$Date=date.fun(convertToDate(bio.dat$Date))
bio.dat$Site="Lake_Outlet"

diversity.vars=subset(bio.dat.head,is.na(col1)==T)$head.val
bio.dat_div=bio.dat[,diversity.vars]
colnames(bio.dat_div)=c("Date",paste0("Phyto_",c("rich",'SWDiv',"Even")),paste0("Zoo_",c("rich",'SWDiv',"Even")))
bio.dat_div$CY=as.numeric(format(bio.dat_div$Date,"%Y"))
bio.dat_div$month=as.numeric(format(bio.dat_div$Date,"%m"))
bio.dat_div$dec.date=lubridate::decimal_date(bio.dat_div$Date)


## Trend Phytoplankton Richness -------------------------------------------
library(mblm)
bio.dat_div2=subset(bio.dat_div,CY>2011)
plot(Phyto_rich~Date,bio.dat_div2)
with(bio.dat_div2,cor.test(dec.date,Phyto_rich,method="kendall"))

pt.rslt=trend::pettitt.test(bio.dat_div2$Phyto_rich)
abline(v=bio.dat_div2[pt.rslt$estimate,])

# png(filename=paste0(plot.path,"PLSF_PhytoRich.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(3,3,0.5,0.25));

xlim.val=date.fun(c("2012-06-01","2020-12-31"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 year");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ylim.val=c(0,120);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(Phyto_rich~Date,bio.dat_div2,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
points(Phyto_rich~Date,bio.dat_div2,pch=21,bg="darkolivegreen2",lwd=0.1)
abline(v=bio.dat_div2[pt.rslt$estimate,],lty=2,lwd=2)

mod=mblm::mblm(Phyto_rich~dec.date,bio.dat_div2)
x.vals=data.frame(dec.date=seq(min(bio.dat_div2$dec.date),max(bio.dat_div2$dec.date),length.out=100))
x.vals2=date.fun(lubridate::date_decimal(x.vals$dec.date))
mod.pred=predict(mod,x.vals,interval="confidence")

lines(x.vals2,mod.pred[,1],col="indianred1",lwd=2)
lines(x.vals2,mod.pred[,2],lty=2,col="indianred1")
lines(x.vals2,mod.pred[,3],lty=2,col="indianred1")
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5);
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,'Phytoplankton Species Richness')
mtext(side=1,line=1.5,"Date (Month-Year)")
dev.off()

plot(Phyto_Even~Date,bio.dat_div2,type="b")





## quant mod (idea from - LTER pulse dynamics working group, July 2022, Jennifer Rudgers)
library(quantmod)
peak.thres=seq(0,40,1)
peak.sens=data.frame()
for(i in 1:length(peak.thres)){
  tmp=findPeaks(bio.dat_div2$Phyto_rich, thresh=peak.thres[i])
  rslt=data.frame(thres=peak.thres[i],Npeak=length(tmp))
  peak.sens=rbind(peak.sens,rslt)
}
plot(Npeak~thres,peak.sens);abline(v=10)

richpeaks=findPeaks(bio.dat_div2$Phyto_rich, thresh=10)
plot(Phyto_rich~Date,bio.dat_div2)
points(Phyto_rich~Date,bio.dat_div2[richpeaks-1,],pch=21,bg="black")

#how many peaks per total obs, which are in units of days (here, obs = 365 days*33 years) ?
peak_per_month<-length(richpeaks)/length(bio.dat_div2$Phyto_rich); #monthly data
peak_per_month

#average peak magnitude (in mm for precip)
peak_mean<-mean(bio.dat_div2[richpeaks-1,]$Phyto_rich)
peak_mean

#peak standard deviation
peak_sd<-sd(bio.dat_div2[richpeaks-1,]$Phyto_rich)
peak_sd

#peak CV
peak_CV<-peak_sd/peak_mean
peak_CV

peaksppt_phyto=bio.dat_div2[richpeaks-1,]

# peak number vs. time: Has the number of peaks increased/decreased over time?
# get slope of (number of peaks per year for each year) vs. year (and p-value) to look for temporal change in number of peaks

peaksppt_phyto.yr=ddply(peaksppt_phyto,"CY",summarise,mean.peak=mean(Phyto_rich),N.peak=N.obs(Phyto_rich))

# second, build the stats models and save the slope and p as output
peak.number.lm<-lm(scale(N.peak)~scale(CY),data=peaksppt_phyto.yr)
summary(peak.number.lm)

# centering is done by subtracting the column means (omitting NAs) of V1 or year from their corresponding column
# scaling is done by dividing the (centered) column of V1 or year by their standard deviations 
# this returns a standardized regression coefficient that makes it possible to compare across drivers that differ in measured units and/or timescales

#plot(peak.number.lm) # turn this on to check model statistical assumptions
lmsum.number<-summary(peak.number.lm)
peak.number.slope<-peak.number.lm$coefficients[2]
peak.number.slope
peak.number.p<-lmsum.number$coefficients[2,4]
peak.number.p

plot(N.peak~CY,peaksppt_phyto.yr)

# peak magnitude (all peaks) vs. time: Has the magnitude of peaks increased/decreased over time?
# get slope of magnitude of peaks vs. time (and p-value)
peak.magnitude.lm<-lm(scale(mean.peak)~scale(CY),data=peaksppt_phyto.yr)

# plot(peak.magnitude.lm) # turn this on to check model statistical assumptions
lmsum.mag<-summary(peak.magnitude.lm);lmsum.mag
peak.magnitude.slope<-peak.magnitude.lm$coefficients[2]
peak.magnitude.slope
peak.magnitude.p<-lmsum.mag$coefficients[2,4]
peak.magnitude.p

plot(mean.peak~CY,peaksppt_phyto.yr)

# Phytoplankton Class data ------------------------------------------------
vars=c("Date",subset(bio.dat.head,is.na(spp.cat)==F&col1=="biovol"&spp.cat=="phytoplankton")$head.val)
bio.dat2=bio.dat[,vars]
bio.dat2.melt=melt(bio.dat2,id.var="Date")
bio.dat2.melt=merge(bio.dat2.melt,bio.dat.head[,c("head.val","spp")],by.x="variable",by.y="head.val",all.x=T)
bio.dat2.melt$Date.num=lubridate::decimal_date(bio.dat2.melt$Date)

# Calculate relative total turnover within replicates
library(codyn)
total.res <- turnover(df=bio.dat2.melt,  
                      time.var = "Date.num", 
                      species.var = "spp",
                      abundance.var = "value")
plot(total~Date.num,total.res,type="l")

appear.res <- turnover(df=bio.dat2.melt,  
                      time.var = "Date.num", 
                      species.var = "spp",
                      abundance.var = "value",
                      metric="appearance")
plot(appearance~Date.num,appear.res,type="l")

disappear.res <- turnover(df=bio.dat2.melt,  
                       time.var = "Date.num", 
                       species.var = "spp",
                       abundance.var = "value",
                       metric="disappearance")
plot(disappearance~Date.num,disappear.res,type="l")


# bio.dat2.xtab=subset(bio.dat2.melt,is.na(value)==T)
bio.dat2.xtab=dcast(bio.dat2.melt,Date~spp,value.var = "value",sum)
# bio.dat2.xtab=subset(bio.dat2.xtab,as.numeric(format(bio.dat2.xtab$Date,"%Y"))>=2011)
bio.dat2.xtab=na.omit(bio.dat2.xtab)

# ### Using ordination to examine compositional variation in time.
# bio_dca=decorana(bio.dat2.xtab[,2:ncol(bio.dat2.xtab)]) 
# bio_dca
# 
# # Bray-Curtis dissimilarity: from vegdist function d[jk] = (sum abs(x[ij]-x[ik]))/(sum (x[ij]+x[ik]))
# bio_pco_bc <- cmdscale(vegdist(bio.dat2.xtab[,2:ncol(bio.dat2.xtab)], method = "bray"), eig = T) 
# 
# (var1 <- bio_pco_bc$eig[1] / sum(bio_pco_bc$eig[bio_pco_bc$eig > 0]))  # % variance explained
# (var2 <- bio_pco_bc$eig[2] / sum(bio_pco_bc$eig[bio_pco_bc$eig > 0]))
# 
# # Extract species scores: the correlation between the raw data and the transect scores
# pco.ca.otus <- as.data.frame(cor(bio.dat2.xtab[,2:ncol(bio.dat2.xtab)], bio_pco_bc$points)) 
# names(pco.ca.otus)[1:2] <- c("PCO1", "PCO2")
# head(pco.ca.otus)

# richness calculations
# bio.dat2.xtab$richness=specnumber(bio.dat2.xtab[,2:ncol(bio.dat2.xtab)])
specnumber(bio.dat2.xtab[,2:ncol(bio.dat2.xtab)])
apply(bio.dat2.xtab[,2:ncol(bio.dat2.xtab)],1,FUN=function(x) {length(x[x>0])})
