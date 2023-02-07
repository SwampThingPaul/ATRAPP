## Title:      Petit-lac-Saint-François water quality data analysis (ATRAPP)
##             Combine GAM and concentration plots
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

## From 1_PLSF_WQ_V3.R
load(paste0(export.path,"PLSF_1/PLSF_GAM.RData"))

# -------------------------------------------------------------------------
sites.vals=c("Godbout","Lake_Outlet")

cols=viridisLite::viridis(4,alpha=0.4,option="E")[2:3]


wq.dat

plot(TP.ugL~Date,subset(wq.dat,Site==sites.vals[1]))

range(subset(wq.dat,Site==sites.vals[1])$Date)
pred.dat=data.frame(Date=seq(date.fun("2009-01-01"),date.fun("2020-09-01"),"7 days"))
pred.dat$DOY=as.numeric(format(pred.dat$Date,"%j"))
# pred.dat$CY=as.numeric(format(pred.dat$Date,"%Y"))
pred.dat$CY=lubridate::decimal_date(pred.dat$Date)



pred.m.TP.in=predict.gam(m.TP.in,pred.dat,type="terms")

pDOYCY=exp(attr(pred.m.TP.in,"constant")+pred.m.TP.in[,3])

lines(pred.dat$Date,pDOYCY,col="Red")

plot(pred.m.TP.in[,1])
plot(pred.m.TP.in[,2])
plot(pred.m.TP.in[,3])


## look at this https://stats.stackexchange.com/questions/349247/overlaying-gam-and-lm-in-r
## https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

