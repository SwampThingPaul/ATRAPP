## Title:      Hudson Bay Data
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 01/18/2023

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
paths=paste0(wd,c("/Plots/HudsonBay/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paste0(wd,"/_documents/CNRC/20230118/")# paths[3]
GIS.path=paths[4]


# Redo Fig8 ---------------------------------------------------------------

dat1=read.xlsx(paste0(data.path,"Nathalie_combined_periods_Hudson Bay1Tagged.xlsx"))

dat1=dat1[,1:13]
colnames(dat1)=c("compound",names(dat1)[2:13])

