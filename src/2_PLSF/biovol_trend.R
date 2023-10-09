
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
library(ggplot2)

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"
paths=paste0(wd,c("/Plots/PLSF_Paper2/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]



plsf.phyto=read.csv(paste0(export.path,"PLSF_microscope_data_phyto.csv"))
plsf.phyto$date=date.fun(plsf.phyto$date)
plsf.phyto$month=as.numeric(format(plsf.phyto$date,"%m"))
plsf.phyto$CY=as.numeric(format(plsf.phyto$date,"%Y"))

## Just for summer season
# plsf.phyto=subset(plsf.phyto,month%in%seq(5,10,1))

spp.class.order=read.csv(paste0(export.path,"20230317_GenusSpp_ClassOrder.csv"))
head(spp.class.order)

spp.class.order=rbind(spp.class.order,
                      data.frame(GenusSpp="Cryptomonas marsonii",
                                 genus="Cryptomonas",
                                 order="Cryptomonadales",
                                 class="Cryptophyceae"))


subset(spp.class.order,GenusSpp=="nostocalean filament sp.")
diazotroph.genera=c("Dolichospermum", "Anabaena", "Aphanizomenon", "Cuspidothrix", 
                    "Aphanocapsa", "Aphanothece", "Komvophoron", "Oscillatoria", 
                    "Phormidium", "Planktolyngbya", "Pseudanabaena", "Sphaerospermopsis", 
                    "Synechococcus", "Synechocystis")
# nostocalean filament sp. also a diazatrophic spp
nondiazotroph.genera=c("Chroococcus", "Limnococcus", "Eucapsis", "Coelomoron", "Coelosphaerium", 
                       "Cyanocatena", "Cyanodictyon", "Cyanogranis", "Merismopedia", 
                       "Microcystis", "Planktothrix", "Snowella", "Spirulina", "Woronichinia"
)
other.nondiaz.GenusSpp=c("Cyanophyte colony", "cyanophyte colony sp.", "cyanophyte cell pair spp.", 
                         "cyanophyte filament sp.", "cyanophyte unicell, oval/rod  2.5-5um spp.", 
                         "cyanophyte unicell, sphere  2.5-5um spp.", "cyanophyte unicelll, oval/rod  2.5-5um spp.", 
                         "cyanophyte cell pair, oval/rod spp.", "cyanophyte tetrad spp."
)

spp.class.order$diaztroph=with(spp.class.order,ifelse(genus%in%diazotroph.genera|
                                                        GenusSpp=="nostocalean filament sp.",1,
                                                      ifelse(genus%in%nondiazotroph.genera|
                                                               GenusSpp%in%other.nondiaz.GenusSpp,0,NA)))
spp.class.order$diaztroph.fac=with(spp.class.order,ifelse(is.na(diaztroph)==T,"NoClass",
                                                          ifelse(diaztroph==1,"diaz",
                                                                 ifelse(diaztroph==0,"nondiaz",NA))))

unique(spp.class.order$class)
spp.class.order$class[spp.class.order$class=="Cyanobacteriia"]="Cyanophyceae"

plsf.phyto$GenusSpp=trimws(plsf.phyto$GenusSpp)
plsf.phyto=merge(plsf.phyto,spp.class.order,"GenusSpp",all.x=T)
sum(is.na(plsf.phyto$diaztroph.fac))

plsf.phyto$diaztroph.fac_cyano=with(plsf.phyto,
                                    ifelse(class=="Cyanophyceae",
                                           paste0("cyano_",diaztroph.fac),
                                           paste0("Other_",diaztroph.fac)))

subset(plsf.phyto,CY==2017)
plsf.phyto2=ddply(plsf.phyto,c("CY","month","GenusSpp","genus","order","class",
                               "diaztroph.fac","diaztroph.fac_cyano"),
                  summarise,
                  mean.biovol.um3mL=mean(totbiovol.um3mL,na.rm=T),
                  mean.conc.cellsmL=mean(Conc.cellsmL,na.rm=T))
#### 
plsf.phyto.class=ddply(plsf.phyto2,c("CY","month","class"),summarise,
                       sum.biovol.um3mL=sum(mean.biovol.um3mL,na.rm=T),
                       sum.conc.cellsmL=sum(mean.conc.cellsmL,na.rm=T))
plsf.phyto.class$log10.biovol=with(plsf.phyto.class,ifelse(sum.biovol.um3mL==0,0,log10(sum.biovol.um3mL)))
plsf.phyto.class$biovol.mm3L=plsf.phyto.class$sum.biovol.um3mL*1e-6

class.reclass=data.frame(class=c("Cyanophyceae", "Cryptophyceae", "Bacillariophyceae", "Dinophyceae", 
                                 "Chlorophyceae", "Chrysophyceae", "Euglenoidea", "Prasinophyceae", 
                                 "Trebouxiophyceae", "Prymnesiophyceae", "Zygnematophyceae", "Eurotiomycetes", 
                                 "unid", "Katablepharidea", "Euglenophyceae", "Xanthophyceae", 
                                 "Eustigmatophyceae", "Liliopsida", "Klebsormidiophyceae"),
                         class.reclass=c("Cyanophyceae", "Cryptophyceae", "Bacillariophyceae", "Dinophyceae", 
                                         "Chlorophyceae", rep("Other",14)))


phyto.class.biovol=merge(plsf.phyto.class,class.reclass,by.x="class",by.y="class")
phyto.class.biovol.sum=ddply(phyto.class.biovol,c("CY","month","class.reclass"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))
phyto.class.biovol.sum$Tbiovol.log10=with(phyto.class.biovol.sum,ifelse(Tbiovol==0,0,log10(Tbiovol)))


## Biovolume trend  --------------------------------------------------------
# phyto.class.biovol.sum=ddply(phyto.class.biovol,c("CY","month","class.reclass"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))

phyto.class.biovol.CY.sum=ddply(subset(phyto.class.biovol,month%in%seq(5,10,1)),c("CY","class.reclass"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))
# phyto.class.biovol.CY.sum=ddply(phyto.class.biovol,c("CY","class.reclass"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))
with(subset(phyto.class.biovol.CY.sum,class.reclass=="Cyanophyceae"&CY>=2011),cor.test(CY,Tbiovol,method="kendall"))
mblm(Tbiovol~CY,subset(phyto.class.biovol.CY.sum,class.reclass=="Cyanophyceae"&CY>=2011),repeated=T)

tmp=subset(phyto.class.biovol.CY.sum,class.reclass=="Cyanophyceae"&CY>=2011)
trend::pettitt.test(tmp$Tbiovol)


phyto.class.total.biovol.CY.sum=ddply(subset(phyto.class.biovol,month%in%seq(5,10,1)),c("CY"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))
# phyto.class.total.biovol.CY.sum=ddply(phyto.class.biovol,c("CY"),summarise,Tbiovol=sum(biovol.mm3L,na.rm=T))
with(subset(phyto.class.total.biovol.CY.sum,CY>=2011),cor.test(CY,Tbiovol,method="kendall"))
mblm(Tbiovol~CY,subset(phyto.class.total.biovol.CY.sum,CY>=2011),repeated=T)
tmp=subset(phyto.class.total.biovol.CY.sum,CY>=2011)
trend::pettitt.test(tmp$Tbiovol)


