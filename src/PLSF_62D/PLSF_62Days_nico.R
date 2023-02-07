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



# -------------------------------------------------------------------------



# Genomic Data ------------------------------------------------------------
## Following code from Nico
library("phyloseq")
library("ranacapa")
library("fantaxtic")

microbe<-read.table(paste0(data.path,"62Day_20221109/PLSF_species_daily_tax_phylo.csv"), row.names = 1, header=T)
#microbe2<-column_to_rownames(microbe, var="ID")
count_phy <- otu_table(microbe, taxa_are_rows=T)
colnames(count_phy) <- sub("^X", "", colnames(count_phy))

meta2<-read.table(paste0(data.path,"62Day_20221109/PLSF_metadata_dailyandplus2019cleaned.csv"), row.names = 1, header=T, sep = ",")
sample_info <- sample_data(meta2)
tax_tab <- read.table(paste0(data.path,"62Day_20221109/taxa_spe_phylo.csv"), header=T,row.names=1, check.names=F)
tax_tab_phy <- tax_table(as.matrix(tax_tab))
Dphyseq_meta <- phyloseq(count_phy, tax_tab_phy, sample_info)
colnames(tax_table(Dphyseq_meta)) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus","Species")

sums_Phy <- data.frame(colSums(otu_table(Dphyseq_meta)))
colnames(sums_Phy) <- "Sample_TotalSeqs"
sums_Phy$sample <- row.names(sums_Phy)
sums_Phy <- arrange(sums_Phy, Sample_TotalSeqs)
####  Create a plot of the number of sequences per sample
library(ggplot2)
ggplot(sums_Phy, aes(x=reorder(sample, Sample_TotalSeqs), y = Sample_TotalSeqs)) + 
  ylab("Number of Sequences per Sample") +
  geom_bar(stat = "identity", colour="black",fill="cornflowerblue")  + xlab("Sample Name") + 
  ggtitle("Total Number of Sequences per Sample") +  scale_y_continuous(breaks =seq(0, 100000000, 1000000))+
  theme(axis.text.x = element_text(colour = "black", size=6, angle=45, hjust = 1, vjust = 1))

(Dphyseq_filt_fin10 <- phyloseq::prune_taxa(phyloseq::taxa_sums(Dphyseq_meta) > 10, Dphyseq_meta))

#rarefaction curve
p <- ggrare(Dphyseq_meta, step = 50000, label = "Sample", se = FALSE)

# https://rdrr.io/rforge/vegan/man/rarefy.html
library(vegan)
data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

microbe$ID=rownames(microbe)
tax_tab$ID=rownames(tax_tab)
tmp=merge(microbe,tax_tab,'ID',all.x=T)
tmp.melt=melt(tmp[,c(colnames(microbe),"species")],id.var=c("species","ID"))
tmp.melt=subset(tmp.melt,is.na(species)==F)
tmp=dcast(tmp.melt,variable~species,value.var = "value",sum)

S=specnumber(tmp[,2:ncol(tmp)])
(raremax <- min(rowSums(tmp[,2:ncol(tmp)])))
Srare <- rarefy(tmp[,2:ncol(tmp)], raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)
rarecurve(BCI, step = 50000, sample = raremax, col = "blue", cex = 0.6)

# https://cran.r-project.org/web/packages/Rarefy/vignettes/Rarefy_basics.html

# Barplot
top <- top_taxa(Dphyseq_filt_fin10, 
                tax_level = "Genus", 
                n_taxa = 10)
plot_nested_bar(top$ps_obj, top_level = "Phylum", nested_level = "Genus")

#Alpha_diversity
set.seed(20200318)
genusDiv <- tax_glom(Dphyseq_filt_fin10, taxrank="Genus")
genusDiv
divnet_genus <- genusDiv %>%
  divnet(ncores=4)

divnet_genus$shannon %>% head
divnet_summaryA <- divnet_genus$shannon %>% summary
names(divnet_summaryA)[names(divnet_summaryA)=="sample_names"] <- "Samples"
meta2$Samples<-rownames(meta2)
meta_ShanonA<-merge(meta2,divnet_summaryA,by="Samples")
