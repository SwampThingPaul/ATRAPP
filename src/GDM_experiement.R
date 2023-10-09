library(gdm)
##fit table environmental data
# format site-pair table using the southwest data table
head(southwest)
sppData <- southwest[c(1,2,13,14)]
envTab <- southwest[c(2:ncol(southwest))]

head(sppData)
head(envTab)

sitePairTab <- formatsitepair(sppData, 2, XColumn="Long", YColumn="Lat", sppColumn="species",
                              siteColumn="site", predData=envTab)


##fit table GDM
gdmTabMod <- gdm(sitePairTab, geo=TRUE)
summary(gdmTabMod)

##fit raster environmental data
##sets up site-pair table
rastFile <- system.file("./extdata/swBioclims.grd", package="gdm")
envRast <- raster::stack(rastFile)

##environmental raster data
sitePairRast <- formatsitepair(sppData, 2, XColumn="Long",
                               YColumn="Lat", sppColumn="species",
                               siteColumn="site", predData=envRast)
##sometimes raster data returns NA in the site-pair table, these rows will
##have to be removed before fitting gdm
sitePairRast <- na.omit(sitePairRast)

##fit raster GDM
gdmRastMod <- gdm(sitePairRast, geo=TRUE)
summary(gdmRastMod)
