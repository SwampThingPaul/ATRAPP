# length(unique(phyto_class.dat.melt$MonCY.date))
# x.val=NULL
# for(i in 1:37){
#  tmp=seq(i,37,1)
#  x.val=c(x.val,tmp)
# }
# x.val
# comm.res.plsf$xval=x.val
# 
# plot(interval~xval,comm.res.plsf)
# range(comm.res.plsf$distance)
# bks=seq(0.007,1.5,length.out=10)
# cols.int=findInterval(comm.res.plsf$distance,bks)
# col.vals=viridis::cividis(length(cols.int))
# points(interval~xval,comm.res.plsf,pch=21,bg=col.vals)
# 
# plot(subset(comm.res.plsf,interval==2)$distance,type="l")


## Calculate community stability 
stab <- community_stability(phyto_class.dat.melt,
                            time.var = "MonCY.date2",
                            abundance.var="value")

# Calculate synchrony via loreau, merge with stab
synch_loreau<-merge(synchrony(phyto_class.dat.melt,
                              time.var = "MonCY.date2",
                              species.var="variable",
                              abundance.var="value"), 
                    stab)

# Calculate synchrony via gross, merge with stab
synch_gross<-merge(synchrony(phyto_class.dat.melt,
                             time.var = "MonCY.date",
                             species.var="variable",
                             abundance.var="value",
                             metric="Gross"), 
                   stab)

# Calculate variance ratio, merge with stab
vr <- merge(variance_ratio(knz_001d, time.var = "year",
                           species.var = "species",
                           abundance.var = "abundance",
                           replicate="subplot",
                           bootnumber=1, 
                           average.replicates = F), 
            stab)
