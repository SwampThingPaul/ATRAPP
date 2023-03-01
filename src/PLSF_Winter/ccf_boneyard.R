## attempt to breakdown the ccf function 

## got the lag plotting figured out but not the statistical test


tmp.dat=subset(month.mean.WQ.tele,variable=="DIN.mgL"&Site==sites.vals[2])
astsa::lag2.plot(tmp.dat$mean.enso,tmp.dat$mean.val,max.lag = 11)

test.acf.out=acf(tmp.dat[,c("mean.enso","mean.val")],demean=T)
lag <- c(rev(test.acf.out$lag[-1, 2, 1]), test.acf.out$lag[, 1, 2])
y <- c(rev(test.acf.out$acf[-1, 2, 1]), test.acf.out$acf[, 1, 2])
data.frame(acf=array(y, dim = c(length(y), 1L, 1L)),lag=array(lag, dim = c(length(y), 1L, 1L)))

h=-11
length(lag(tmp.dat$mean.enso,h))

lagged=lag(zoo::as.zoo(tmp.dat$mean.enso),h,na.pad=T)
tmp.val=zoo::as.zoo(tmp.dat$mean.val)

x.dat1=tmp.dat[,c("mean.enso","mean.val")]
x.dat.demean=sweep(x.dat1, 2, colMeans(x.dat1, na.rm = TRUE), check.margin = FALSE)

lagged=lag(zoo::as.zoo(x.dat.demean$mean.enso),h,na.pad=T)
tmp.val=zoo::as.zoo(x.dat.demean$mean.val)


plot(lagged,tmp.val)
cor.test(x.dat.demean$lagged,x.dat.demean$tmp.val)
cor.test(tmp.val,lagged)









library(testcorr)
test=with(subset(month.mean.WQ.tele,variable=="DIN.mgL"&Site==sites.vals[2]),cc.test(mean.amo,mean.val,max.lag = 24))
test$lag
test$cc
test$t #standard t-value
test$pvt 
test$ttilde #robust t-value
test$pvttilde

test$lag[test$pvt<0.05]
test$lag[test$pvttilde<0.05]

plot(test$lag,test$pvttilde)

plot(lag(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2])$mean.val,1)~subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2])$mean.amo)
cor.test(lag(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2])$mean.val,1),subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2])$mean.amo)

test=with(subset(tmp2,variable=="DIN.mgL"&Site==sites.vals[2]&winter==1),cc.test(mean.oni,mean.val,max.lag = 10))
test=with(subset(tmp2,variable=="TN.mgL"&Site==sites.vals[2]&winter==1),cc.test(mean.oni,mean.val,max.lag = 10))
test=with(subset(tmp2,variable=="TN.mgL"&Site==sites.vals[2]&winter==1),cc.test(mean.pdo,mean.val,max.lag = 10))


layout(matrix(1:24,4,6))
for(i in 1:length(clim.ind)){
  for(j in 1:length(var.vals1)){
    ccf(subset(tmp2,variable==var.vals1[j]&Site==sites.vals[1]&winter==1)[,clim.ind[i]],
        subset(tmp2,variable==var.vals1[j]&Site==sites.vals[1]&winter==1)$mean.val)
  }
}


var.vals1=c("TN.mgL","DIN.mgL","TP.ugL","SRP.ugL")
clim.ind=paste("mean",c("amo","nao","pdo","soi","oni","enso"),sep=".")
ccf.rslt=data.frame()
for(i in 1:length(sites.vals)){
  tmp.dat=subset(month.mean.WQ.tele,Site==sites.vals[i])
  for(j in 1:length(var.vals1)){
    for(k in 1:length(clim.ind)){
      test=cc.test(tmp.dat[tmp.dat$variable==var.vals1[j],"mean.val"],
                   tmp.dat[tmp.dat$variable==var.vals1[j],clim.ind[k]],max.lag = 24,
                   plot=F,table=F)
      rslt=with(test,data.frame(lag=lag,cc=cc,t=t,pvt=pvt,ttilde=pvttilde,
                                Site=sites.vals[i],
                                variable=var.vals1[j],
                                climate.index=clim.ind[k],
                                ccf.CI=qnorm((1+0.95)/2)/sqrt(nrow(tmp.dat))))
      ccf.rslt=rbind(rslt,ccf.rslt)
      
    }
  }
}
ccf.rslt