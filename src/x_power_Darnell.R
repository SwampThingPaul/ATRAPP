## Title:      Statistical power of detecting trends following 
##             Darnell et al http://dx.doi.org/10.1016/j.marpolbul.2012.04.002
## Created by: Paul Julian (pjulian@sccf.org/pauljulianphd@gmail.com)
## Created on: 05/31/2022

## From Darnell
## info from Table 1

library(mblm)

set.seed(123)

rslt=data.frame()
trend.rslt=data.frame()
for(j in 1:100){
# Burdekin
sig.alpha=0.8;
sig=0.8;

Nyears=10; #as example
delta=0.005;
alpha.i=rnorm(1,0,sig.alpha^2);
beta=rnorm(1,0.45,0.02);


syear=1987
yr.vals=syear:(syear+Nyears)
for(i in 1:Nyears){
  N=rnbinom(1,mu=30,size=1)
  Q=rnorm(1,5,2)
  e.val=rnorm(1,0,sig^2)
  
  null.mod=alpha.i+beta*Q+e.val
  alt.mod=null.mod+delta*yr.vals[i]
  tmp.rslt=data.frame(iter=j,YR=yr.vals[i],
                      N=N,Q=Q,e.val=e.val,null.mod=null.mod,alt.mod=alt.mod)
  rslt=rbind(rslt,tmp.rslt)
}

# find trend
# using linear model
# trend.null=lm(null.mod~YR,subset(rslt,iter==j))
# trend.alt=lm(alt.mod~YR,subset(rslt,iter==j))

# using median based linear model ... like thiel-sen to estimate trend 
# (or use kendall correlation?)
trend.null=mblm(null.mod~YR,subset(rslt,iter==j),repeated=F)
trend.alt=mblm(alt.mod~YR,subset(rslt,iter==j),repeated=F)

tmp.trend.rslt=data.frame(iter=j,
                          trend.null.slope=as.numeric(coefficients(trend.null)[1]),
                          trend.alt.slope=as.numeric(coefficients(trend.alt)[1]))
trend.rslt=rbind(tmp.trend.rslt,trend.rslt)

}


# Calculate 95th percentile
null.95=quantile(trend.rslt$trend.null.slope,0.95)
alt.95=quantile(trend.rslt$trend.alt.slope,0.95)

# Calculate power as the percentage
with(trend.rslt,sum(trend.alt.slope>null.95)/length(trend.alt.slope)*100)
