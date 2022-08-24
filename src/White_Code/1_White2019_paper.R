## Work through code from White (2019)
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 02/14/2022

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")

#Paths
wd="C:/Julian_LaCie/_GitHub/ATRAPP"

# -------------------------------------------------------------------------
# From White ER (2019) Minimum Time Required to Detect Population Trends: 
## The Need for Long-Term Monitoring Programs. BioScience 69:40–46. 
## doi: 10.1093/biosci/biy144
## https://github.com/SwampThingPaul/time-series-project

calculate_slope= function(x){
  return(as.numeric(coef(lm(c(x)~c(1:length(x))))[2]))
}
calculate_p_value = function(x){
  return(summary(lm(c(x)~c(1:length(x))))$coefficients[2,4])
}

calculate_power = function(r,phi,sigma,trials,max.time){
  # Parameter values
  trials   = trials
  max.time = max.time
  r        = r
  phi      = phi
  sigma    = sigma
  
  y=replicate(n=trials,simulate_pop(r,phi,sigma,max.time)) # simulate a number of populations with same parameter values
  
  power=sum((apply(y,2,calculate_p_value)<0.05) & sign(apply(y,2,calculate_slope))==sign(r))/trials # evaluates slope coefficients compared to 0.05 (this can be modified)
  # Alternative definition of power which does not require linear regression to have same sign of "true" trend
  # power=sum((apply(y,2,calculate_p_value)<0.05))/trials 
  
  return(power)
}

simulate_pop = function(r,phi,sigma,max.time){
  #set.seed(12345)
  pop = matrix(1000,nrow=1,ncol=max.time)
  for (t in 1:(max.time-1)){
    pop[t + 1] = pop[t] + r + rnorm(n = 1,mean=0,sd = sigma)
  }    
  return(c(pop)) #return vector of population sizes
}

years_to_sample=seq(2,100,by=1)
power=lapply(X=years_to_sample,FUN = calculate_power,phi = 0.6,r=0.2,sigma = .5,trials=100)#should use 300+ trials

simulations = replicate(n=30,simulate_pop(phi = 0.6,r=0.2,sigma = .5,max.time=100))
min_time_needed = years_to_sample[tail(which(power<0.8),1)] + 1
# save(simulations,years_to_sample,power,min_time_needed,
#     file=paste0(wd,'/White_Code/example_simulations_fig1.Rdata'))
load(paste0(wd,'/White_Code/example_simulations_fig1.Rdata'))

## Figure 1
par(mfrow=c(2,2),oma=c(1.5,2,0.5,0.5),mar=c(3,5,2.5,0.5))
matplot(simulations,type='l',col=rgb(0.5,0.5,0.5,0.5),lty=1,ylab='',xlab='',las=1,cex.axis=1.2)
mtext(text = 'time' ,side = 1,cex = 1.2,line=3)
mtext(text = 'population size' ,side = 2,cex = 1.2,line=3.5)
mtext(text = '(a)',side = 3,line = -1.2,adj=0.05,font = 2,cex=1.2)

plot(years_to_sample,power,type='l',lwd=2,las=1,ylab='',xlab='',cex.axis=1.2)
abline(v=min_time_needed,lty=2,lwd=2,col='black')
abline(h=0.8,lty=2,lwd=2,col='black')
mtext(text = 'time sampled' ,side = 1,cex = 1.2,line=3)
mtext(text = 'statistical power' ,side = 2,cex = 1.2,line=3)
mtext(text = '(b)',side = 3,line = -1.2,adj=0.05,font = 2,cex=1.2)

# Plot versus trend strength
  # r_values = seq(1,3,by=0.1) #by=0.2
  # sigma_values=seq(1,7,by=0.3) #by=0.2
  # 
  # parameter_combinations = expand.grid(r_values,5)
  # parameter_combinations$min_time_required = NA
  # names(parameter_combinations)= c('r','sigma','min_time_required')
  # 
  # for (i in 1:nrow(parameter_combinations)){
  #   years_to_sample=seq(2,50,by=1)
  #   y=lapply(X=years_to_sample,FUN = calculate_power,phi =0.5,r=parameter_combinations$r[i],sigma = parameter_combinations$sigma[i],trials=150)
  #   if (length(tail(which(y<0.8),1))>0){
  #   parameter_combinations$min_time_required[i] = years_to_sample[tail(which(y<0.8),1)] + 1
  #   }
  #   #print(i)
  # }
# save(parameter_combinations,
#     file=paste0(wd,'/White_Code/min_time_vs_trend_strength.Rdata'))

load(paste0(wd,'/White_Code/min_time_vs_trend_strength.Rdata'))

  plot(parameter_combinations$r,parameter_combinations$min_time_required,type='l',lwd=2,las=1,ylab='',xlab='',cex.axis=1.2,ylim=c(0,50))
  mtext(text = 'trend strength (slope coefficient)' ,side = 1,cex = 1.2,line=3)
  mtext(text = expression(T[min]) ,side = 2,cex = 1.2,line=3)
  mtext(text = '(c)',side = 3,line = -1.2,adj=0.05,font = 2,cex=1.2)
 
  # # Plot versus sigma values
  # parameter_combinations = expand.grid(1.5,sigma_values)
  # parameter_combinations$min_time_required = NA
  # names(parameter_combinations)= c('r','sigma','min_time_required')
  # 
  # for (i in 1:nrow(parameter_combinations)){
  #   years_to_sample=seq(2,50,by=1)
  #   y=lapply(X=years_to_sample,FUN = calculate_power,phi =0.5,r=parameter_combinations$r[i],sigma = parameter_combinations$sigma[i],trials=150)
  #   if (length(tail(which(y<0.8),1))>0){
  #   parameter_combinations$min_time_required[i] = years_to_sample[tail(which(y<0.8),1)] + 1
  #   }
  #   print(i)
  # }
  # 
  # save(parameter_combinations,
  #      file=paste0(wd,'/White_Code/min_time_vs_pop_var.Rdata'))
  load(paste0(wd,'/White_Code/min_time_vs_pop_var.Rdata'))
  
  plot(parameter_combinations$sigma,parameter_combinations$min_time_required,type='l',lwd=2,las=1,ylab='',xlab='',cex.axis=1.2,ylim=c(0,50))
  mtext(text = 'population variability' ,side = 1,cex = 1.2,line=3)
  mtext(text = expression(T[min]) ,side = 2,cex = 1.2,line=3)
  mtext(text = '(d)',side = 3,line = -1.2,adj=0.05,font = 2,cex=1.2)
  