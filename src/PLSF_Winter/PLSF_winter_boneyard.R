
# from rate of change ice cover --------------------------------------------

loess.span=0.8
# png(filename=paste0(plot.path,"PLSF_RateIceOn_v2.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,1.5),oma=c(2.5,2,0.25,0.25));
layout(matrix(c(1:4),2,2,byrow = F),widths=c(1,1))
xlim.val=c(80,180);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.val=c(-0.6,0.6);by.y=0.3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(slope.val~max.days,ann.slope.ice,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
points(slope.val~max.days,subset(ann.slope.ice,variable=="TP.ugL"),
       pch=21,bg="grey",cex=1.25,lwd=0.1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,nsmall=2));box(lwd=1)
mod=loess(slope.val~max.days,subset(ann.slope.ice,variable=="TP.ugL"),span=loess.span)
x.val=seq(min(ann.slope.ice$max.days),max(ann.slope.ice$max.days),length.out=100)
pred.mod=predict(mod,data.frame(max.days=x.val),se=T)
lines(pred.mod$fit~x.val,lwd=1.5,col="grey")
abline(h=0)
mtext(side=2,line=3.25,"TP Slope (\u03BCg L\u207B\u00B9 D\u207B\u00B9)")
legend("topleft",legend=c("TP","LOESS"),
       pch=c(21,NA),pt.bg=c("grey",NA),pt.cex=1,
       lty=c(NA,1),lwd=c(0.01,1.5),col=c("black","grey"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)

ylim.val=c(-0.5,0.3);by.y=0.25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(slope.val~max.days,ann.slope.ice,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
points(slope.val~max.days,subset(ann.slope.ice,variable=="SRP.ugL"),
       pch=21,bg="grey",cex=1.25,lwd=0.1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mod=loess(slope.val~max.days,subset(ann.slope.ice,variable=="SRP.ugL"),span=loess.span)
x.val=seq(min(ann.slope.ice$max.days),max(ann.slope.ice$max.days),length.out=100)
pred.mod=predict(mod,data.frame(max.days=x.val),se=T)
lines(pred.mod$fit~x.val,lwd=1.5,col="grey")
abline(h=0)
mtext(side=2,line=3.25," Slope (\u03BCg L\u207B\u00B9 D\u207B\u00B9)")
legend("topleft",legend=c("SRP","LOESS"),
       pch=c(21,NA),pt.bg=c("grey",NA),pt.cex=1,
       lty=c(NA,1),lwd=c(0.01,1.5),col=c("black","grey"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)
mtext(side=1,"Total Days Since Ice On",outer=T,line=1)

ylim.val=c(-0.01,0);by.y=0.002;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(slope.val~max.days,ann.slope.ice,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
points(slope.val~max.days,subset(ann.slope.ice,variable=="TN.mgL"),
       pch=21,bg="grey",cex=1.25,lwd=0.1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mod=loess(slope.val~max.days,subset(ann.slope.ice,variable=="TN.mgL"),span=loess.span)
x.val=seq(min(ann.slope.ice$max.days),max(ann.slope.ice$max.days),length.out=100)
pred.mod=predict(mod,data.frame(max.days=x.val),se=T)
lines(pred.mod$fit~x.val,lwd=1.5,col="grey")
# lines(x.val,pred.mod$fit-qt(0.975,pred.mod$df)*pred.mod$se.fit,lwd=1.5,lty=2)
# lines(x.val,pred.mod$fit+qt(0.975,pred.mod$df)*pred.mod$se.fit,lwd=1.5,lty=2)
mtext(side=2,line=3.5,"Slope (mg L\u207B\u00B9 D\u207B\u00B9)")
legend("topleft",legend=c("TN","LOESS"),
       pch=c(21,NA),pt.bg=c("grey",NA),pt.cex=1,
       lty=c(NA,1),lwd=c(0.01,1.5),col=c("black","grey"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)

ylim.val=c(-0.0090,0.003);by.y=0.003;ymaj=round(seq(ylim.val[1],ylim.val[2],by.y),3);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(slope.val~max.days,ann.slope.ice,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
points(slope.val~max.days,subset(ann.slope.ice,variable=="NOx.mgL"),
       pch=21,bg="grey",cex=1.25,lwd=0.1)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mod=loess(slope.val~max.days,subset(ann.slope.ice,variable=="NOx.mgL"),span=loess.span)
x.val=seq(min(ann.slope.ice$max.days),max(ann.slope.ice$max.days),length.out=100)
pred.mod=predict(mod,data.frame(max.days=x.val),se=T)
lines(pred.mod$fit~x.val,lwd=1.5,col="grey")
mtext(side=2,line=3.5,"Slope (mg L\u207B\u00B9 D\u207B\u00B9)")
abline(h=0)
legend("topleft",legend=c("DIN","LOESS"),
       pch=c(21,NA),pt.bg=c("grey",NA),pt.cex=1,
       lty=c(NA,1),lwd=c(0.01,1.5),col=c("black","grey"),
       ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)
# points(slope.val~max.days,subset(ann.slope.ice,variable=="NH4.mgL"),
#        pch=21,bg="dodgerblue1",cex=1.25,lwd=0.1)
# mod=loess(slope.val~max.days,subset(ann.slope.ice,variable=="NH4.mgL"),span=loess.span)
# x.val=seq(min(ann.slope.ice$max.days),max(ann.slope.ice$max.days),length.out=100)
# pred.mod=predict(mod,data.frame(max.days=x.val),se=T)
# lines(pred.mod$fit~x.val,lwd=1.5,col="dodgerblue1")

# points(slope.val~max.days,subset(ann.slope.ice,variable=="DIN.mgL"),
#        pch=21,bg="indianred1",cex=1.25,lwd=0.1)
# mod=loess(slope.val~max.days,subset(ann.slope.ice,variable=="DIN.mgL"),span=loess.span)
# x.val=seq(min(ann.slope.ice$max.day),max(ann.slope.ice$max.days),length.out=100)
# pred.mod=predict(mod,data.frame(max.days=x.val),se=T)
# lines(pred.mod$fit~x.val,lwd=1.5,col="indianred1")
# abline(h=0)
# legend("bottomright",legend=c("NO\u2093","NH\u2084","DIN","LOESS"),
#        pch=c(21,21,21,NA),pt.bg=c("grey","dodgerblue1","indianred1",NA),pt.cex=1,
#        lty=c(NA,NA,NA,1),lwd=c(0.01,0.01,0.01,1.5),col=c("black","black","black","grey"),
#        ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,yjust=1)
dev.off()

# # png(filename=paste0(plot.path,"PLSF_IceOn.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
# par(family="serif",mar=c(1,2.5,0.5,1),oma=c(2.5,2,1.5,0.25));
# layout(matrix(1:4,2,2,byrow = F),widths=c(1,1))
# 
# xlim.val=c(0,180);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
# 
# tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TN.mgL"&Ice_cum>0)
# ylim.val=c(0.5,2.5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
# plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
# abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
# points(value~Ice_cum,tmp.dat,
#        pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
# x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
# mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
# lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
# lines(x.val,mod.pred[,2],lty=2,col="black")
# lines(x.val,mod.pred[,3],lty=2,col="black")
# axis_fun(1,xmaj,xmin,NA)
# axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1));box(lwd=1)
# mtext(side=2,line=2,"TN (mg L\u207B\u00B9)")
# cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
# txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
# mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)
# 
# tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="DIN.mgL"&Ice_cum>0)
# ylim.val=c(0.1,1.5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
# plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
# abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
# points(value~Ice_cum,tmp.dat,
#        pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# # mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
# mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
# x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
# mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
# lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
# lines(x.val,mod.pred[,2],lty=2,col="black")
# lines(x.val,mod.pred[,3],lty=2,col="black")
# axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
# axis_fun(2,ymaj,ymin,format(ymaj,nsmall=1));box(lwd=1)
# mtext(side=2,line=2,"DIN (mg L\u207B\u00B9)")
# cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
# txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
# mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)
# 
# tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="TP.ugL"&Ice_cum>0)
# ylim.val=c(20,200);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
# plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
# abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
# points(value~Ice_cum,tmp.dat,
#        pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# # mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
# mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
# x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
# mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
# lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
# lines(x.val,mod.pred[,2],lty=2,col="black")
# lines(x.val,mod.pred[,3],lty=2,col="black")
# axis_fun(1,xmaj,xmin,NA)
# axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0));box(lwd=1)
# mtext(side=3,adj=1,"Lake Outlet ",font=3)
# cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
# txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
# mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)
# 
# tmp.dat=subset(wq.dat.melt2,Site=='Lake_Outlet'&variable=="SRP.ugL"&Ice_cum>0)
# ylim.val=c(1,70);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
# plot(value~Ice_cum,tmp.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,log="y",type="n")
# abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
# points(value~Ice_cum,tmp.dat,
#        pch=21,bg=adjustcolor("dodgerblue1",0.7),col=adjustcolor("grey",0.75),lwd=0.01,cex=1.25)
# # mod=mblm::mblm(value~Ice_cum,subset(tmp.dat,is.na(value)==F),repeated=F)
# mod=lm(value~Ice_cum,subset(tmp.dat,is.na(value)==F))
# x.val=with(tmp.dat,seq(min(Ice_cum),max(Ice_cum),length.out=50))
# mod.pred=predict(mod,data.frame(Ice_cum=x.val),interval="confidence")
# lines(x.val,mod.pred[,1],lty=1,col="black",lwd=2)
# lines(x.val,mod.pred[,2],lty=2,col="black")
# lines(x.val,mod.pred[,3],lty=2,col="black")
# axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
# axis_fun(2,ymaj,ymin,format(ymaj,nsmall=0));box(lwd=1)
# mtext(side=2,line=2,"SRP (\u03BCg L\u207B\u00B9)")
# mod.pred=predict.segmented(seg.mod.SRP.outlet,data.frame(Ice_cum=x.val))
# lines(x.val,mod.pred,col="red",lwd=1)
# mtext(side=1,outer=T,line=1,"Days since Ice On")
# cor.val=with(tmp.dat,cor.test(value,Ice_cum,method="spearman"))
# txt.val=with(cor.val,paste("r =",round(estimate,2),"; \u03C1-value",ifelse(p.value<0.01,"<0.01 ",ifelse(p.value<0.05,"<0.05 ",paste0("=",round(p.value,2)," ")))))
# mtext(side=3,adj=1,cex=0.75,line=-1.25,txt.val)
# 
# dev.off()