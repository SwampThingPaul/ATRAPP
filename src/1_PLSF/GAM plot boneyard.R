## GAM plot bone yard



# Phosphorus --------------------------------------------------------------

## Version 1 --------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P.png"),width=12,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:40,5,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

# TP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.in$model$DOY,partial.resids.TP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,TP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
  # lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  # lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.in$model$CY,partial.resids.TP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,TP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
  # lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
  # lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TP.in.pdat$CY)),nrow=length(unique(TP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TP.in.pdat$CY),y=unique(TP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TP.in.pdat$CY),y=unique(TP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.out$model$DOY,partial.resids.TP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,TP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.TP.out$model$CY,partial.resids.TP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,TP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TP.out.pdat$CY)),nrow=length(unique(TP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(TP.out.pdat$CY),y=unique(TP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(TP.out.pdat$CY),y=unique(TP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# PP
{
  ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.in$model$DOY,partial.resids.PP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,PP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.in$model$CY,partial.resids.PP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,PP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(PP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(PP.in.pdat$CY)),nrow=length(unique(PP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(PP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(PP.in.pdat$CY),y=unique(PP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(PP.in.pdat$CY),y=unique(PP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-4,4);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,PP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.out$model$DOY,partial.resids.PP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,PP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = PP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = PP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,PP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.PP.out$model$CY,partial.resids.PP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,PP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = PP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = PP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = PP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = PP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(PP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(PP.out.pdat$CY)),nrow=length(unique(PP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(PP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(PP.out.pdat$CY),y=unique(PP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(PP.out.pdat$CY),y=unique(PP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# DP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.in$model$DOY,partial.resids.DP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
  
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.in$model$CY,partial.resids.DP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DP.in.pdat$CY)),nrow=length(unique(DP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DP.in.pdat$CY),y=unique(DP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DP.in.pdat$CY),y=unique(DP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.out$model$DOY,partial.resids.DP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DP.out$model$CY,partial.resids.DP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DP.out.pdat$CY)),nrow=length(unique(DP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DP.out.pdat$CY),y=unique(DP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DP.out.pdat$CY),y=unique(DP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
}

# SRP
{
  ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.in$model$DOY,partial.resids.SRP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,SRP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
  
  ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.in$model$CY,partial.resids.SRP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,SRP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(SRP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP.in.pdat$CY)),nrow=length(unique(SRP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(SRP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(SRP.in.pdat$CY),y=unique(SRP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(SRP.in.pdat$CY),y=unique(SRP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.out$model$DOY,partial.resids.SRP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  # mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.SRP.out$model$CY,partial.resids.SRP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,SRP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = SRP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = SRP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(SRP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP.out.pdat$CY)),nrow=length(unique(SRP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(SRP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(SRP.out.pdat$CY),y=unique(SRP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(SRP.out.pdat$CY),y=unique(SRP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  # mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

# DOP
{
  ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DOP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.in$model$DOY,partial.resids.DOP.in[,1],pch=19,col=inflow.col)
  lines(fit~DOY,DOP.in.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DOP.in.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DOP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DOP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.in$model$CY,partial.resids.DOP.in[,2],pch=19,col=inflow.col)
  lines(fit.CY~CY,DOP.in.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DOP.in.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DOP.in.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DOP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DOP.in.pdat$CY)),nrow=length(unique(DOP.in.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DOP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DOP.in.pdat$CY),y=unique(DOP.in.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DOP.in.pdat$CY),y=unique(DOP.in.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  
  ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
  plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
  lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
  lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
  lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
  lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
  lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
  mtext(side=1,line=2,"DOY",cex=labs.cex)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
  plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
  lines(fit.CY~CY,DOP.out.pdat,lwd=2)
  lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
  lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
  lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
  lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  # mtext(side=3,line=1,"Outflow",cex=1,font=2)
  mtext(side=2,line=1.5,"Effect",cex=labs.cex)
  
  ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
  xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
  tmp.ma1=with(DOP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DOP.out.pdat$CY)),nrow=length(unique(DOP.out.pdat$DOY))))
  brk=10
  breaks.val=classInt::classIntervals(DOP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
  pal=hcl.colors(n=brk,alpha=0.75)
  image(x=unique(DOP.out.pdat$CY),y=unique(DOP.out.pdat$DOY),z=t(tmp.ma1),
        breaks=breaks.val,col=pal,
        ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
  contour(x=unique(DOP.out.pdat$CY),y=unique(DOP.out.pdat$DOY),z=t(tmp.ma1),
          levels=breaks.val,nlevels=brk,
          add=T,drawlabels=F,lwd=0.75,col="white")
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
  axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
  mtext(side=2,line=1.75,"DOY",cex=labs.cex)
  mtext(side=1,line=2,"Year",cex=labs.cex)
  
  legend_image=as.raster(matrix(rev(pal),ncol=1))
  plot(0:1,0:1,ann=F,axes=F,type="n")
  top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
  rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
  text(x=x.max, y = c(bot.val,top.val), 
       labels =format(round(range(breaks.val),2)),
       cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
  text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
}

dev.off()


## Version 2 --------------------------------------------------------------

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P_v2.png"),width=10,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:22),6,4,byrow = T),heights=c(0.2,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

{
  # TP
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
    par(mar=c(2,1.75,1,1))
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.in$model$DOY,partial.resids.TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.in$model$CY,partial.resids.TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.out$model$DOY,partial.resids.TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.out$model$CY,partial.resids.TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
  }
  
  # PP
  {
    ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.in$model$DOY,partial.resids.PP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,PP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.in$model$CY,partial.resids.PP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,PP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-4,4);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.out$model$DOY,partial.resids.PP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,PP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.out$model$CY,partial.resids.PP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,PP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  # DP
  {
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.in$model$DOY,partial.resids.DP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.in$model$CY,partial.resids.DP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.out$model$DOY,partial.resids.DP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.out$model$CY,partial.resids.DP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  # SRP
  {
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.in$model$DOY,partial.resids.SRP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,SRP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
    
    ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.in$model$CY,partial.resids.SRP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,SRP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.out$model$DOY,partial.resids.SRP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.out$model$CY,partial.resids.SRP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,SRP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  # DOP
  {
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DOP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.in$model$DOY,partial.resids.DOP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DOP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DOP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.in$model$CY,partial.resids.DOP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DOP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DOP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
  }
}
dev.off()



## Version 3 --------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P_v3.png"),width=10,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(2,2.5,0.5,0.25));
layout(matrix(c(1,1,2,2,3:22),6,4,byrow = T),heights=c(0.2,1,1,1,1,1))

yaxs.cex=0.9
labs.cex=1
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]
xlab.line=1.5

xlim.val1=c(0,366);by.x1=90;xmaj1=seq(xlim.val1[1],xlim.val1[2],by.x1);xmin1=seq(xlim.val1[1],xlim.val1[2],by.x1/3)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
{
  # TP
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"s(DOY)",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"s(Year)",cex=1.5,font=2)
    par(mar=c(1,1.75,1,1))
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TP.in$model$DOY,partial.resids.TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Inlet",cex=labs.cex)
    mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TP.out$model$DOY,partial.resids.TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Outlet",cex=labs.cex)
    
    ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.in$model$CY,partial.resids.TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Inlet",cex=labs.cex)
    
    plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.out$model$CY,partial.resids.TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Outlet",cex=labs.cex)
    
  }
  # PP
  {
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,PP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.PP.in$model$DOY,partial.resids.PP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,PP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,PP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.PP.out$model$DOY,partial.resids.PP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,PP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,PP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.in$model$CY,partial.resids.PP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,PP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,PP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.out$model$CY,partial.resids.PP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,PP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
  }
  # DP
  {
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,DP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DP.in$model$DOY,partial.resids.DP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,DP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DP.out$model$DOY,partial.resids.DP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,DP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.in$model$CY,partial.resids.DP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,DP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.out$model$CY,partial.resids.DP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
  }
  # SRP
  {
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.in$model$DOY,partial.resids.SRP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,SRP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.out$model$DOY,partial.resids.SRP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.in$model$CY,partial.resids.SRP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,SRP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.out$model$CY,partial.resids.SRP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,SRP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
  }
  # DOP
  {
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,DOP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.in$model$DOY,partial.resids.DOP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DOP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
    mtext(side=1,line=xlab.line,"DOY")
    
    plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"DOY")
    
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,DOP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.in$model$CY,partial.resids.DOP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DOP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"Year")
    
    plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DOP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"Year")
  }
}
dev.off()



## Version 4 - Inlet/Outlet -----------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P_inlet.png"),width=6.5,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:10,5,2,byrow = T))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

{
  # TP
  {
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.in$model$DOY,partial.resids.TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DOP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DOP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    ylim.val=c(-2,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.in$model$CY,partial.resids.TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }    
  # PP
  {
    ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.in$model$DOY,partial.resids.PP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,PP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,4);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.in$model$CY,partial.resids.PP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,PP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  # DP
  {
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.in$model$DOY,partial.resids.DP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.in$model$CY,partial.resids.DP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  # SRP
  {
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.in$model$DOY,partial.resids.SRP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,SRP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
    
    ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.in$model$CY,partial.resids.SRP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,SRP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
  }
  # DOP
  {
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DOP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.in$model$DOY,partial.resids.DOP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DOP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DOP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.in$model$CY,partial.resids.DOP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DOP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"Year",cex=labs.cex)
  }
  mtext(side=3,outer=T,"Lake Inlet",font=2)
}
dev.off()

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_P_outlet.png"),width=6.5,height=8,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:10,5,2,byrow = T))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2

{
  # TP
  {
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.out$model$DOY,partial.resids.TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TP.out$model$CY,partial.resids.TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
  }    
  # PP
  {
    ylim.val=c(-4,4);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.out$model$DOY,partial.resids.PP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,PP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"PP\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP.out$model$CY,partial.resids.PP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,PP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  # DP
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.out$model$DOY,partial.resids.DP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"DP\nEffect",cex=labs.cex)
    
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP.out$model$CY,partial.resids.DP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  # SRP
  {
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.out$model$DOY,partial.resids.SRP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,SRP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"SRP\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP.out$model$CY,partial.resids.SRP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,SRP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  # DOP
  {
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DOP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$DOY,partial.resids.DOP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DOP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DOP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DOP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DOP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"DOP\nEffect",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DOP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DOP.out$model$CY,partial.resids.DOP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DOP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DOP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DOP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DOP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"Year",cex=labs.cex)
  }
  mtext(side=3,outer=T,"Lake Outlet",font=2)
}
dev.off()



# Nitrogen ----------------------------------------------------------------

## Version 1 --------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_N.png"),width=12,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:48,6,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
  TN.in.pdat=subset(TN.in.pdat,CY>=2011)
  TN.out.pdat=subset(TN.out.pdat,CY>=2011)
  # TN
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.in$model$DOY,partial.resids.TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.in$model$CY,partial.resids.TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TN.in.pdat$CY)),nrow=length(unique(TN.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TN.in.pdat$CY),y=unique(TN.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TN.in.pdat$CY),y=unique(TN.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.out$model$DOY,partial.resids.TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.out$model$CY,partial.resids.TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TN.out.pdat$CY)),nrow=length(unique(TN.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TN.out.pdat$CY),y=unique(TN.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TN.out.pdat$CY),y=unique(TN.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  TON.in.pdat=subset(TON.in.pdat,CY>=2013)
  TON.out.pdat=subset(TON.out.pdat,CY>=2013)
  # TON
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TON.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.in$model$DOY,partial.resids.TON.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TON.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TON.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TON.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"TON\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TON.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.in$model$CY,partial.resids.TON.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TON.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TON.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TON.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TON.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TON.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TON.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TON.in.pdat$CY)),nrow=length(unique(TON.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TON.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TON.in.pdat$CY),y=unique(TON.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TON.in.pdat$CY),y=unique(TON.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-2,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TON.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.out$model$DOY,partial.resids.TON.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TON.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TON.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TON.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TON.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.out$model$CY,partial.resids.TON.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TON.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TON.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TON.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TON.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TON.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TON.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TON.out.pdat$CY)),nrow=length(unique(TON.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TON.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TON.out.pdat$CY),y=unique(TON.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TON.out.pdat$CY),y=unique(TON.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  TKN.in.pdat=subset(TKN.in.pdat,CY>=2011)
  TKN.out.pdat=subset(TKN.out.pdat,CY>=2011)
  # TKN
  {
    ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TKN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.in$model$DOY,partial.resids.TKN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TKN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TKN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TKN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"TKN\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TKN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.in$model$CY,partial.resids.TKN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TKN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TKN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TKN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TKN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TKN.in.pdat$CY)),nrow=length(unique(TKN.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TKN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TKN.in.pdat$CY),y=unique(TKN.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TKN.in.pdat$CY),y=unique(TKN.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TKN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.out$model$DOY,partial.resids.TKN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TKN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TKN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TKN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TKN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.out$model$CY,partial.resids.TKN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TKN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TKN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TKN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TKN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TKN.out.pdat$CY)),nrow=length(unique(TKN.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TKN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TKN.out.pdat$CY),y=unique(TKN.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TKN.out.pdat$CY),y=unique(TKN.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
  }
  
  NOx.in.pdat=subset(NOx.in.pdat,CY>=2011)
  NOx.out.pdat=subset(NOx.out.pdat,CY>=2011)
  # NOx
  {
    ylim.val=c(-4,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.in$model$DOY,partial.resids.NOx.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NOx.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"NOx\nEffect",cex=labs.cex)
    
    ylim.val=c(-5,2.5);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.in$model$CY,partial.resids.NOx.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NOx.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NOx.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx.in.pdat$CY)),nrow=length(unique(NOx.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NOx.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NOx.in.pdat$CY),y=unique(NOx.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NOx.in.pdat$CY),y=unique(NOx.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    
    ylim.val=c(-6,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.out$model$DOY,partial.resids.NOx.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NOx.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.out$model$CY,partial.resids.NOx.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NOx.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NOx.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx.out.pdat$CY)),nrow=length(unique(NOx.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NOx.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NOx.out.pdat$CY),y=unique(NOx.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NOx.out.pdat$CY),y=unique(NOx.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
  }
  
  NH4.in.pdat=subset(NH4.in.pdat,CY>=2013)
  NH4.out.pdat=subset(NH4.out.pdat,CY>=2013)
  # NH4
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.in$model$DOY,partial.resids.NH4.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NH4.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"NH\u2084\u207A\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.in$model$CY,partial.resids.NH4.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NH4.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NH4.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4.in.pdat$CY)),nrow=length(unique(NH4.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NH4.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NH4.in.pdat$CY),y=unique(NH4.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NH4.in.pdat$CY),y=unique(NH4.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    
    ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.out$model$DOY,partial.resids.NH4.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NH4.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-5,3);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.out$model$CY,partial.resids.NH4.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NH4.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NH4.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4.out.pdat$CY)),nrow=length(unique(NH4.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NH4.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NH4.out.pdat$CY),y=unique(NH4.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NH4.out.pdat$CY),y=unique(NH4.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  DIN.in.pdat=subset(DIN.in.pdat,CY>=2013)
  DIN.out.pdat=subset(DIN.out.pdat,CY>=2013)
  # DIN
  {
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.in$model$DOY,partial.resids.DIN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DIN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"DIN\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.in$model$CY,partial.resids.DIN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DIN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(DIN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN.in.pdat$CY)),nrow=length(unique(DIN.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(DIN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(DIN.in.pdat$CY),y=unique(DIN.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(DIN.in.pdat$CY),y=unique(DIN.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.out$model$DOY,partial.resids.DIN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DIN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.out$model$CY,partial.resids.DIN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DIN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(DIN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN.out.pdat$CY)),nrow=length(unique(DIN.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(DIN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(DIN.out.pdat$CY),y=unique(DIN.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(DIN.out.pdat$CY),y=unique(DIN.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
}
dev.off()


## Version 2 ---------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_N_v2.png"),width=12,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:26),7,4,byrow = T),heights=c(0.2,1,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
  TN.in.pdat=subset(TN.in.pdat,CY>=2011)
  TN.out.pdat=subset(TN.out.pdat,CY>=2011)
  # TN
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
    par(mar=c(2,1.75,1,1))
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.in$model$DOY,partial.resids.TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.in$model$CY,partial.resids.TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.out$model$DOY,partial.resids.TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.out$model$CY,partial.resids.TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  TON.in.pdat=subset(TON.in.pdat,CY>=2013)
  TON.out.pdat=subset(TON.out.pdat,CY>=2013)
  # TON
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TON.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.in$model$DOY,partial.resids.TON.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TON.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TON.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TON.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TON\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TON.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.in$model$CY,partial.resids.TON.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TON.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TON.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TON.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TON.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TON.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TON.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.out$model$DOY,partial.resids.TON.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TON.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TON.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TON.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TON.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.out$model$CY,partial.resids.TON.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TON.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TON.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TON.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TON.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TON.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  TKN.in.pdat=subset(TKN.in.pdat,CY>=2011)
  TKN.out.pdat=subset(TKN.out.pdat,CY>=2011)
  # TKN
  {
    ylim.val=c(-2,1.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TKN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.in$model$DOY,partial.resids.TKN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TKN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TKN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TKN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TKN\nEffect",cex=labs.cex)
    
    ylim.val=c(-2,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TKN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.in$model$CY,partial.resids.TKN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TKN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TKN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TKN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TKN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.out$model$DOY,partial.resids.TKN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TKN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TKN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TKN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TKN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.out$model$CY,partial.resids.TKN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TKN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TKN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TKN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  NOx.in.pdat=subset(NOx.in.pdat,CY>=2011)
  NOx.out.pdat=subset(NOx.out.pdat,CY>=2011)
  # NOx
  {
    ylim.val=c(-4,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.in$model$DOY,partial.resids.NOx.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NOx.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"NOx\nEffect",cex=labs.cex)
    
    ylim.val=c(-5,2.5);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.in$model$CY,partial.resids.NOx.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NOx.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-6,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.out$model$DOY,partial.resids.NOx.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NOx.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-4,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.out$model$CY,partial.resids.NOx.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NOx.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
  }
  
  NH4.in.pdat=subset(NH4.in.pdat,CY>=2013)
  NH4.out.pdat=subset(NH4.out.pdat,CY>=2013)
  # NH4
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.in$model$DOY,partial.resids.NH4.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NH4.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"NH\u2084\u207A\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.in$model$CY,partial.resids.NH4.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NH4.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.out$model$DOY,partial.resids.NH4.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NH4.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-5,3);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.out$model$CY,partial.resids.NH4.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NH4.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
  }
  
  DIN.in.pdat=subset(DIN.in.pdat,CY>=2013)
  DIN.out.pdat=subset(DIN.out.pdat,CY>=2013)
  # DIN
  {
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.in$model$DOY,partial.resids.DIN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DIN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"DIN\nEffect",cex=labs.cex)
    
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.in$model$CY,partial.resids.DIN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DIN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.out$model$DOY,partial.resids.DIN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DIN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.out$model$CY,partial.resids.DIN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DIN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
  }
}
dev.off()


## Version 3 ---------------------------------------------------------------

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_N_v3.png"),width=12,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1.5,2.5,0.5,0.25));
layout(matrix(c(1,1,2,2,3:26),7,4,byrow = T),heights=c(0.2,1,1,1,1,1))

yaxs.cex=0.9
labs.cex=1
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]
xlab.line=1.5

xlim.val1=c(0,366);by.x1=90;xmaj1=seq(xlim.val1[1],xlim.val1[2],by.x1);xmin1=seq(xlim.val1[1],xlim.val1[2],by.x1/3)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

{
  TN.in.pdat=subset(TN.in.pdat,CY>=2011)
  TN.out.pdat=subset(TN.out.pdat,CY>=2011)
  # TN
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"s(DOY)",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"c(Year)",cex=1.5,font=2)
    par(mar=c(1,1.75,0.5,1))
    
    ylim.val=c(-2,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TN.in$model$DOY,partial.resids.TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Inlet",cex=labs.cex)
    mtext(side=2,line=1.5,"TN\nEffect",cex=labs.cex)
    
    plot(fit~DOY,TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TN.out$model$DOY,partial.resids.TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Outlet",cex=labs.cex)
    
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.in$model$CY,partial.resids.TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Inlet",cex=labs.cex)
    
    plot(fit.CY~CY,TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN.out$model$CY,partial.resids.TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Outlet",cex=labs.cex)
  }
  
  TON.in.pdat=subset(TON.in.pdat,CY>=2013)
  TON.out.pdat=subset(TON.out.pdat,CY>=2013)
  # TON
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,TON.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TON.in$model$DOY,partial.resids.TON.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TON.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TON.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TON.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TON.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"TON\nEffect",cex=labs.cex)
    
    plot(fit~DOY,TON.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TON.out$model$DOY,partial.resids.TON.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TON.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TON.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TON.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TON.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,TON.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.in$model$CY,partial.resids.TON.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TON.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TON.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TON.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TON.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TON.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,TON.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TON.out$model$CY,partial.resids.TON.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TON.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TON.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TON.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TON.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TON.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  TKN.in.pdat=subset(TKN.in.pdat,CY>=2011)
  TKN.out.pdat=subset(TKN.out.pdat,CY>=2011)
  # TKN
  {
    ylim.val=c(-2,2);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,TKN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.in$model$DOY,partial.resids.TKN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TKN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TKN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TKN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TKN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"TKN\nEffect",cex=labs.cex)
    
    plot(fit~DOY,TKN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.out$model$DOY,partial.resids.TKN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TKN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TKN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TKN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TKN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-2,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,TKN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.in$model$CY,partial.resids.TKN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TKN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TKN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TKN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TKN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,TKN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TKN.out$model$CY,partial.resids.TKN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TKN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TKN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TKN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TKN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  NOx.in.pdat=subset(NOx.in.pdat,CY>=2011)
  NOx.out.pdat=subset(NOx.out.pdat,CY>=2011)
  # NOx
  {
    ylim.val=c(-6,3);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,NOx.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.in$model$DOY,partial.resids.NOx.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NOx.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"NOx\nEffect",cex=labs.cex)
    
    plot(fit~DOY,NOx.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.out$model$DOY,partial.resids.NOx.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NOx.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-5,5);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,NOx.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.in$model$CY,partial.resids.NOx.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NOx.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,NOx.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx.out$model$CY,partial.resids.NOx.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NOx.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
  }
  
  NH4.in.pdat=subset(NH4.in.pdat,CY>=2013)
  NH4.out.pdat=subset(NH4.out.pdat,CY>=2013)
  # NH4
  {
    ylim.val=c(-4,3);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,NH4.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.in$model$DOY,partial.resids.NH4.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NH4.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"NH\u2084\u207A\nEffect",cex=labs.cex)
    
    plot(fit~DOY,NH4.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.out$model$DOY,partial.resids.NH4.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NH4.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-5,3);by.y=2.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,NH4.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.in$model$CY,partial.resids.NH4.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NH4.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,NH4.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4.out$model$CY,partial.resids.NH4.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NH4.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
  }
  
  DIN.in.pdat=subset(DIN.in.pdat,CY>=2013)
  DIN.out.pdat=subset(DIN.out.pdat,CY>=2013)
  # DIN
  {
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,DIN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.in$model$DOY,partial.resids.DIN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DIN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"DIN\nEffect",cex=labs.cex)
    
    plot(fit~DOY,DIN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.out$model$DOY,partial.resids.DIN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DIN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"DOY",cex=labs.cex)
    
    ylim.val=c(-3,2);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,DIN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.in$model$CY,partial.resids.DIN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DIN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"Year",cex=labs.cex)
    
    plot(fit.CY~CY,DIN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN.out$model$CY,partial.resids.DIN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DIN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=xlab.line,"Year",cex=labs.cex)
  }
}
dev.off()


# Ratios ------------------------------------------------------------------

## Version 1 ---------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_ratios.png"),width=12,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1.5),oma=c(1,2,1.5,0.25));
layout(matrix(1:56,7,8,byrow = T),widths=c(1,1,1,0.3,1,1,1,0.3))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
  TN_TP.in.pdat=subset(TN_TP.in.pdat,CY>=2011)
  TN_TP.out.pdat=subset(TN_TP.out.pdat,CY>=2011)
  # TN_TP
  {
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.in$model$DOY,partial.resids.TN_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TN_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"TN:TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.in$model$CY,partial.resids.TN_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TN_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=3,line=1,"Lake Inlet",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TN_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(TN_TP.in.pdat$CY)),nrow=length(unique(TN_TP.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TN_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TN_TP.in.pdat$CY),y=unique(TN_TP.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TN_TP.in.pdat$CY),y=unique(TN_TP.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.out$model$DOY,partial.resids.TN_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TN_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.out$model$CY,partial.resids.TN_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TN_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=3,line=1,"Lake Outlet",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(TN_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(TN_TP.out.pdat$CY)),nrow=length(unique(TN_TP.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(TN_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(TN_TP.out.pdat$CY),y=unique(TN_TP.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(TN_TP.out.pdat$CY),y=unique(TN_TP.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  DIN_SRP.in.pdat=subset(DIN_SRP.in.pdat,CY>=2013)
  DIN_SRP.out.pdat=subset(DIN_SRP.out.pdat,CY>=2013)
  # DIN_SRP
  {
    ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN_SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.in$model$DOY,partial.resids.DIN_SRP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DIN_SRP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN_SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN_SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"DIN:SRP\nEffect",cex=labs.cex)
    
    ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN_SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.in$model$CY,partial.resids.DIN_SRP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DIN_SRP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN_SRP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN_SRP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Inflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(DIN_SRP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN_SRP.in.pdat$CY)),nrow=length(unique(DIN_SRP.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(DIN_SRP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(DIN_SRP.in.pdat$CY),y=unique(DIN_SRP.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(DIN_SRP.in.pdat$CY),y=unique(DIN_SRP.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN_SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.out$model$DOY,partial.resids.DIN_SRP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DIN_SRP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN_SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN_SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN_SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.out$model$CY,partial.resids.DIN_SRP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DIN_SRP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN_SRP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN_SRP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(DIN_SRP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DIN_SRP.out.pdat$CY)),nrow=length(unique(DIN_SRP.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(DIN_SRP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(DIN_SRP.out.pdat$CY),y=unique(DIN_SRP.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(DIN_SRP.out.pdat$CY),y=unique(DIN_SRP.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  # PP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.in$model$DOY,partial.resids.PP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,PP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%PP of TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.in$model$CY,partial.resids.PP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,PP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=3,line=1,"Inflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(PP_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(PP_TP.in.pdat$CY)),nrow=length(unique(PP_TP.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(PP_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(PP_TP.in.pdat$CY),y=unique(PP_TP.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(PP_TP.in.pdat$CY),y=unique(PP_TP.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.out$model$DOY,partial.resids.PP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,PP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.out$model$CY,partial.resids.PP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,PP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(PP_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(PP_TP.out.pdat$CY)),nrow=length(unique(PP_TP.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(PP_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(PP_TP.out.pdat$CY),y=unique(PP_TP.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(PP_TP.out.pdat$CY),y=unique(PP_TP.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  # SRP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.in$model$DOY,partial.resids.SRP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,SRP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%SRP of TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.in$model$CY,partial.resids.SRP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,SRP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Inflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(SRP_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP_TP.in.pdat$CY)),nrow=length(unique(SRP_TP.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(SRP_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(SRP_TP.in.pdat$CY),y=unique(SRP_TP.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(SRP_TP.in.pdat$CY),y=unique(SRP_TP.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.out$model$DOY,partial.resids.SRP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,SRP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.out$model$CY,partial.resids.SRP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,SRP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(SRP_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(SRP_TP.out.pdat$CY)),nrow=length(unique(SRP_TP.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(SRP_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(SRP_TP.out.pdat$CY),y=unique(SRP_TP.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(SRP_TP.out.pdat$CY),y=unique(SRP_TP.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  # DP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.in$model$DOY,partial.resids.DP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%DP of TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.in$model$CY,partial.resids.DP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Inflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(DP_TP.in.pdat,matrix(fit.DOYCY,ncol=length(unique(DP_TP.in.pdat$CY)),nrow=length(unique(DP_TP.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(DP_TP.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(DP_TP.in.pdat$CY),y=unique(DP_TP.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(DP_TP.in.pdat$CY),y=unique(DP_TP.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.out$model$DOY,partial.resids.DP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.out$model$CY,partial.resids.DP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(DP_TP.out.pdat,matrix(fit.DOYCY,ncol=length(unique(DP_TP.out.pdat$CY)),nrow=length(unique(DP_TP.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(DP_TP.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(DP_TP.out.pdat$CY),y=unique(DP_TP.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(DP_TP.out.pdat$CY),y=unique(DP_TP.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  NH4_TN.in.pdat=subset(NH4_TN.in.pdat,CY>=2013)
  NH4_TN.out.pdat=subset(NH4_TN.out.pdat,CY>=2013)
  # NH4_TN
  {
    ylim.val=c(-5,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.in$model$DOY,partial.resids.NH4_TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NH4_TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%NH\u2084 of TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-5,15);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.in$model$CY,partial.resids.NH4_TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NH4_TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4_TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4_TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Inflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NH4_TN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4_TN.in.pdat$CY)),nrow=length(unique(NH4_TN.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NH4_TN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NH4_TN.in.pdat$CY),y=unique(NH4_TN.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NH4_TN.in.pdat$CY),y=unique(NH4_TN.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-10,10);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.out$model$DOY,partial.resids.NH4_TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NH4_TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    # mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-12,12);by.y=12;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.out$model$CY,partial.resids.NH4_TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NH4_TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4_TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4_TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NH4_TN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NH4_TN.out.pdat$CY)),nrow=length(unique(NH4_TN.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NH4_TN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NH4_TN.out.pdat$CY),y=unique(NH4_TN.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NH4_TN.out.pdat$CY),y=unique(NH4_TN.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    # mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
  
  NOx_TN.in.pdat=subset(NOx_TN.in.pdat,CY>=2012)
  NOx_TN.out.pdat=subset(NOx_TN.out.pdat,CY>=2012)
  # NOx_TN
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.in$model$DOY,partial.resids.NOx_TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NOx_TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%NOx of TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.in$model$CY,partial.resids.NOx_TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NOx_TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx_TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx_TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Inflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NOx_TN.in.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx_TN.in.pdat$CY)),nrow=length(unique(NOx_TN.in.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NOx_TN.in.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NOx_TN.in.pdat$CY),y=unique(NOx_TN.in.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NOx_TN.in.pdat$CY),y=unique(NOx_TN.in.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.out$model$DOY,partial.resids.NOx_TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NOx_TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-25,25);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.out$model$CY,partial.resids.NOx_TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NOx_TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx_TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx_TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    # mtext(side=3,line=1,"Outflow",cex=1,font=2)
    # mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
    xlim.val=c(2010,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
    tmp.ma1=with(NOx_TN.out.pdat,matrix(fit.DOYCY,ncol=length(unique(NOx_TN.out.pdat$CY)),nrow=length(unique(NOx_TN.out.pdat$DOY))))
    brk=10
    breaks.val=classInt::classIntervals(NOx_TN.out.pdat$fit.DOYCY,style="equal",n=brk)$brks
    pal=hcl.colors(n=brk,alpha=0.75)
    image(x=unique(NOx_TN.out.pdat$CY),y=unique(NOx_TN.out.pdat$DOY),z=t(tmp.ma1),
          breaks=breaks.val,col=pal,
          ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
    contour(x=unique(NOx_TN.out.pdat$CY),y=unique(NOx_TN.out.pdat$DOY),z=t(tmp.ma1),
            levels=breaks.val,nlevels=brk,
            add=T,drawlabels=F,lwd=0.75,col="white")
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"ti(DOY,Year)",cex=labs.cex)
    mtext(side=2,line=1.75,"DOY",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    plot(0:1,0:1,ann=F,axes=F,type="n")
    top.val=rmp.top;bot.val=rmp.bot;mid.v.val=bot.val+(top.val-bot.val)/2
    x.max=rmp.x.max;x.min=rmp.x.min;mid.val=x.min+(x.max-x.min)/2
    rasterImage(legend_image,x.min,bot.val,x.max,top.val,xpd=NA)
    text(x=x.max, y = c(bot.val,top.val), 
         labels =format(round(range(breaks.val),2)),
         cex=yaxs.cex,adj=0,pos=4,offset=0.5,xpd=NA)
    text(x=mid.val,y=top.val,"Effect",pos=3,cex=labs.cex,xpd=NA)
  }
}
dev.off()

## Version 2 -------------------------------------------------------------
# png(filename=paste0(plot.path,"PLSF_GAM_weekly_ratios_v2.png"),width=12,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,2.25,0.5,0.25));
layout(matrix(c(1,1,2,2,3:30),8,4,byrow = T),heights=c(0.2,1,1,1,1,1,1,1))

yaxs.cex=0.8
labs.cex=0.9
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]

rmp.x.max=-2
rmp.x.min=-4
rmp.top=0.8
rmp.bot=0.2
{
  TN_TP.in.pdat=subset(TN_TP.in.pdat,CY>=2011)
  TN_TP.out.pdat=subset(TN_TP.out.pdat,CY>=2011)
  # TN_TP
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Inlet",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"Lake Outlet",cex=1.5,font=2)
    par(mar=c(2,1.75,1,1))
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.in$model$DOY,partial.resids.TN_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TN_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"TN:TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.in$model$CY,partial.resids.TN_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TN_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,TN_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.out$model$DOY,partial.resids.TN_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TN_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,TN_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.out$model$CY,partial.resids.TN_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TN_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  DIN_SRP.in.pdat=subset(DIN_SRP.in.pdat,CY>=2013)
  DIN_SRP.out.pdat=subset(DIN_SRP.out.pdat,CY>=2013)
  # DIN_SRP
  {
    ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN_SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.in$model$DOY,partial.resids.DIN_SRP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DIN_SRP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN_SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN_SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"DIN:SRP\nEffect",cex=labs.cex)
    
    ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN_SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.in$model$CY,partial.resids.DIN_SRP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DIN_SRP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN_SRP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN_SRP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DIN_SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.out$model$DOY,partial.resids.DIN_SRP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DIN_SRP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN_SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN_SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DIN_SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.out$model$CY,partial.resids.DIN_SRP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DIN_SRP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN_SRP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN_SRP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  # PP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.in$model$DOY,partial.resids.PP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,PP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"%PP of TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.in$model$CY,partial.resids.PP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,PP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,PP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.out$model$DOY,partial.resids.PP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,PP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,PP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.out$model$CY,partial.resids.PP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,PP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  # SRP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.in$model$DOY,partial.resids.SRP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,SRP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"%SRP of TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.in$model$CY,partial.resids.SRP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,SRP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,SRP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.out$model$DOY,partial.resids.SRP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,SRP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,SRP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.out$model$CY,partial.resids.SRP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,SRP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  # DP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.in$model$DOY,partial.resids.DP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"%DP of TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.in$model$CY,partial.resids.DP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,DP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.out$model$DOY,partial.resids.DP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,DP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.out$model$CY,partial.resids.DP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  NH4_TN.in.pdat=subset(NH4_TN.in.pdat,CY>=2013)
  NH4_TN.out.pdat=subset(NH4_TN.out.pdat,CY>=2013)
  # NH4_TN
  {
    ylim.val=c(-5,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.in$model$DOY,partial.resids.NH4_TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NH4_TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=2,line=1.5,"%NH\u2084\u207B of TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-5,15);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.in$model$CY,partial.resids.NH4_TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NH4_TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4_TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4_TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    
    ylim.val=c(-10,10);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NH4_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.out$model$DOY,partial.resids.NH4_TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NH4_TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    
    ylim.val=c(-12,12);by.y=12;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NH4_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.out$model$CY,partial.resids.NH4_TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NH4_TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4_TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4_TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
  }
  
  NOx_TN.in.pdat=subset(NOx_TN.in.pdat,CY>=2012)
  NOx_TN.out.pdat=subset(NOx_TN.out.pdat,CY>=2012)
  # NOx_TN
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.in$model$DOY,partial.resids.NOx_TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NOx_TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%NOx of TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.in$model$CY,partial.resids.NOx_TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NOx_TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx_TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx_TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
    plot(fit~DOY,NOx_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.out$model$DOY,partial.resids.NOx_TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NOx_TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(DOY)",cex=labs.cex)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    ylim.val=c(-25,25);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
    plot(fit.CY~CY,NOx_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.out$model$CY,partial.resids.NOx_TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NOx_TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx_TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx_TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"s(Year)",cex=labs.cex)
    mtext(side=1,line=2,"Year",cex=labs.cex)
  }
}
dev.off()


## Version 3 -------------------------------------------------------------

# png(filename=paste0(plot.path,"PLSF_GAM_weekly_ratios_v3.png"),width=12,height=9.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(2,2.5,0.5,0.25));
layout(matrix(c(1,1,2,2,3:30),8,4,byrow = T),heights=c(0.2,1,1,1,1,1,1,1))

yaxs.cex=0.9
labs.cex=1
inflow.col=viridisLite::viridis(4,alpha=0.25,option="E")[2]
outflow.col=viridisLite::viridis(4,alpha=0.5,option="E")[3]
xlab.line=1.5

xlim.val1=c(0,366);by.x1=90;xmaj1=seq(xlim.val1[1],xlim.val1[2],by.x1);xmin1=seq(xlim.val1[1],xlim.val1[2],by.x1/3)
xlim.val=c(2010,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
{
  TN_TP.in.pdat=subset(TN_TP.in.pdat,CY>=2011)
  TN_TP.out.pdat=subset(TN_TP.out.pdat,CY>=2011)
  # TN_TP
  {
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"s(DOY)",cex=1.5,font=2)
    plot(0:1,0:1,axes=F,ann=F,type="n")
    text(0.5,0.5,"s(Year)",cex=1.5,font=2)
    par(mar=c(1,1.75,1,1))
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,TN_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.in$model$DOY,partial.resids.TN_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,TN_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Inlet",cex=labs.cex)
    mtext(side=2,line=1.5,"TN:TP\nEffect",cex=labs.cex)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,TN_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.out$model$DOY,partial.resids.TN_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,TN_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = TN_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = TN_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = TN_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Outlet",cex=labs.cex)
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,TN_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.in$model$CY,partial.resids.TN_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,TN_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Inlet",cex=labs.cex)
    
    ylim.val=c(-25,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,TN_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.TN_TP.out$model$CY,partial.resids.TN_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,TN_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = TN_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = TN_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = TN_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=3,adj=0,"Lake Outlet",cex=labs.cex)
  }
  
  DIN_SRP.in.pdat=subset(DIN_SRP.in.pdat,CY>=2013)
  DIN_SRP.out.pdat=subset(DIN_SRP.out.pdat,CY>=2013)
  # DIN_SRP
  {
    ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,DIN_SRP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.in$model$DOY,partial.resids.DIN_SRP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DIN_SRP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN_SRP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN_SRP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN_SRP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"DIN:SRP\nEffect",cex=labs.cex)
    
    ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,DIN_SRP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.out$model$DOY,partial.resids.DIN_SRP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DIN_SRP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DIN_SRP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DIN_SRP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DIN_SRP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-200,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,DIN_SRP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.in$model$CY,partial.resids.DIN_SRP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DIN_SRP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN_SRP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN_SRP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN_SRP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-50,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,DIN_SRP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DIN_SRP.out$model$CY,partial.resids.DIN_SRP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DIN_SRP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DIN_SRP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DIN_SRP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DIN_SRP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  # PP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,PP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.in$model$DOY,partial.resids.PP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,PP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"%PP of TP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,PP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.out$model$DOY,partial.resids.PP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,PP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = PP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = PP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = PP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,PP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.in$model$CY,partial.resids.PP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,PP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,PP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.PP_TP.out$model$CY,partial.resids.PP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,PP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = PP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = PP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = PP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  # SRP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,SRP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.in$model$DOY,partial.resids.SRP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,SRP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"%SRP of TP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,SRP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.out$model$DOY,partial.resids.SRP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,SRP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = SRP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = SRP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = SRP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,SRP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.in$model$CY,partial.resids.SRP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,SRP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,SRP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.SRP_TP.out$model$CY,partial.resids.SRP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,SRP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = SRP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = SRP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = SRP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  # DP_TP
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,DP_TP.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.in$model$DOY,partial.resids.DP_TP.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,DP_TP.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP_TP.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP_TP.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP_TP.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"%DP of TP\nEffect",cex=labs.cex)
    
    plot(fit~DOY,DP_TP.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.out$model$DOY,partial.resids.DP_TP.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,DP_TP.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = DP_TP.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = DP_TP.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = DP_TP.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,DP_TP.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.in$model$CY,partial.resids.DP_TP.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,DP_TP.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP_TP.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP_TP.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP_TP.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    plot(fit.CY~CY,DP_TP.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.DP_TP.out$model$CY,partial.resids.DP_TP.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,DP_TP.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = DP_TP.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = DP_TP.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = DP_TP.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  NH4_TN.in.pdat=subset(NH4_TN.in.pdat,CY>=2013)
  NH4_TN.out.pdat=subset(NH4_TN.out.pdat,CY>=2013)
  # NH4_TN
  {
    ylim.val=c(-5,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,NH4_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.in$model$DOY,partial.resids.NH4_TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NH4_TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4_TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=2,line=1.5,"%NH\u2084\u207B of TN\nEffect",cex=labs.cex)
    
    ylim.val=c(-10,10);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,NH4_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.out$model$DOY,partial.resids.NH4_TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NH4_TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NH4_TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NH4_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NH4_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-5,15);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,NH4_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.in$model$CY,partial.resids.NH4_TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NH4_TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4_TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4_TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4_TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    
    ylim.val=c(-12,12);by.y=12;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,NH4_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NH4_TN.out$model$CY,partial.resids.NH4_TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NH4_TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NH4_TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NH4_TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NH4_TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
  }
  
  NOx_TN.in.pdat=subset(NOx_TN.in.pdat,CY>=2012)
  NOx_TN.out.pdat=subset(NOx_TN.out.pdat,CY>=2012)
  # NOx_TN
  {
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit~DOY,NOx_TN.in.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.in$model$DOY,partial.resids.NOx_TN.in[,1],pch=19,col=inflow.col)
    lines(fit~DOY,NOx_TN.in.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx_TN.in.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx_TN.in.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx_TN.in.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"%NOx of TN\nEffect",cex=labs.cex)
    
    plot(fit~DOY,NOx_TN.out.DOY.sig,ylim=ylim.val,xlim=xlim.val1,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj1,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.out$model$DOY,partial.resids.NOx_TN.out[,1],pch=19,col=outflow.col)
    lines(fit~DOY,NOx_TN.out.DOY.sig,lwd=2)
    lines(UCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
    lines(LCI ~ DOY, data = NOx_TN.out.DOY.sig, lty = "dashed")
    lines(dsig.incr ~ DOY, data = NOx_TN.out.DOY.sig, col = "red", lwd = 3,lty=1)
    lines(dsig.decre ~ DOY, data = NOx_TN.out.DOY.sig, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj1,xmin1,xmaj1,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"DOY",cex=labs.cex)
    mtext(side=2,line=1.5,"Effect",cex=labs.cex)
    
    
    ylim.val=c(-50,50);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    plot(fit.CY~CY,NOx_TN.in.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.in$model$CY,partial.resids.NOx_TN.in[,2],pch=19,col=inflow.col)
    lines(fit.CY~CY,NOx_TN.in.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx_TN.in.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx_TN.in.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx_TN.in.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"Year",cex=labs.cex)
    
    plot(fit.CY~CY,NOx_TN.out.pdat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    abline(h=0)
    points(m.NOx_TN.out$model$CY,partial.resids.NOx_TN.out[,2],pch=19,col=outflow.col)
    lines(fit.CY~CY,NOx_TN.out.pdat,lwd=2)
    lines(upper.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
    lines(lower.CY ~ CY, data = NOx_TN.out.pdat, lty = "dashed")
    lines(dsig.CY.incr ~ CY, data = NOx_TN.out.pdat, col = "red", lwd = 3,lty=1)
    lines(dsig.CY.decr ~ CY, data = NOx_TN.out.pdat, col = "blue", lwd = 3,lty=1)
    axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=yaxs.cex);
    axis_fun(2,ymaj,ymin,format(ymaj),cex=yaxs.cex);box(lwd=1)
    mtext(side=1,line=2,"Year",cex=labs.cex)
  }
}
dev.off()


