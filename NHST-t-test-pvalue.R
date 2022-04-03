print('file: NHST-t-test-pvalue.R')
# plot data & mu0, SE, 1-sample-t-test 
x = c(1.5,0.3,1.8,-1.4,0.8,3.0,-0.3,0.2,-0.4,1.9,0.0,0.3,-1.0,
      1.2,3.8,0.5,-0.8,2.0,1.1,1.2,-0.4,2.7,0.5,-1.4,1.1)
T0 = 0   # hypothesized temperature of null hypothesis H0
xmean = mean(x); print(c(round(xmean,2),'sample mean (deg.C)'))
n = length(x); print(c(n,'sample size n'))   # sample size
SE = sd(x)/sqrt(n)
print(c(round(sd(x),2),'s = sd(x) (deg.C)'))
print(c(round(var(x),2),'s = var(x) (deg.C)^2'))
print(c(round(SE,4),'standard error of the mean (deg.C)'))
dis = (xmean-T0)/SE; print(c(round(dis,3),'t = (xmean-mu0)/SE'))
tvalue = (xmean-T0)/SE; print(c(round(tvalue,3),'tvalue (pedestrian)'))
tobs = as.numeric(t.test(x,mu=T0)$statistic); print(c(round(tobs,3),'tvalue = tobs (t-test)'))
tobsr = round(tobs,3)
pvalue = t.test(x)$p.value; pr = round(pvalue,3); phr = round(pvalue/2,3)
nu = n-1
tarr = seq(-4,4,0.01)
ftarr = dt(tarr,nu)
alpha = 0.05
tc = -qt(alpha/2,nu); tcr = round(tc,3)
# --------------------------------------------------------------------------------------
# png('Zar10Ex7d1pvalue191117.png',width=16,height=16,units='cm',res=300)
plot(tarr,ftarr,type='l',lwd=3,col='black',
     xlab='t',ylab=NA,las=1,cex=0.4,xlim=c(-3.8,3.8),
     ylim=c(0,0.42),xaxs='i',yaxs='i',cex.lab=1.5)
title(ylab=TeX('$Student-t(t;\\,\\nu = 24)$'),line=2.5,cex.lab=1.5)
x1=tobs; x2=max(tarr); y1=dt(x1,nu); y2=dt(x2,nu); dx=(x2-x1)/50; 
xn=seq(x1,x2,dx); yn=dt(xn,nu); xf=c(x2,x1,xn); yf=c(0,0,yn) 
polygon(xf,yf,col='blue')
x1=min(tarr); x2=-tobs; y1=dt(x1,nu); y2=dt(x2,nu); dx=(x2-x1)/50; 
xn=seq(x1,x2,dx); yn=dt(xn,nu); xf=c(x2,x1,xn); yf=c(0,0,yn) 
polygon(xf,yf,col='blue')
text(0,0.05,bquote(~alpha == .(alpha)),col='red',pos=1,cex=1.5)
text(0,0.1,bquote(~t[c] == .(tcr)),col='red',pos=1,cex=1.5)
text(0,0.15,bquote(~t[obs] == .(tobsr)),col='blue',pos=1,cex=1.5)
text(0,0.2,bquote(~p == .(pr)),col='blue',pos=1,cex=1.5)
abline(v=tobs,col='blue',lty=1)
abline(v=-tobs,col='blue',lty=1)
text(2.713,0.35,TeX('$t_{obs}$'),col='blue',pos=4,cex=1.5)
text(-3.7,0.35,TeX('$-t_{obs}$'),col='blue',pos=4,cex=1.5)
text(-3.8,0.04,as.character(phr),col='blue',pos=4,cex=1.5)
text(2.713,0.04,as.character(phr),col='blue',pos=4,cex=1.5)
abline(v=tc,col='red',lty=2)
abline(v=-tc,col='red',lty=2)
text(tc-0.6,0.35,TeX('$t_c$'),col='red',pos=4,cex=1.5)
text(-tc,0.35,TeX('$-t_c$'),col='red',pos=4,cex=1.5)
# dev.off()