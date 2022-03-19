print('file: R_AIC-LRT-230319.R')
# AIC as likelihood ratio test
# ''For many kinds of models, the asymptotic null-hypothesis distribution of 
# -2 (l0-l1) is asymptotically \chi^2 with degrees of freedom (df) equal to p1-p0.
# Consulting a \chi^2 table and assuming p1-p0 = 1,
# AIC (An = 2) becomes equivalent to a LRT test at an alpha level of about 
# .16 (i.e., the probability of a \chi^2_1 deviate being greater than 2).'' 
# Dziak et al. (2018, p.11)
# sflag = 1
# if (sflag == 1) {
xp = seq(0.1,5,0.01)
nu = 1
yp = dchisq(xp,nu)
AnAIC = 2
alphaAIC = 1-pchisq(AnAIC,nu)
library(latex2exp)
# png('ChiSq1df181119.png',width=16,height=12,units='cm',res=300)
plot(xp,yp,type='l',lwd=3,col='blue',xlab='Test statistic q',
     ylab=NA,las=1,cex=0.4,cex.lab=1.5)
title(ylab=TeX('$\\chi^2(q; \\nu = 1)$'),cex.lab=1.5,line=2.3)
x1=2; x2=max(xp); y1=dchisq(x1,nu); y2=dchisq(x2,nu); dx=(x2-x1)/50; 
xn=seq(x1,x2,dx); yn=dchisq(xn,nu); xf=c(x2,x1,xn); yf=c(0,0,yn) 
polygon(xf,yf,col='red') 
text(2,0.2,expression(paste(alpha,' = 0.16')),col='red',pos=4,cex=1.5)
# dev.off()