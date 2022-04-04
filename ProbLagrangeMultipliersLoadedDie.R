print('file: ProbLagrangeMultipliersLoadedDie.R')
# Lagrange multipliers for loaded die: z-eq.
mu = 3.8
myFct1 = function(z) (1-z^7)/(1-z)-1-z/mu*(1-7*z^6+6*z^7)/(1-z)^2
zlower=1.05; zupper=1.15
za = seq(zlower,zupper,0.001)
zopt = uniroot(myFct1,lower=zlower,upper=zupper,extendInt='downX')$root
L2 = log(zopt)
L1 = 1 + log(mu/zopt*(1-zopt)^2/(1-7*zopt^6+6*zopt^7))
ja = seq(1,6)
pj = exp(-1+L1+L2*ja)
pj0 = rep(1/6,6)
# test constraints:
C1 = sum(pj)      # normalization
C2 = sum(ja*pj)   # mean
sflag = 2
library(latex2exp)
# text(0,0.2,TeX('$\\mu = 0$'),col='black')
if (sflag == 1) {
  # png('LoadedDieZeq171211.png',width=16,height=16,units='cm',res=300)
  plot(za,myFct1(za),type='l',lwd=4,col='blue',xlab='z',ylab='f(z)',las=1,cex=0.4,cex.lab=1.5)
  abline(0,0,col='green')
  # dev.off()
}
xp = c(1,6); yp = c(pj[1],pj[6])
if (sflag == 2) {
  # png('LoadedDiePj220222.png',width=16,height=16,units='cm',res=300)
  plot(ja,pj0,type='p',lwd=4,col='black',xlab='j',pch=24,
       ylab=NA,las=1,cex=0.6,ylim=c(0,0.25),cex.lab=1.5)
  title(ylab=TeX('$p_j$'),line=2.3,cex.lab=1.5)
  lines(xp,yp,col='green')
  points(ja,pj,col='blue',lwd=4,cex=0.6)
  # dev.off()
}
print(' ---------------------------------------------------')
print('Results: ')
print(c('z       =  ',round(zopt,4)))
print(c('Lambda1 = ',round(L1,4)))
print(c('Lambda2 =  ',round(L2,4)))
print(c('pj       = ',round(pj,4)))
print(c('normalization       = ',round(C1,4)))
print(c('mean                = ',round(C2,4)))
# ----------------------------------------------------------------
# Remarks:
# myfct1 = function(z) {...}   create your own function; curly brackets not required when
#                                 only one term
# seq(x1,x2,dx)   generate a sequence of numbers from x1 to x2 with equidistant spacing dx
# uniroot(f,lower,upper)   searches the interval from lower to upper for a root (i.e., zero) of the function f
# uniroot(f,lower,upper)$root   take only the variable 'root' from output of uniroot()
# ----------------------------------------------------------------