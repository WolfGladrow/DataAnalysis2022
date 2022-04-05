print('file: LagrangeLDieIter.R')
# Lagrange multipliers for loaded die: iterative solution
# (1) initial Lagrange multipliers & probabilities
Lambda1u = 1-log(6); Lambda2u = 0; pju = rep(1/6,6); ja=seq(1,6)
print(c(round(Lambda1u,4),'Lambda1u'))   # -0.7917595
Lambda1 = Lambda1u; Lambda2 = Lambda2u; pj = pju
# (2) iteration
itmax = 5
Lambda1a = numeric(itmax); Lambda2a = numeric(itmax)
ita = seq(1,itmax)-1
for(it in 1:itmax) {
  print(c('it = ',it))
  c1 = sum(pj); c2 = sum(ja*pj); c3 = sum(ja^2*pj)
  y = (3.8-c2-c2*(1-c1)/c1)/(c3-c2^2/c1)
  x = (1-c1)/c1-c2/c1*y
  Lambda1a[it] = Lambda1; Lambda2a[it] = Lambda2
  Lambda1 = Lambda1 + x
  Lambda2 = Lambda2 + y
  pj = exp(-1+Lambda1+ja*Lambda2)
  print(c('Lambda1 = ',round(Lambda1,4)))
  print(c('Lambda2 = ',round(Lambda2,4)))
  print(c('Lambda1 = ',Lambda1))
  print(c('Lambda2 = ',Lambda2))
  print(c('pj = ',round(pj,4)))
  # test of constraints:
  #                                               exact
  print(c('sum(pj) = ',round(sum(pj),6)))        #  1
  print(c('sum(ja*pj) = ',round(sum(ja*pj),6)))  #  3.8
}
# (3) plot results:
library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('Lagrange1LoadedIter170718.png',width=16,height=16,units='cm',res=300)
  plot(ita,Lambda1a,type='p',lwd=4,col='blue',xlab='Iteration number',ylab='',las=0,cex=0.6,
       cex.lab=1.5)
  title(ylab=TeX('$Lagrange\\, multiplier\\, \\lambda_1$'),line=2.3,cex.lab=1.5)
  # dev.off()
}
if (sflag == 2) {
  png('Lagrange2LoadedIter170718.png',width=16,height=16,units='cm',res=300)
  plot(ita,Lambda2a,type='p',lwd=4,col='blue',xlab='Iteration number',ylab='',las=0,cex=0.6,
       cex.lab=1.5)
  title(ylab=TeX('$Lagrange\\, multiplier\\, \\lambda_2$'),line=2.3,cex.lab=1.5)
  dev.off()
}