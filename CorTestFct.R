print('file: CorTestFct.R')
print(date())
# Correlation different from zero? testcorsig0()
testcorsig0 = function(r,n) {
  # Test: Is correlation significantly different from 0?
  # Function yields (1) p-value for 2-tailed test
  #   and (2) decision based on alpha = 0.05
  # The p-value for the 'directional' (1-tailed) test can be obtained
  # simply by division by 2.
  tcor=r/sqrt((1-r^2)/(n-2)); p = 2*pt(-abs(tcor),df=n-2);
  if (p > 1.e-4) {pstring=as.character(round(p,4)) 
  } else { pstring=as.character(p)}
  print(paste('p = ',pstring));
  alpha = 0.05;
  if (p < alpha) {print('Null hypothesis (no correlation) rejected (alpha=0.05)')
  } else {print('Null hypothesis (no correlation) not rejected (alpha=0.05)')};
  return(p)}
# --------------------------------------------------------------------------------
# Calculate the p-values of the test for correlation significance
#   for r = 0.6 (r^2 = 0.36) as a function of sample size between N = 6 and 17.
r = 0.6
Narray = seq(6,17); L = length(Narray); pa = numeric(L)
for(k in 1:L) {n=Narray[k]; pa[k]=testcorsig0(r,n)}
library(latex2exp)
# png('TestCorSigExer160728.png',width=16,height=16,units='cm',res=300)
plot(Narray,pa,type='p',lwd=4,col='red',xlab='Sample size n',ylab='p-value',
     las=1,cex=0.6,cex.lab=1.5)
points(Narray[1:6],pa[1:6],lwd=4,col='black',cex=0.6)
abline(h=0.05,col='red',lty=4); abline(v=11.5,col='black',lty=2)
text(7,0.19,pos=4,TeX('$H_0 \\, not\\, rejected$'),col='black',cex=1.5)
text(14,0.035,pos=4,TeX('$H_0 \\, rejected$'),col='red',cex=1.5)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: CorTestFct.R"
# "Sat Dec 17 10:41:21 2022"
# "p =  0.208"
# "Null hypothesis (no correlation) not rejected (alpha=0.05)"
# "p =  0.1544"
# "Null hypothesis (no correlation) not rejected (alpha=0.05)"
# "p =  0.1158"
# "Null hypothesis (no correlation) not rejected (alpha=0.05)"
# "p =  0.0876"
# "Null hypothesis (no correlation) not rejected (alpha=0.05)"
# "p =  0.0667"
# "Null hypothesis (no correlation) not rejected (alpha=0.05)"
# "p =  0.051"
# "Null hypothesis (no correlation) not rejected (alpha=0.05)"
# "p =  0.0392"
# "Null hypothesis (no correlation) rejected (alpha=0.05)"
# "p =  0.0302"
# "Null hypothesis (no correlation) rejected (alpha=0.05)"
# "p =  0.0233"
# "Null hypothesis (no correlation) rejected (alpha=0.05)"
# "p =  0.0181"
# "Null hypothesis (no correlation) rejected (alpha=0.05)"
# "p =  0.014"
# "Null hypothesis (no correlation) rejected (alpha=0.05)"
# "p =  0.0109"
# "Null hypothesis (no correlation) rejected (alpha=0.05)"
# -----------------------------------------------------------------------------
