print('file: MannWhitneyGenFct.R')
# Mann-Whitney test: generating function, n=3=m
n = 3; m = 3
fx = function(x) { # (1) generating function
  s1 = 1; s2 = 1; for(i in (m+1):(m+n)) s1=s1*(1-x^i); for(i in 1:n) s2=s2*(1-x^i);
  return(s1/s2/(factorial(n+m)/factorial(m)/factorial(n)))}
xa = seq(-0.5,0.5,1e-3)
sflag = 1
if (sflag == 1) {
  # png('GenFct33MWtest180527.png',width=16,height=12,units='cm',res=300)
  plot(xa,fx(xa),type='l',lwd=3,col='blue',xlab='x',
       ylab='Generating function f(x;3,3)',las=1,cex=0.4,cex.lab=1.5)
  # dev.off()
}