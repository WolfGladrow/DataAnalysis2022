print('file: MaxEntChiSq.R')
# Chi-squared PDF
# (1) Wikipedia: Maximum entropy distributions:
# E(x) = k; E(ln(x)) = psi(k/2)+ln(2)
k = 5
mu = k
alpha2 = psigamma(k/2, deriv=0)+log(2)
# (2) Define function determining Lambda_2 (= L2 = z) and plot over
#     a range that includes the root:
myFct1 = function(z){psigamma(z+1, deriv=0)-log((z+1)/mu)-alpha2} # z=L2
L2lower = 0.5; L2upper = 2
L2a = seq(L2lower,L2upper,0.001)
sflag = 1
if (sflag == 1) {
  # png('MaxEntElnxZero220223.png',width=16,height=16,units='cm',res=300)
  plot(L2a,myFct1(L2a),type='l',lwd=4,col='blue',xlab=TeX('$\\lambda_2$'),
       ylab=NA,las=0,cex=0.4,cex.lab=1.5)
  title(ylab=TeX('$Fct_1$'),line=2.3,cex.lab=1.5)
  abline(0,0,col='green')
  # dev.off()
}
# (3) Numerical calculation of the root Lambda2 = L2:
#     remark: from plot of myFct1 we know that root is crossed in
#             upward direction -> set extendInt to 'upX'
out1 = uniroot(myFct1,lower=L2lower,upper=L2upper,extendInt='upX')
L2 = out1$root
# (4) Calculate the other Lagrange multipliers:
L1 = -(L2+1)/mu
L0 = 1+log( ((-L1)^(L2+1))/ gamma(L2+1) )
# (5) Check constraints:
p = function(x){exp(-1+L0+L1*x+L2*log(x))}
out2=integrate(p,0,Inf)   # normalization
px = function(x){x*exp(-1+L0+L1*x+L2*log(x))}
out3=integrate(px,0,Inf)     # mean
plnx = function(x){log(x)*exp(-1+L0+L1*x+L2*log(x))}
out4=integrate(plnx,0,Inf)   # E(ln(x))
# (6) Plot MaxEnt-PDF:
x = seq(0,20,0.01)
chisqFct = dchisq(x,df=k)
if (sflag == 2) {
  # png('MaxEntChiSq171210.png',width=16,height=16,units='cm',res=300)
  plot(x,p(x),type='l',lwd=4,col='black',xlab='x',ylab='MaxEnt-PDF',las=1,cex=0.4,cex.lab=1.5)
  lines(x,chisqFct,col='magenta',lty=3,lwd=4)
  xt = 10
  text(xt,0.14,paste('E(x) = ',as.character(mu)),col='black',pos=4,cex=1.5)
  text(xt,0.12,paste('E(ln(x)) = ',as.character(round(alpha2,4))),col='black',pos=4,cex=1.5)
  # dev.off()
}