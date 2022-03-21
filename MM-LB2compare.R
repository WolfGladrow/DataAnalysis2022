print('file: MM-LB2compare.R')
# Michaelis-Menten: compare non-linear fit with Lineweaver-Burk estimate
# data: II
x = c(0.31, 0.32, 0.32, 0.35, 0.36, 0.41, 0.45, 0.48, 0.50, 0.50, 0.52, 0.52,
      0.52, 0.58, 0.59, 0.59,0.62, 0.62, 0.65, 0.65, 0.73, 0.78, 0.94, 1.19, 
      1.23, 1.30, 1.46, 1.87, 2.87, 7.58)
y = c(0.57, 0.54, 0.55, 0.55, 0.61, 0.67, 0.72, 0.77, 0.81, 0.79, 0.80, 0.81,
      0.88, 0.97, 0.88, 0.92,0.97, 0.92, 0.97, 0.97, 1.16, 1.14, 1.48, 1.46, 
      1.65, 1.54, 1.64, 2.22, 2.48, 3.34)
n = length(x); print(c(n,'n sample size'))
alpha = 4 # true Vmax
beta  = 2 # true half-saturation constant K
# print(c(alpha,beta,'alpha = Vmax, beta = K'))
# ---------------------------------------------------
# (2) non-linear regression
# (2a) you have to choose start values for model parameters
astart = 5; print(c(astart,'start value for alpha = Vmax'))
bstart = 3; print(c(bstart,'start value for beta = K'))
# ---------------------------------------------------
# (2b) apply nls()
NLs = summary(nls(y ~ a*x/(x+b),start=list(a=astart,b=bstart)))
aNL = NLs$coefficients[1]; bNL = NLs$coefficients[2]
uaNL = NLs$coefficients[3]; ubNL = NLs$coefficients[4]
print(c('estimated alpha = ',round(aNL,2),'+-',round(uaNL,2)))
print(c('estimated  beta = ',round(bNL,2),'+-',round(ubNL,2)))
resNL = NLs$residuals
# ---------------------------------------------------
# (2c) Residuals (noise) normally distributed? Shapiro-Wilk test
# H0: residuals are normally distributed
ShNL = shapiro.test(resNL)
pShNL = ShNL$p.value; print(c(round(pShNL,5),'p (Shapiro-Wilk test)'))
if (pShNL >= 0.05) print('Do not reject H0 (on significance level = 0.05')
if (pShNL < 0.05) print('Reject H0 (on significance level = 0.05')
# ---------------------------------------------------
# (3) Lineweaver-Burk transformation
# (3a) apply Lineweaver-Burk transformation
gamma = 1/alpha; print(c(gamma,'gamma   true y-intercept')) 
delta = beta/alpha; print(c(delta,'delta   true slope'))
xLB = 1/x; yLB = 1/y # Lineweaver-Burk transformation
#  ---------------------------------------------------
# (3b) apply lm()
LBs = summary(lm(yLB ~ xLB))
cEst = LBs$coefficients[1]; dEst = LBs$coefficients[2]
ucEst = LBs$coefficients[3]; udEst = LBs$coefficients[4]
print(c('cEst = ',round(cEst,3),'+-',round(ucEst,3)))
print(c('dEst = ',round(dEst,3),'+-',round(udEst,3)))
resLB = LBs$residuals
print(' ---------------------------------------------------')
# (3c) Residuals (noise) normally distributed? Shapiro-Wilk test
# H0: residuals are normally distributed
ShLB = shapiro.test(resLB); 
pShLB = ShLB$p.value; print(c(round(pShLB,5),'p (Shapiro-Wilk test)'))
if (pShLB >= 0.05) print('Do not reject H0 (on significance level = 0.05')
if (pShLB < 0.05) print('Reject H0 (on significance level = 0.05')
# ---------------------------------------------------
# (3d) derive estimates for alpha, beta via Lineweaver-Burk
aEst = 1/cEst; print(c(round(aEst,2),'estimate of alpha via LB'))
bEst = dEst/cEst; print(c(round(bEst,2),'estimate of beta via LB'))
# ---------------------------------------------------
# (3e) propagation of uncertainties
uaEst = ucEst/cEst^2
print(c('Estimate of alpha = ',round(aEst,3),'+-',round(uaEst,3)))
ubEst = sqrt(ucEst^2*dEst^2/cEst^4+udEst^2/cEst^2)
print(c('Estimate of beta = ',round(bEst,3),'+-',round(ubEst,3)))
# ---------------------------------------------------
sflag = 21
if (sflag == 21) {
  xp = seq(0,10,0.01); yp = aNL*xp/(xp+bNL); ypLB = aEst*xp/(xp+bEst)
  ypExact = alpha*xp/(xp+beta);
  # png('MichaelisDataIIFit210228.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,
       ylim=c(0,4.5),las=1,yaxs='i') 
  aEstr = round(aEst,2); uaEstr = round(uaEst,2); 
  bEstr = round(bEst,2); ubEstr = round(ubEst,2); 
  text(0.1,4.2,'Lineweaver-Burk',col='magenta',pos=4,cex=1.5)
  text(0.1,3.81,bquote(~hat(alpha) == .(aEstr) %+-% .(uaEstr)),col='magenta',pos=4,cex=1.5)
  text(0.1,3.31,bquote(~hat(beta) == .(bEstr) %+-% .(ubEstr)),col='magenta',pos=4,cex=1.5)
  lines(xp,yp,col='black',lwd=3)
  lines(xp,ypExact,col='blue',lwd=3,lty=2)
  lines(xp,ypLB,col='magenta',lwd=3,lty=4)
  xt = 2
  text(xt,0.6,'exact',col='blue',pos=4,cex=1.5)
  text(xt,1.5,bquote(~alpha == .(alpha)),col='blue',pos=4,cex=1.5)
  text(xt,1.0,bquote(~beta == .(beta)),col='blue',pos=4,cex=1.5)
  aNLr = round(aNL,2); uaNLr = round(uaNL,2); 
  bNLr = round(bNL,2); ubNLr = round(ubNL,2); 
  xt = 4
  text(xt,1.9,'non-linear regression',col='black',pos=4,cex=1.5)
  text(xt,1.51,bquote(~hat(alpha) == .(aNLr) %+-% .(uaNLr)),col='black',pos=4,cex=1.5)
  text(xt,1.01,bquote(~hat(beta) == .(bNLr) %+-% .(ubNLr)),col='black',pos=4,cex=1.5)
  # dev.off()
}