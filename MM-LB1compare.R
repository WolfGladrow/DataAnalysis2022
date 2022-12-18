print('file: MM-LB1compare.R')
print(date())
# Michaelis-Menten: compare non-linear fit with Lineweaver-Burk estimate
# data: I
x = c(0.40, 1.05, 1.62, 2.07, 2.32, 2.46, 2.55, 3.21, 3.87, 4.17, 4.64, 4.64, 
      4.86, 4.93, 5.10, 5.13,5.23, 5.78, 5.86, 5.87, 6.04, 6.05, 6.25, 6.70, 
      7.46, 8.51, 8.68, 9.33, 9.44, 9.66)
y = c(0.36, 1.54, 1.90, 2.54, 2.10, 2.25, 2.34, 2.56, 2.56, 2.75, 2.92, 2.84,
      2.54, 2.52, 3.00, 2.86,2.76, 3.13, 3.05, 3.03, 2.70, 2.97, 2.62, 3.13, 
      2.95, 3.31, 3.32, 3.02, 3.21, 3.24)
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
if (pShLB >= 0.05) print('Do not reject H0 (on significance level = 0.05)')
if (pShLB < 0.05) print('Reject H0 (on significance level = 0.05)')
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
sflag = 11
if (sflag == 11) { # non-linear regression
  xp = seq(0,10,0.01); yp = aNL*xp/(xp+bNL); ypLB = aEst*xp/(xp+bEst)
  ypExact = alpha*xp/(xp+beta);
  # png('MichaelisDataFit210228.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,
       ylim=c(0,4.5),las=1,yaxs='i',cex.lab=1.5)
  aEstr = round(aEst,2); uaEstr = round(uaEst,2); 
  bEstr = round(bEst,2); ubEstr = round(ubEst,2); 
  text(0.1,4.2,'Lineweaver-Burk',col='magenta',pos=4,cex=1.5)
  text(0.1,3.81,bquote(~hat(alpha) == .(aEstr) %+-% .(uaEstr)),col='magenta',pos=4,cex=1.5)
  text(0.1,3.31,bquote(~hat(beta) == .(bEstr) %+-% .(ubEstr)),col='magenta',pos=4,cex=1.5)
  lines(xp,yp,col='black',lwd=3)
  lines(xp,ypExact,col='blue',lwd=3,lty=2)
  lines(xp,ypLB,col='magenta',lwd=3,lty=4)
  text(2.5,0.6,'exact',col='blue',pos=4,cex=1.5)
  text(2.5,1.5,bquote(~alpha == .(alpha)),col='blue',pos=4,cex=1.5)
  text(2.5,1.0,bquote(~beta == .(beta)),col='blue',pos=4,cex=1.5)
  aNLr = round(aNL,2); uaNLr = round(uaNL,2); 
  bNLr = round(bNL,2); ubNLr = round(ubNL,2); 
  xt = 5
  text(xt,1.9,'non-linear regression',col='black',pos=4,cex=1.5)
  text(xt,1.51,bquote(~hat(alpha) == .(aNLr) %+-% .(uaNLr)),col='black',pos=4,cex=1.5)
  text(xt,1.01,bquote(~hat(beta) == .(bNLr) %+-% .(ubNLr)),col='black',pos=4,cex=1.5)
  # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: MM-LB1compare.R"
# "30"            "n sample size"
# "5"             "start value for alpha = Vmax"
# "3"             "start value for beta = K"
# "estimated alpha = " "3.72"   "+-"  "0.13"              
# "estimated  beta = " "1.58"   "+-"  "0.22"              
# "0.20856"               "p (Shapiro-Wilk test)"
# "Do not reject H0 (on significance level = 0.05"
# "0.25"               "gamma   true y-intercept"
# "0.5"                "delta   true slope"
# "cEst = " "0.14"    "+-"      "0.027"  
# "dEst = " "0.959"   "+-"      "0.049"  
# " ---------------------------------------------------"
# "0.00016"               "p (Shapiro-Wilk test)"
# "Reject H0 (on significance level = 0.05)"
# "7.14"                  "estimate of alpha via LB"
# "6.85"                  "estimate of beta via LB"
# "Estimate of alpha = " "7.139"  "+-"  "1.36"                
# "Estimate of beta = "  "6.846"  "+-"  "1.349"   
# -----------------------------------------------------------------------------
