print('file: MM-LB2res.R')
print(date())
# Michaelis-Menten: estimate parameters using Lineweaver-Burk transformation
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
print('(3) Lineweaver-Burk transformation:')
print('(3a) apply Lineweaver-Burk transformation:')
gamma = 1/alpha; print(c(gamma,'gamma   true y-intercept')) 
delta = beta/alpha; print(c(delta,'delta   true slope'))
xLB = 1/x; yLB = 1/y # Lineweaver-Burk transformation
# ---------------------------------------------------
# (3b) OLS of transformed data: apply lm()
LBs = summary(lm(yLB ~ xLB))
cEst = LBs$coefficients[1]; dEst = LBs$coefficients[2]
ucEst = LBs$coefficients[3]; udEst = LBs$coefficients[4]
print(c('cEst = ',round(cEst,3),'+-',round(ucEst,3)))
print(c('dEst = ',round(dEst,3),'+-',round(udEst,3)))
resLB = LBs$residuals
# ------------------------------------------------------------------
sflag = 24
if (sflag == 24) { # residuals of Lineweaver-Burk followed by simple linear regression
  # png('MichaelisResIILB210228.png',width=16,height=16,units='cm',res=300)
  plot(xLB,resLB,type='p',lwd=4,col='blue',xlab='xLB',ylab='Residuals LB',las=1,
       cex=0.6,cex.lab=1.5)
  abline(h=0,col='magenta',lty=4)
  # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: MM-LB2res.R"
# "Sun Dec 18 06:51:30 2022"
# "30"            "n sample size"
# "(3) Lineweaver-Burk transformation:"
# "(3a) apply Lineweaver-Burk transformation:"
# "0.25"          "gamma   true y-intercept"
# "0.5"           "delta   true slope"
# "cEst = " "0.216"   "+-"      "0.021"  
# "dEst = " "0.516"   "+-"      "0.011" 
# -----------------------------------------------------------------------------
