print('file: MM-LB1res.R')
# Michaelis-Menten: esimate parameters using Lineweaver-Burk transformation
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
sflag = 14
if (sflag == 14) {
  # png('MichaelisResLB210228.png',width=16,height=12,units='cm',res=300)
  plot(xLB,resLB,type='p',lwd=4,col='blue',xlab='xLB',ylab='Residuals LB',las=1,
       cex=0.6,cex.lab=1.5)
  abline(h=0,col='green',lty=4)
  # dev.off()
}