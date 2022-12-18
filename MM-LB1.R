print('file: MM-LB1.R')
print(date())
# Michaelis-Menten: estimate parameters using Lineweaver-Burk transformation
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
# resLB = LBs$residuals
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
# ------------------------------------------------------------------
sflag = 12
if (sflag == 12) { 
  xp = c(min(xLB),max(xLB)); yp = dEst*xp+cEst
  # png('MichaelisLinDataFit210228.png',width=16,height=16,units='cm',res=300)
  plot(xLB,yLB,type='p',lwd=4,col='blue',xlab='xLB',ylab='yLB',las=1,cex=0.6,
       xlim=c(0,3),ylim=c(0,3),cex.lab=1.5)
  lines(xp,yp,col='black',lwd=3)
  aEstr = round(aEst,2); uaEstr = round(uaEst,2); 
  bEstr = round(bEst,2); ubEstr = round(ubEst,2); 
  text(0.1,2.8,bquote(~alpha == .(alpha)),col='magenta',pos=4,cex=1.5)
  text(0.1,2.5,bquote(~beta == .(beta)),col='magenta',pos=4,cex=1.5)
  text(0.7,2.81,bquote(~hat(alpha) == .(aEstr) %+-% .(uaEstr)),col='magenta',pos=4,cex=1.5)
  text(0.7,2.51,bquote(~hat(beta) == .(bEstr) %+-% .(ubEstr)),col='magenta',pos=4,cex=1.5)
  cEstr = round(cEst,2); ucEstr = round(ucEst,2); 
  dEstr = round(dEst,2); udEstr = round(udEst,2); 
  text(1.0,0.5,bquote(~gamma == .(gamma)),col='black',pos=4,cex=1.5)
  text(1.0,0.1,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
  text(1.8,0.51,bquote(~hat(gamma) == .(cEstr) %+-% .(ucEstr)),col='black',pos=4,cex=1.5)
  text(1.8,0.11,bquote(~hat(delta) == .(dEstr) %+-% .(udEstr)),col='black',pos=4,cex=1.5)
  # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: MM-LB1.R"
# "Sun Dec 18 06:05:04 2022"
# "30"            "n sample size"
# "(3) Lineweaver-Burk transformation:"
# "(3a) apply Lineweaver-Burk transformation:"
# "0.25"               "gamma   true y-intercept"
# "0.5"                "delta   true slope"
# "cEst = " "0.14"    "+-"      "0.027"  
# "dEst = " "0.959"   "+-"      "0.049"  
# "7.14"                "estimate of alpha via LB" 
# "6.85"                "estimate of beta via LB"
# "Estimate of alpha = " "7.139"  "+-"  "1.36"                
# "Estimate of beta = "  "6.846"  "+-"  "1.349"    
# -----------------------------------------------------------------------------