print('file: MM-LB2.R.R')
# Michaelis-Menten: esimate parameters using Lineweaver-Burk transformation
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
# resLB = LBs$residuals
# ---------------------------------------------------
# (3d) derive estimates for alpha, beta via Lineweaver-Burk
aEst = 1/cEst; print(c(round(aEst,2),'estimate of alpha via LB'))
bEst = dEst/cEst; print(c(round(bEst,2),'estimate of beta via LB'))
# ---------------------------------------------------
# (3e) propagation of uncertainties
# gamma = 1/alpha <-> alpha = 1/gamma = a = 1/c
# ua^2 = (-1/c^2)^2*uc^2 -> ua = uc/c^2
uaEst = ucEst/cEst^2
print(c('Estimate of alpha = ',round(aEst,3),'+-',round(uaEst,3)))
# delta = beta/alpha <-> beta = delta*alpha = b = d*a = d/c
# ub^2 = (-d/c^2)^2 * uc^2 + (1/c)^2 * ud^2
# ub = sqrt(uc^2*d^2/c^4 + ud^2/c^2)
ubEst = sqrt(ucEst^2*dEst^2/cEst^4+udEst^2/cEst^2)
print(c('Estimate of beta = ',round(bEst,3),'+-',round(ubEst,3)))
# ------------------------------------------------------------------
sflag = 12
if (sflag == 12) { 
  xp = c(min(xLB),max(xLB)); yp = dEst*xp+cEst
  # png('MichaelisLinDataFitII210228.png',width=16,height=16,units='cm',res=300)
  plot(xLB,yLB,type='p',lwd=4,col='blue',xlab='xLB',ylab='yLB',las=1,cex=0.6,
       xlim=c(0,4),ylim=c(0,2),cex.lab=1.5)
  lines(xp,yp,col='black',lwd=3)
  aEstr = round(aEst,2); uaEstr = round(uaEst,2); 
  bEstr = round(bEst,2); ubEstr = round(ubEst,2); 
  text(0.1,1.8,bquote(~alpha == .(alpha)),col='magenta',pos=4,cex=1.5)
  text(0.1,1.5,bquote(~beta == .(beta)),col='magenta',pos=4,cex=1.5)
  text(0.7,1.81,bquote(~hat(alpha) == .(aEstr) %+-% .(uaEstr)),col='magenta',pos=4,cex=1.5)
  text(0.7,1.51,bquote(~hat(beta) == .(bEstr) %+-% .(ubEstr)),col='magenta',pos=4,cex=1.5)
  cEstr = round(cEst,2); ucEstr = round(ucEst,2); 
  dEstr = round(dEst,2); udEstr = round(udEst,2); 
  text(1.0,0.5,bquote(~gamma == .(gamma)),col='black',pos=4,cex=1.5)
  text(1.0,0.1,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
  text(1.8,0.51,bquote(~hat(gamma) == .(cEstr) %+-% .(ucEstr)),col='black',pos=4,cex=1.5)
  text(1.8,0.11,bquote(~hat(delta) == .(dEstr) %+-% .(udEstr)),col='black',pos=4,cex=1.5)
  # dev.off()
}