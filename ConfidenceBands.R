print('file: ConfidenceBands1.R')
# Casella02Berger: Table 11.3.1 p.542 (xns = x not sorted)
xns = c(3.74,3.66,0.78,2.40,2.18,1.93,0.20,2.50,3.50,1.35,2.36,3.13,
        1.22,1.00,1.29,0.95,1.05,2.92,1.76,0.51,2.17,1.99,1.53,2.60)
yns = c(3.22,4.87,0.12,2.31,4.25,2.24,2.81,3.71,3.11,0.90,4.39,4.36,
        1.23,3.13,4.05,2.28,3.60,5.39,4.12,3.16,4.40,1.18,2.54,4.89)
n = length(xns); print(c(n,'sample size n'))
xmean = mean(xns); print(c(round(xmean,2),'xmean'))
ymean = mean(yns); print(c(round(ymean,2),'ymean'))
Sxx = sum((xns-xmean)^2); print(c(round(Sxx,2),'Sxx'))
print(' ---------------------------------------------------')
print('Sort data & simple linear regression (SLM):')
sx = order(xns); x = xns[sx]; y = yns[sx] 
outSLR = lm(y ~ x)
b0 = as.numeric(outSLR$coefficients[1]); print(c(round(b0,4),'intercept (estimate)'))
b1 = as.numeric(outSLR$coefficients[2]); print(c(round(b1,4),'slope (estimate)'))
r = outSLR$residuals
print(' ---------------------------------------------------')
print(' Normally distributed residuals? Perform Shapiro-Wilk test:')
outST = shapiro.test(r) # W = 0.89667, p-value = 0.01831
pST = outST$p.value; print(c(round(pST,4),'p-value Shapiro-Wilk test'))
if (pST < 0.05) print('Reject H0: residuals from normal PDF, alpha = 0.05')
if (pST >= 0.05) print('Do not reject H0: residuals from normal PDF, alpha = 0.05')
S = sqrt(sum(r^2)/(n-2)); print(c(round(S,4),'S=standard error of the regression'))
# Gafarian (1964, p.187)
print(' ---------------------------------------------------')
alpha = 0.1; print(c(alpha,'alpha (as in Casella & Berger, 2002)'))
print(' ---------------------------------------------------')
print('Confidence bands: single point confidence interval approximation:')
print('hyperbolic')
nures = n-2   # degrees of freedom 
tc = qt(1-alpha/2,nures); print(c(round(tc,4),'tc'))
SE = S*sqrt((1/n+(x-xmean)^2/Sxx))
CB1U = b0+b1*x+tc*SE # upper 90\% confidence band
CB1L = b0+b1*x-tc*SE # lower 90\% confidence band
print(' ---------------------------------------------------')
print('Confidence bands: (2) Scheffe (1959): hyperbolic')
# Casella \& Berger (2002, p.560, Eq. 11.3.43)
Malpha = sqrt(2*qf(1-alpha,2,n-2)); print(c(round(Malpha,4),'Malpha'))
print('or Malpha = sqrt(2*qf(alpha,2,n-2,lower.tail=FALSE))')
CB2U = b0+b1*x+Malpha*S*sqrt(1/n+(x-xmean)^2/Sxx)
CB2L = b0+b1*x-Malpha*S*sqrt(1/n+(x-xmean)^2/Sxx)
print(' ---------------------------------------------------')
print('Confidence interval at x = x0: single point confidence interval')
x0 = 1.2
SE0 = S*sqrt((1/n+(x0-xmean)^2/Sxx))
xpCI = c(x0,x0)
CI1x1 = c(b0+b1*x0-tc*SE0,b0+b1*x0+tc*SE0)
print(' ---------------------------------------------------')
print(' Gafarian (1964) confidence bands: straight ')
# ------------------------------------------------------------------------
density2 = function(t1,t2,n) 1/(1+(t1^2+t2^2)/(n-2))^(n/2)/2/pi
# Miller (1981, p.122, Eq.24)
# ------------------------------------------------------------------------
Gafarian1964 = function(n,xL,xU,delta,sigXi,ndt2) {
  # input:
  #   n = sample size
  #   xL = lower limit of interval (= a in G64; = x_* in Miller81)
  #   xU = upper limit of interval (= b in G64; = x^* in Miller81)
  #   delta = half-width/sigEst (first guess;
  #          sigEst = standard deviation of the regression)
  #   sigXi = sd of sample points x (= s in G64, = v in M81)
  #   ndt2 = number of intervals in t2-direction (for integration)
  # output: 
  #   1 - alpha = integral over parallelogram
  # remark: xmean = mean(X) = mean of sample points = (xU+xL)/2
  # ------------------------------------------------------------------------
  t11 = sqrt(n)*delta; t21 = 0 # parallelogram corners coordinates Miller81 Fig.6
  c1 = 2*sqrt(n)*delta/(xU-xL); 
  t12 = 0; t22 = c1*sigXi; # print(c(t11,t21,'corner 1')); print(c(t12,t22,'corner 2'))
  b12 = -t22/(t11-t12); a12 = -b12*t11 # line through corners 1 \& 2
  # print(c(a12,b12,'line through corners 1 & 2'))
  # ------------ Integration: split t2 range into ndt2 slices:
  # symmetry -> integrate over positive quadrant, then multiply by 4
  dt2 = t22/ndt2; t2 = 0
  t1Lower = 0; t1Upper = (t2-a12)/b12
  integrand = function(t1) density2(t1,t2,n)
  q = integrate(integrand,t1Lower,t1Upper)$value
  for(k in 1:(ndt2-1)) {
    t2 = dt2*k; t1Upper = (t2-a12)/b12
    integrand = function(t1) density2(t1,t2,n)
    q = q+integrate(integrand,t1Lower,t1Upper)$value
  }
  return(4*q*dt2)
}
print(' ---------------------------------------------------')
print('Guess delta from single point CI in middle of x-range')
x01 = (max(x)-min(x))/2
SE11 = S*sqrt((1/n+(x01-xmean)^2/Sxx))
deltaGuess = SE11*tc; print(c(round(deltaGuess,4),'deltaGuess'))
print(' ---------------------------------------------------')
xU = max(x); xL = min(x); print(c(xL,xU,'xL,xU'))
print('G64: chose xU, xL such that (xL+xU)/2 = xmean:')
xL = 2*xmean-xU; print(c(xL,xU,'xL,xU'))
sigXi = sqrt(sum((x-xmean)^2)/n); print(c(round(sigXi,4),'sigXi = sd of x = v in M81'))
cGaf = 2*sigXi/(xU-xL); print(c(round(cGaf,1),'cGaf'))
onemalpha = 1 - alpha
dalpha = alpha/500; print(c(dalpha,'dalpha'))
ndt2 = 1e4
print('Try to find delta for given alpha by iteration!')
delta = deltaGuess
ddelta = 0.1; print(c(ddelta,'ddelta')) # 1. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) delta = delta-ddelta
  if(out < onemalpha) delta = delta+ddelta
  out = Gafarian1964(n,xL,xU,delta,sigXi,ndt2)
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
  print(c(k,dalpha,out-onemalpha,'k,dalpha,out-onemalpha'))}
}
print(c(k,dalpha,out-onemalpha,'k,dalpha,out-onemalpha'))
ddelta = 0.01; print(c(ddelta,'ddelta')) # 2. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) delta = delta-ddelta
  if(out < onemalpha) delta = delta+ddelta
  out = Gafarian1964(n,xL,xU,delta,sigXi,ndt2)
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
  print(c(k,dalpha,out-onemalpha,'k,dalpha,out-onemalpha'))}
}
print(c(k,dalpha,out-onemalpha,'k,dalpha,out-onemalpha'))
ddelta = 0.001; print(c(ddelta,'ddelta')) # 3. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) delta = delta-ddelta
  if(out < onemalpha) delta = delta+ddelta
  out = Gafarian1964(n,xL,xU,delta,sigXi,ndt2)
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
  print(c(k,dalpha,out-onemalpha,'k,dalpha,out-onemalpha'))}
}
print(c(k,dalpha,out-onemalpha,'k,dalpha,out-onemalpha'))
ndt2 = 1e5
out = Gafarian1964(n,xL,xU,delta,sigXi,ndt2)
print(c(out,1-out,out-onemalpha,'1-alpha,alpha,out-onemalpha'))
print(c(round(delta,4),'delta'))
print(c(round(delta*S,4),'delta*S = Gafarian half-width'))
CBGafU = b0+b1*x+delta*S
CBGafL = b0+b1*x-delta*S
print(' ---------------------------------------------------')
print('Bowden & Graybill (1966):')
print(' ---------------------------------------------------')
IntegrandBG66 = function(z,n,rho) {u=z[1]; v=z[2];
return(1/(2*pi*sqrt(1-rho^2))*(1+(u^2-2*rho*u*v+v^2)/((n-2)*(1-rho^2)))^(-n/2))}
# install.packages('cubature')
library(cubature)
print(' ---------------------------------------------------')
Dg = 3 # guess D
gB66 = sqrt(1/n+(xL-xmean)^2/(n*sigXi^2))
hB66 = sqrt(1/n+(xU-xmean)^2/(n*sigXi^2))
AB66 = gB66/hB66; print(c(round(AB66,4),'AB66'))
rho = (1+(xL-xmean)*(xU-xmean)/sigXi^2)/(sqrt(1+(xL-xmean)^2/sigXi^2)*
                                           sqrt(1+(xU-xmean)^2/sigXi^2))
print(c(round(rho,4),'rho'))
A = AB66; print(c(round(A,4),'A'))
dalpha = alpha/500; print(c(dalpha,'dalpha'))
deltaDg = 0.1; print(c(deltaDg,'deltaDg'))  # 1. decimal
k = 0; out = 0
while ((k < 20) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) Dg = Dg-deltaDg
  if(out < onemalpha) Dg = Dg+deltaDg
  out4=adaptIntegrate(IntegrandBG66,lowerLimit=c(-Dg,-A*Dg),
                      upperLimit=c(Dg,A*Dg),n,rho)
  out = out4$integral;
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
  print(c(k,out-onemalpha,'k,out-onemalpha'))}
}
print(c(k,Dg,'k,Dg'))
deltaDg = 0.01; print(c(deltaDg,'deltaDg'))  # 2. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) Dg = Dg-deltaDg
  if(out < onemalpha) Dg = Dg+deltaDg
  out4=adaptIntegrate(IntegrandBG66,lowerLimit=c(-Dg,-A*Dg),
                      upperLimit=c(Dg,A*Dg),n,rho)
  out = out4$integral;
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
  print(c(k,out-onemalpha,'k,out-onemalpha'))}
}
print(c(k,Dg,'k,Dg'))
deltaDg = 0.001; print(c(deltaDg,'deltaDg'))  # 3. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) Dg = Dg-deltaDg
  if(out < onemalpha) Dg = Dg+deltaDg
  out4=adaptIntegrate(IntegrandBG66,lowerLimit=c(-Dg,-A*Dg),upperLimit=c(Dg,A*Dg),n,rho)
  out = out4$integral;
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
  print(c(k,out-onemalpha,'k,out-onemalpha'))}
}
print(c(k,Dg,'k,Dg'))
print(c(round(Dg,4),'D (final) = half-width (Bowden66Graybill)'))
print(c(round(Dg,2),'D (final) = half-width (Bowden66Graybill)'))
print('B66,Table3,alpha=0.1,n-2=20,A=1,|rh0|=0.3: D=2.05')
deltaB66 = Dg*gB66; 
print(c(round(deltaB66,4),'deltaB66 = Cstar (Bowden66Graybill)'))
print(c(round(deltaB66*S,4),'deltaB66*S = Bowden66 half-width'))
ratio = deltaB66/delta; print(c(round(ratio,4),'ratio')) 
print(' ---------------------------------------------------')
print(c(round(deltaB66*S,4),'deltaB66*S = Gafarian half-width (Bowden66)'))
CBBow66U = b0+b1*x+deltaB66*S
CBBow66L = b0+b1*x-deltaB66*S
sflag = 2
if (sflag == 2) { # data \& Scheffe \& Gafarian \& single point CI
  # png('ConfidenceBands200628.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  abline(outSLR,col='blue')
  lines(xpCI,CI1x1,col='red',lty=1) # single point CI
  lines(x,CB2U,col='red',lty=1) # Scheffe
  lines(x,CB2L,col='red',lty=1)
  lines(x,CBBow66U,col='black',lty=1) # Gafarian (Bowden \& Greybill, 1966)
  lines(x,CBBow66L,col='black',lty=1)
  # dev.off()
}