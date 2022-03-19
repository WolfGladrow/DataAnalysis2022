print('file: PolynomialFit.R')
# fit polynomials of different order to data
# created by: Dieter.Wolf-Gladrow@awi.de 
#    4/2021 version 1.0
# This software is provided 'as is' without warranty of
# any kind. But it's mine, so you can't sell it.
# 
# ---------------------------------------------------------
  x = 1:100; L = length(x)
# Example: generate artificial data
  set.seed(1953) # set seed for random number generators
  kmax = 5
  r = runif(kmax,min(x),max(x))
  r[1] = 5; r[2] = 95
  y = (x-r[1])*(x-r[2])*(x-r[3])*(x-r[4])*(x-r[5])
  y = y+1e6*x        # add linear trend
  y = y+1e7*rnorm(L) # add normal noise
  y = y/1e8          # scale (to avoid exponential numbers in axis legend)
# ---------------------------------------------------
# Ordinary Least-Squares (OLS):
  OLS = lm(y ~ x) 
# ---------------------------------------------------
  print('Polynomial up to x^2')
  Poly2 = lm(y ~ x+I(x^2)) # where I() means 'inhibit interpretation'
  (c2 = as.numeric(Poly2$coefficients))
  y2 = c2[2]*x+c2[3]*x^2+c2[1] # fitted polynomial
# ---------------------------------------------------
  print('Polynomial up to x^3')
  Poly3 = lm(y ~ x+I(x^2)+I(x^3))
  (c3 = as.numeric(Poly3$coefficients))
  y3 = c3[2]*x+c3[3]*x^2+c3[4]*x^3+c3[1]
# ---------------------------------------------------
  print('Polynomial up to x^5')
  Poly5 = lm(y ~ x+I(x^2)+I(x^3)+I(x^4)+I(x^5))
  (c5 = as.numeric(Poly5$coefficients))
  y5 = c5[2]*x+c5[3]*x^2+c5[4]*x^3+c5[5]*x^4+c5[6]*x^5+c5[1]
# png('PolynomialFit210622.png',width=16,height=12,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y(x)',las=1,cex=0.6,cex.lab=1.5)
  abline(OLS,col='black')
  lines(x,y2,col='magenta')
  lines(x,y3,col='blue',lty=2)
  lines(x,y5,col='red',lty=4)
# dev.off()
# ---------------------------------------------------
# print('AIC, BIC:')
  AIC1 = AIC(OLS);   print(c(round(AIC1,2),'AIC1'))
  AIC2 = AIC(Poly2); print(c(round(AIC2,2),'AIC2'))
  AIC3 = AIC(Poly3); print(c(round(AIC3,2),'AIC3'))
  AIC5 = AIC(Poly5); print(c(round(AIC5,2),'AIC5'))