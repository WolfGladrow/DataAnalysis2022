print('file: MMnonlinearFit.R')
print(date())
# Michaelis-Menten: non-linear regression 
# ---------------------------------------------------
# data:
  x = c(0.40, 1.05, 1.62, 2.07, 2.32, 2.46, 2.55, 3.21, 3.87, 4.17, 4.64, 4.64, 
        4.86, 4.93, 5.10, 5.13,5.23, 5.78, 5.86, 5.87, 6.04, 6.05, 6.25, 6.70, 
        7.46, 8.51, 8.68, 9.33, 9.44, 9.66)
  y = c(0.36, 1.54, 1.90, 2.54, 2.10, 2.25, 2.34, 2.56, 2.56, 2.75, 2.92, 2.84,
        2.54, 2.52, 3.00, 2.86,2.76, 3.13, 3.05, 3.03, 2.70, 2.97, 2.62, 3.13, 
        2.95, 3.31, 3.32, 3.02, 3.21, 3.24)
n = length(x); print(c(n,'n sample size'))
alpha = 4; print(c(alpha,'true alpha')) # true Vmax
beta  = 2; print(c(beta,'true beta'))   # true half-saturation constant K
# non-linear regression: you have to choose start values for model parameters
astart = 5; print(c(astart,'start value for alpha = Vmax'))
bstart = 3; print(c(bstart,'start value for beta = K'))
NLs = summary(nls(y ~ a*x/(x+b),start=list(a=astart,b=bstart))) # <-----
aNL = NLs$coefficients[1]; bNL = NLs$coefficients[2]
uaNL = NLs$coefficients[3]; ubNL = NLs$coefficients[4]
print(c('estimated alpha = ',round(aNL,2),'+-',round(uaNL,2)))
print(c('estimated  beta = ',round(bNL,2),'+-',round(ubNL,2)))
# install.packages('latex2exp') 
library(latex2exp)
sflag = 1
if (sflag == 1) { # data plus non-linear regression
  xp = seq(0,10,0.01); yp = aNL*xp/(xp+bNL)
# png('MichaelisDataI210228.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,
       xlim=c(0,10),ylim=c(0,4),cex.lab=1.5)
  lines(xp,yp,col='black',lwd=3)
  text(3,1.5,TeX('$\\alpha = 4$'),col='blue',pos=4,cex=1.5)
  text(3,1.1,TeX('$\\beta = 2$'),col='blue',pos=4,cex=1.5)
  aNLr = round(aNL,2); uaNLr = round(uaNL,2)
  text(5.9,1.52,bquote(~hat(alpha) == .(aNLr) %+-% .(uaNLr)),col='black',pos=4,cex=1.5)
  bNLr = round(bNL,2); ubNLr = round(ubNL,2)
  text(5.9,1.1,bquote(~hat(beta) == .(bNLr) %+-% .(ubNLr)),col='black',pos=4,cex=1.5)
# dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: MMnonlinearFit.R"
# "Sun Dec 18 07:08:43 2022"
# "30"        "n sample size"
# "4"         "true alpha"
# "2"         "true beta"
# "5"             "start value for alpha = Vmax"
# "3"             "start value for beta = K"
# "estimated alpha = " "3.72"  "+-"   "0.13"              
# "estimated  beta = " "1.58"  "+-"   "0.22"              
# -----------------------------------------------------------------------------
# Remarks: 
# The non-linear fitting routine nls() yields reasonable estimates (given the
#   small sample size n=30 and the noise in the data), however, good start
#   values (close to the true values) have to be provided.
# The non-linear function that should be fitted to the data is specified in the
#   argument list of nls(): nls(y ~ a*x/(x+b),start=list(a=astart,b=bstart));
#   it is used as the predictor.
# -----------------------------------------------------------------------------

