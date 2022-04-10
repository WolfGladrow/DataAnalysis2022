print('file: MMnonlinearFit2Res.R')
# Michaelis-Menten: non-linear regression 
# ---------------------------------------------------
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
# non-linear regression: you have to choose start values for model parameters
astart = 5; print(c(astart,'start value for alpha = Vmax'))
bstart = 3; print(c(bstart,'start value for beta = K'))
NLs = summary(nls(y ~ a*x/(x+b),start=list(a=astart,b=bstart)))
aNL = NLs$coefficients[1]; bNL = NLs$coefficients[2]
uaNL = NLs$coefficients[3]; ubNL = NLs$coefficients[4]
print(c('estimated alpha = ',round(aNL,2),'+-',round(uaNL,2)))
print(c('estimated  beta = ',round(bNL,2),'+-',round(ubNL,2)))
resNL = NLs$residuals
# residuals for non-linear regression for data set II
# png('MichaelisIIResNL210228.png',width=16,height=16,units='cm',res=300)
plot(x,resNL,type='p',lwd=4,col='blue',xlab='x',ylab='Residuals',las=1,cex=0.6,cex.lab=1.5)
abline(h=0,col='magenta',lty=4)
# dev.off()