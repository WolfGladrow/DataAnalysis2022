print('file: DemingRegression.R')
print(date())
# Deming regression: NO3 & PO4 & Redfield ratios
# estimate variances; notation: Casella & Berger, 2002, p.583-585
# install.packages('ncdf4') # install package (apply only once on your computer)  # ncdf4
library(ncdf4)
inNO3 = nc_open('nitrate_annual_1deg.nc')     # open netCDF file
inPO4 = nc_open('phosphate_annual_1deg.nc')
NO3 = ncvar_get(inNO3,'n_an')                 # get annual mean values (global ocean)
PO4 = ncvar_get(inPO4,'p_an')
# Remove NAs (NA = not available) and remove high PO4 values (above 4 micro-mol/L):
N = 360*180*33 # number of data (including NAs)
NO3clean = numeric(N); PO4clean = numeric(N); c=0  # c is a counter
for (k in 1:N) {if ( (is.na(NO3[k]) == FALSE) && (is.na(PO4[k]) == FALSE) )
{ if (PO4[k] < 4){c=c+1; NO3clean[c] = NO3[k]; PO4clean[c] = PO4[k]}}}
# adapt array size: 
NO3c = numeric(c); PO4c = numeric(c)
NO3c[1:c] = NO3clean[1:c]; PO4c[1:c] = PO4clean[1:c]
print(c('correlation NO3 PO4 = ',round(cor(NO3c,PO4c),4)))
out2 = summary(lm(NO3c ~ PO4c))  # simple linear regression: NO3 over PO4
b0est = out2$coefficients[1];  print(c(round(b0est,3),'b0est'))
ub0est = out2$coefficients[3]; print(c(round(ub0est,3),'ub0est'))
b1est = out2$coefficients[2];  print(c(round(b1est,3),'b1est'))
ub1est = out2$coefficients[4]; print(c(round(ub1est,3),'ub1est'))
out4 = summary(lm(PO4c ~ NO3c))  #  inverse simple (ordinary) least squares SLS(X|Y): PO4 over PO3
b2est = out4$coefficients[1]; print(c(round(b2est,3),'b2est')) 
ub2est = out4$coefficients[3]; print(c(round(ub2est,3),'ub2est'))
b3est = out4$coefficients[2]; print(c(round(b3est,3),'b3est'))
ub3est = out4$coefficients[4]; print(c(round(ub3est,3),'ub3est'))
slope2 = 1/b3est;   print(c(round(slope2,3),'slope2'))
ic2 = -b2est/b3est; print(c(round(ic2,3),'ic2'))
# Estimate ratio of variances:
x = PO4c; y = NO3c
Sxx=var(x); Syy = var(y); Sxy = var(x,y)
lambdaS = Sxx/Syy; print(c('lambdaS = ',round(lambdaS,6)))
rPO4 = out4$residuals; rNO3 = out2$residuals
lambda = var(rPO4)/var(rNO3); print(c('lambda = ',round(lambda,6)))
DemingSlope = (-(Sxx-lambda*Syy)+sqrt((Sxx-lambda*Syy)^2+4*lambda*Sxy^2))/(2*lambda*Sxy)
DemingIntercept = mean(y)-DemingSlope*mean(x)
print('---------------- Plot data & lines: ----------------')
xp = c(0,4); yp=c(b0est,b0est+b1est*4)
yp2 = c(ic2,ic2+slope2*4)
yp3=c(DemingIntercept,DemingIntercept+DemingSlope*4)
# png('NO3PO4Deming220308.png',width=16,height=16,units='cm',res=300)
plot(PO4c,NO3c,xlab=expression(paste('[',PO[4],'] (',mu,'mol ',L^{-1},')')),
     ylab=NA,type='p',col='blue',pch='.',xlim=c(0,4),xaxs='i',yaxs='i',las=1,cex.lab=1.5)
title(ylab=expression(paste('[',NO[3],'] (',mu,'mol ',L^{-1},')')),line=2.3,cex.lab=1.5)
lines(xp,yp,col='red',lwd=3)
lines(xp,yp2,col='black',lwd=3,lty=2)
lines(xp,yp3,col='magenta',lwd=3,lty=2)
b0r = round(b0est,3); ub0r = round(ub0est,3)
text(0.1,50,bquote(~hat(beta)[0,SLR1] == .(b0r) %+-% .(ub0r)),col='red',pos=4,cex=1.5)
b1r = round(b1est,3); ub1r = round(ub1est,3)
text(0.1,45,bquote(~hat(beta)[SLR1] == .(b1r) %+-% .(ub1r)),col='red',pos=4,cex=1.5)
ic2r = round(ic2,3); # uic2r = round(slope2,3)
text(0.1,40,bquote(~hat(beta)[0,SLR2] == .(ic2r)),col='black',pos=4,cex=1.5)
slope2r = round(slope2,3); # ub1r = round(ub1est,3)
text(0.1,35,bquote(~hat(beta)[SLR2] == .(slope2r)),col='black',pos=4,cex=1.5)
b0r = round(DemingIntercept,3); # ub0r = round(ub0est,3)
text(2.3,10,bquote(~hat(beta)[0,Deming] == .(b0r)),col='red',pos=4,cex=1.5)
b1r = round(DemingSlope,3); # ub1r = round(ub1est,3)
text(2.3,5,bquote(~hat(beta)[Deming] == .(b1r)),col='red',pos=4,cex=1.5)
lambdar = round(lambda,4)
text(2.3,15,bquote(~hat(lambda) == .(lambdar)),col='red',pos=4,cex=1.5)
# dev.off()
print(date())
# -----------------------------------------------------------------------------
# Results:
# "file: DemingRegression.R"
# "Sat Dec 17 11:07:51 2022"
# "correlation NO3 PO4 = " "0.9828"                
# "-1.7"  "b0est"
# "0.005"  "ub0est"
# "14.783" "b1est" 
# "0.003"  "ub1est"
# "0.168" "b2est"
# "0"      "ub2est"
# "0.065" "b3est"
# "0"      "ub3est"
# "15.306" "slope2"
# "-2.567" "ic2"   
# "lambdaS = " "0.00442"   
# "lambda = " "0.00442"  
# "---------------- Plot data & lines: ----------------"
# "Sat Dec 17 11:08:01 2022"
# -----------------------------------------------------------------------------
# Remarks:
# Generating the plot takes a few seconds.
# -----------------------------------------------------------------------------