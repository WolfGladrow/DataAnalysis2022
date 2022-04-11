print('file: SLR-NO3overPO4.R')
# Simple Linear rRegressio: NO3 over PO4; whole ocean: 
# install.packages('ncdf4')
library(ncdf4)
# install.packages('fields') # install package (apply only once)
library(fields)
# -------------------------------------------------------------------
infoNO3.nc = nc_open('nitrate_annual_1deg.nc')     # open netCDF file
infoPO4.nc = nc_open('phosphate_annual_1deg.nc')   # open netCDF file
NO3 = ncvar_get( infoNO3.nc, 'n_an')
PO4 = ncvar_get( infoPO4.nc, 'p_an')
# Remove 'na' (not available) values:
N = 360*180*33 # number of data (including not avaible ('na') data)
NO3clean = numeric(N); PO4clean = numeric(N)
c=0;
for (k in 1:N) {if ( (is.na(NO3[k]) == FALSE) && (is.na(PO4[k]) == FALSE) )
{ if (PO4[k] < 4){ c=c+1; NO3clean[c] = NO3[k]; PO4clean[c] = PO4[k]}}}
print(c('c = ',c))
NO3c = numeric(c); PO4c = numeric(c)
NO3c[1:c] = NO3clean[1:c]; PO4c[1:c] = PO4clean[1:c]
# Fit straight line to the data:
out = lm(NO3c ~ PO4c)
out1 = summary(out)
beta0Est  = out1$coefficients[1]  # estimate of intercept
betaEst   = out1$coefficients[2]  # estimate of slope
ubeta0Est = out1$coefficients[3]  # estimate of intercept uncertainty
ubetaEst  = out1$coefficients[4]  # estimate of slope uncertainty
xp = c(0,4); yp=c(beta0Est,beta0Est+betaEst*4)
sflag = 1
if (sflag == 1) {
  # png('NO3vsPO4all170701.png',width=16,height=16,units='cm',res=300)
  plot(PO4c,NO3c,las=1,
       xlab=expression(paste(PO[4],' (',mu,'mol ',L^-1,')')),ylab='',
       type='p',col='blue',pch='.',xlim=c(0,4),xaxs='i',yaxs='i',cex.lab=1.5)
  title(ylab=expression(paste(NO[3],' (',mu,'mol ',L^-1,')')),line=2,cex.lab=1.5)
  lines(xp,yp,col='magenta',lwd=3)
  br = round(betaEst,3); ubr = round(ubetaEst,3)
  text(0.05,40,bquote(~hat(beta)  == .(br) %+-% .(ubr)),col='magenta',pos=4,cex=1.5)
  b0r = round(beta0Est,3); ub0r = round(ubeta0Est,3)
  text(0.05,47,bquote(~hat(beta)[0]  == .(b0r) %+-% .(ub0r)),col='magenta',pos=4,cex=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
#   Plotting of more than 1 million data points takes a few seconds!
# ----------------------------------------------------------------