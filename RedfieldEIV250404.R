print('file: RedfieldEIV250404.R')
print(date())
# history: R_RedfieldEIV_250401.R,RedfieldEIV.R
# updates:
#   04.04.2025 clean version for github
#   01.04.2025 geometric & bisection lines & uncertainties
# purpose: estimate molar NO3 & PO4 (Redfield) ratio
# Maximum Likelihood Estimation (MLE): Casella & Berger, 2002, p.583-585
print('----------------------------------------------------------')
print('(1) load data:')
print('----------------------------------------------------------')
# install.packages('ncdf4') # install package (apply only once on your computer)
library(ncdf4)
# data source: https://www.ncei.noaa.gov/products/world-ocean-atlas
inNO3 = nc_open('nitrate_annual_1deg.nc')     # open netCDF file
inPO4 = nc_open('phosphate_annual_1deg.nc')
NO3 = ncvar_get(inNO3,'n_an') # get annual mean values (global ocean)
PO4 = ncvar_get(inPO4,'p_an')
print('----------------------------------------------------------')
print('(2) Remove NAs (NA = not available) and remove high PO4 values') 
print('    (above 4 micro-mol/L):')
print('----------------------------------------------------------')
N = 360*180*33 # number of data (including NAs)
NO3clean = numeric(N); PO4clean = numeric(N); c=0  # c is a counter
for (k in 1:N) {if ( (is.na(NO3[k]) == FALSE) && (is.na(PO4[k]) == FALSE) )
{ if (PO4[k] < 4){c=c+1; NO3clean[c] = NO3[k]; PO4clean[c] = PO4[k]}}}
# adapt array size: 
x = numeric(c); y = numeric(c)
x[1:c] = PO4clean[1:c]; y[1:c] = NO3clean[1:c]; n = length(x)
print('----------------------------------------------------------')
# (3) Simple linear regression (SLM), y on x, NO3 on PO4:
print('----------------------------------------------------------')
print(c('correlation(x,y) = ',round(cor(x,y),4)))
out2 = summary(lm(y ~ x))   # simple linear regression: NO3 over PO4
b0est = out2$coefficients[1];  print(c(round(b0est,4),'b0est intercept'))
ub0est = out2$coefficients[3]; print(c(round(ub0est,4),'ub0est'))
b1est = out2$coefficients[2];  print(c(round(b1est,4),'b1est slope'))
ub1est = out2$coefficients[4]; print(c(round(ub1est,4),'ub1est'))
print('----------------------------------------------------------')
# (4) inverse simple (ordinary) least squares SLS(X|Y), x on y, PO4 on NO3:
print('----------------------------------------------------------')
out4 = summary(lm(x ~ y))  
b2est = out4$coefficients[1]; print(c(round(b2est,4),'b2est intercept')) 
ub2est = out4$coefficients[3]; print(c(round(ub2est,6),'ub2est'))
b3est = out4$coefficients[2]; print(c(round(b3est,4),'b3est slope x on y'))
ub3est = out4$coefficients[4]; print(c(round(ub3est,6),'ub3est'))
slope2 = 1/b3est;   print(c(round(slope2,4),'slope2 (via) x on y'))
ic2 = -b2est/b3est; print(c(round(ic2,4),'ic2'))
SlopeYonX1 = slope2; IcYonX1 = ic2
print('----------------------------------------------------------')
print(' Estimate uncertainties: Monte Carlo')
print('----------------------------------------------------------')
Mr = 1000 # number of Monte Carlo values
a = b2est; ua = ub2est; b = b3est; ub = ub3est; 
ra = rnorm(Mr,a,ua); rb = rnorm(Mr,b,ub)
rSlopeYonX1 = 1/rb; rIcYonX1 = -ra/rb
# hist(rSlopeYonX1,col='blue',breaks=round(sqrt(Mr)),las=1,main='') # check normality
# hist(rIcYonX1,col='blue',breaks=round(sqrt(Mr)),las=1,main='')       # check normality
# Both distributions are non-normal -> apply robust estimation
mrSlopeYonX1 = median(rSlopeYonX1)
mrIcYonX1 = median(rIcYonX1)
source('myMADN.R')
sdSlopeYonX1 = myMADN(rSlopeYonX1)
sdIcYonX1 = myMADN(rIcYonX1)
print(c(round(mrSlopeYonX1,5),'+-',
        round(sdSlopeYonX1,5),'mrSlopeYonX1 (median +- MADN)'))
print(c(round(mrIcYonX1,5),'+-',
        round(sdIcYonX1,5),'mrIcYonX1 (median +- MADN)'))
print(c(round(SlopeYonX1,4),'SlopeYonX1 (via) x on y'))
print(c(round(IcYonX1,4),'IcYonX1'))
# ------------------------------------------------------------------------------
print('----------------------------------------------------------')
print(' Isobe et al. (1990): slopes & their uncertainties')
print('  uncertainty = standard error, i.e. proportional 1/sqrt(n)')
print('----------------------------------------------------------')
xmean = mean(x); ymean = mean(y);
Sxx = sum((x-xmean)^2)
Syy = sum((y-ymean)^2)
Sxy = sum((x-xmean)*(y-ymean))
(beta1I90 = Sxy/Sxx) # regression y on x
(beta2I90 = Syy/Sxy) # regression (via) x on y
(beta3I90 = 1/(beta1I90+beta2I90)*(beta1I90*beta2I90-1+
      sqrt((1+beta1I90^2)*(1+beta2I90^2)))) # bisector
(beta4I90 = ((beta2I90-1/beta1I90)+sign(Sxy)*
               sqrt(4+(beta2I90-1/beta1I90)^2))/2) # orthogonal
(beta5I90 = sign(Sxy)*sqrt(beta1I90*beta2I90)) # geometric
(beta5I90b = sign(Sxy)*sqrt(Syy/Sxx)) # geometric (alternative expression)
(Var1I90 = (sum((x-xmean)^2*(y-beta1I90*x-ymean+beta1I90*xmean)^2))/Sxx^2)
(Var2I90 = (sum((y-ymean)^2*(y-beta2I90*x-ymean+beta2I90*xmean)^2))/Sxy^2)
(Cov12I90 = 1/(beta1I90*Sxx^2)*
    sum((x-xmean)*(y-ymean)*(y-ymean-beta1I90*(x-xmean))*
          (y-ymean-beta2I90*(x-xmean))))
(Var3I90 = beta3I90^2/((beta1I90+beta2I90)^2*(1+beta1I90^2)*
    (1+beta2I90^2))*((1+beta2I90^2)^2*Var1I90+
   2*(1+beta1I90^2)*(1+beta2I90^2)*Cov12I90+(1+beta1I90^2)^2*Var2I90))
(Var4I90 = beta4I90^2/(4*beta1I90^2+(beta1I90*beta2I90-1)^2)*
    (Var1I90/beta1I90^2+2*Cov12I90+beta1I90^2*Var2I90))
(Var5I90 = (beta2I90/beta1I90*Var1I90+2*Cov12I90+
              beta1I90/beta2I90*Var2I90)/4)
(sig1I90 = sqrt(Var1I90))
(sig2I90 = sqrt(Var2I90))
(sig3I90 = sqrt(Var3I90))
(sig4I90 = sqrt(Var4I90))
(sig5I90 = sqrt(Var5I90))
print(c(round(beta1I90,4),'beta1I90 y on x'))
print(c(round(beta2I90,4),'beta2I90 (via) x on y'))
print(c(round(beta3I90,4),'beta3I90 bisector'))
print(c(round(beta4I90,4),'beta4I90 orthogonal'))
print(c(round(beta5I90,4),'beta5I90 geometric'))
print(c(round(Var1I90,8),'Var1I90 y on x'))
print(c(round(Var2I90,8),'Var2I90 (via) x on y'))
print(c(round(Var3I90,8),'Var3I90 bisector'))
print(c(round(Var4I90,8),'Var4I90 orthogonal'))
print(c(round(Var5I90,8),'Var5I90 geometric'))
print(c(round(Cov12I90,8),'Cov12I90'))
print(c(round(sig1I90,5),'sig1I90 y on x'))
print(c(round(sig2I90,5),'sig2I90 (via) x on y'))
print(c(round(sig3I90,5),'sig3I90 bisection'))
print(c(round(sig4I90,5),'sig4I90 orthogonal'))
print(c(round(sig5I90,5),'sig5I90 geometric'))
print('----------------------------------------------------------')
print(' Isobe et al. (1990): intercepts & their uncertainties')
print('  uncertainty = standard error, i.e. proportional 1/sqrt(n)')
print('----------------------------------------------------------')
(IC1I90 = ymean-beta1I90*xmean)
(IC2I90 = ymean-beta2I90*xmean)
(IC3I90 = ymean-beta3I90*xmean)
(IC4I90 = ymean-beta4I90*xmean)
(IC5I90 = ymean-beta5I90*xmean)
print(c(round(b0est,4),round(IC1I90,4),'b0est,IC1I90 y on x'))
print(c(round(IcYonX1,3),round(IC2I90,3),'IcYonX1,IC2I90 (via) x on y'))
print(c(round(IC3I90,3),'IC3I90 bisection'))
print(c(round(IC4I90,3),'IC4I90 orthogonal'))
print(c(round(IC5I90,3),'IC5I90 geometric'))
# Variances of intercept estimates: Eqs.(9)-(21)
(gamma1 = beta3I90/((beta1I90+beta2I90)*sqrt((1+beta1I90^2)*
          (1+beta2I90^2)))) # Eq.(20)
(gamma2 = beta4I90/sqrt(4*beta1I90^2+(beta1I90*beta2I90-1)^2)) # Eq.(21)
gamma11 = 1                             # Eq.(10)
gamma12 = 0                             # Eq.(11)
(gamma13 = gamma1*(1+beta2I90^2))       # Eq.(12)
(gamma14 = gamma2/abs(beta1I90))        # Eq.(13)
(gamma15 = 0.5*sqrt(beta2I90/beta1I90)) # Eq.(14)
gamma21 = 0                             # Eq.(15)
gamma22 = 1                             # Eq.(16)
gamma23 = (gamma1*(1+beta1I90^2))       # Eq.(17)
gamma24 = (abs(beta1I90)*gamma2)        # Eq.(18)
gamma25 = (sqrt(beta1I90/beta2I90)/2)   # Eq.(19)
x0 = x-xmean; y0 = y-ymean # centered
(VarAlpha1I90 = sum(((y0-beta1I90*x0)-n*xmean*(gamma11/Sxx*
  x0*(y0-beta1I90*x0)+gamma21/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
SigAlpha1I90 = sqrt(VarAlpha1I90)
print(c(round(ub0est,4),round(SigAlpha1I90,4),
        'ub0est, SigAlpha1I90 y on x'))
(VarAlpha2I90 = sum(((y0-beta2I90*x0)-n*xmean*(gamma12/Sxx*
    x0*(y0-beta1I90*x0)+gamma22/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
SigAlpha2I90 = sqrt(VarAlpha2I90)
print(c(round(sdIcYonX1,4),round(SigAlpha2I90,4),
          'sdIcYonX1, SigAlpha2I90, (via) x on y'))
(VarAlpha3I90 = sum(((y0-beta3I90*x0)-n*xmean*(gamma13/Sxx*
  x0*(y0-beta1I90*x0)+gamma23/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
SigAlpha3I90 = sqrt(VarAlpha3I90)
print(c(round(SigAlpha3I90,4),'SigAlpha3I90, bisection'))
(VarAlpha4I90 = sum(((y0-beta4I90*x0)-n*xmean*(gamma14/Sxx*
    x0*(y0-beta1I90*x0)+gamma24/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
SigAlpha4I90 = sqrt(VarAlpha4I90)
print(c(round(SigAlpha4I90,4),'SigAlpha4I90, orthogonal'))
(VarAlpha5I90 = sum(((y0-beta5I90*x0)-n*xmean*(gamma15/Sxx*
    x0*(y0-beta1I90*x0)+gamma25/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
SigAlpha5I90 = sqrt(VarAlpha5I90)
print(c(round(SigAlpha5I90,4),'SigAlpha5I90, geometric'))
print('----------------------------------------------------------')
# (5) Maximum Likelihood Estimator (MLE): Casalla & Berger (2002, Eq. 12.2.16)
# Ratio of error variances: lambda = (sigma_x)^2 / (sigma_y)^2
print('----------------------------------------------------------')
# Lambda set by the data analyst:
lambda = 1/15^2; print(c('lambda = ',round(lambda,6)))
Sxxn=Sxx/n; Syyn = Syy/n; Sxyn = Sxyn = Sxy/n # variances
# -----------------------------------------------------------
MLESlope = (-(Sxxn-lambda*Syyn)+sqrt((Sxxn-lambda*Syyn)^2+
            4*lambda*Sxyn^2))/(2*lambda*Sxyn)
MLEIntercept = mean(y)-MLESlope*mean(x)
print(c(round(MLESlope,3),'MLESlope'))
print(c(round(MLEIntercept,3),'MLEIntercept'))
# --------------------------------------------------------------------------------
# (6) Plot data & lines:
print('----------------------------------------------------------')
# -----------------------------------------------------------------------------
sflag = 1
if (sflag == 1) { # plot geometric line only:
  xp = c(0,4); 
  yp=c(IC5I90,IC5I90+beta5I90*4) # yp=c(b0est,b0est+b1est*4)
  # png('NO3PO4MLE250402.png',width=16,height=16,units='cm',res=300)
  plot(x,y,
       xlab=expression(paste('[',PO[4],'] (',mu,'mol ',L^{-1},')')),
       ylab=NA,type='p',col='black',pch='.',xlim=c(0,4),xaxs='i',
       yaxs='i',las=1,cex.lab=1.5)
  title(ylab=expression(paste('[',NO[3],'] (',mu,'mol ',L^{-1},')')),
        line=2.3,cex.lab=1.5)
  lines(xp,yp,col='blue',lwd=3)
  b0r = round(IC5I90,3); ub0r = round(ub0est,3)
  text(0.01,40,bquote(~hat(beta)[0,geom] == .(b0r) %+-% .(ub0r)),
       col='blue',pos=4,cex.lab=1.4)
  b1r = round(beta5I90,3); ub1r = round(SigAlpha5I90,3)
  text(0.01,48,bquote(~hat(beta)[geom] == .(b1r) %+-% .(ub1r)),
       col='blue',pos=4,cex.lab=1.4)
  # dev.off()
}
print(date())
# --------------------------------------------------------------------
# Remarks:
# Generating the plot might take a few seconds.
# --------------------------------------------------------------------
# [1] "file: RedfieldEIV250404.R"
# [1] "Fri Apr  4 22:12:15 2025"
# [1] "----------------------------------------------------------"
# [1] "(1) load data:"
# [1] "----------------------------------------------------------"
# [1] "----------------------------------------------------------"
# [1] "(2) Remove NAs (NA = not available) and remove high PO4 values"
# [1] "    (above 4 micro-mol/L):"
# [1] "----------------------------------------------------------"
# [1] "----------------------------------------------------------"
# [1] "----------------------------------------------------------"
# [1] "correlation(x,y) = " "0.9828"             
# [1] "-1.7"        "b0est intercept"
# [1] "0.0049"      "ub0est"
# [1] "14.7827"     "b1est slope"
# [1] "0.0026"       "ub1est"
# [1] "----------------------------------------------------------"
# [1] "----------------------------------------------------------"
# [1] "0.1677"      "b2est intercept"
# [1] "0.000303"    "ub2est"  
# [1] "0.0653"      "b3est slope x on y"
# [1] "1.1e-05"     "ub3est" 
# [1] "15.3062"     "slope2 (via) x on y" 
# [1] "-2.5668"     "ic2"    
# [1] "----------------------------------------------------------"
# [1] " Estimate uncertainties: Monte Carlo"
# [1] "----------------------------------------------------------"
# [1] "15.30638" "+-" "0.00264"   "mrSlopeYonX1 (median +- MADN)"
# [1] "-2.56677" "+-" "0.00487"   "mrIcYonX1 (median +- MADN)"
# [1] "15.3062" "SlopeYonX1 (via) x on y"
# [1] "-2.5668" "IcYonX1"
# [1] "----------------------------------------------------------"
# [1] " Isobe et al. (1990): slopes & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] "----------------------------------------------------------"
# [1] "14.7827"         "beta1I90 y on x"
# [1] "15.3062"         "beta2I90 (via) x on y"
# [1] "15.0399"         "beta3I90 bisector"
# [1] "15.3039"         "beta4I90 orthogonal"
# [1] "15.0422"         "beta5I90 geometric"
# [1] "5.62e-06"        "Var1I90 y on x"
# [1] "8.14e-06"        "Var2I90 (via) x on y"
# [1] "5.73e-06"        "Var3I90 bisector"
# [1] "8.12e-06"        "Var4I90 orthogonal"
# [1] "5.75e-06"        "Var5I90 geometric"
# [1] "4.65e-06"        "Cov12I90"
# [1] "0.00237"         "sig1I90 y on x"
# [1] "0.00285"         "sig2I90 (via) x on y"
# [1] "0.00239"         "sig3I90 bisection"
# [1] "0.00285"         "sig4I90 orthogonal"
# [1] "0.0024"          "sig5I90 geometric"
# [1] "----------------------------------------------------------"
# [1] " Isobe et al. (1990): intercepts & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] "----------------------------------------------------------"
# [1] "-1.7"   "-1.7"    "b0est,IC1I90 y on x"
# [1] "-2.567" "-2.567"  "IcYonX1,IC2I90 (via) x on y"
# [1] "-2.126"           "IC3I90 bisection"
# [1] "-2.563"           "IC4I90 orthogonal"
# [1] "-2.13"            "IC5I90 geometric"
# [1] "0.0049"  "0.0047"  "ub0est,    SigAlpha1I90 y on x"
# [1] "0.0049"  "0.0061"  "sdIcYonX1, SigAlpha2I90, (via) x on y"
# [1] "0.0051"                       "SigAlpha3I90, bisection"
# [1] "0.006"                        "SigAlpha4I90, orthogonal"
# [1] "0.0051"                       "SigAlpha5I90, geometric"
# [1] "----------------------------------------------------------"
# [1] "----------------------------------------------------------"
# [1] "lambda = " "0.004444" 
# [1] "15.043"    "MLESlope"
# [1] "-2.131"    "MLEIntercept"
# [1] "----------------------------------------------------------"
# [1] "Fri Apr  4 22:12:17 2025"
