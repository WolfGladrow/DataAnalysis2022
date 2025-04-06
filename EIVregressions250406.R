print('file: EIVregressions2504.R')
print(date())
# history: R_Jitjareonchai06MCMC2503.R
# purpose: Errors-In-Variables (EIV)
# analyse example Jitjareonchai et al. (2006) using 
# regressions or regression-based methods
# created by: Dieter.Wolf-Gladrow@awi.de 
#    4/2025 version 1.0
# update: 3/2025 Isobe et al. (1990) estimates 
# --------------------------------------------------------------------------------------------------
print(' ---------------------------------------------------')
print('(1) Jitjareonchai et al. (2006, Table 2): true model parameters & data')
print(' ---------------------------------------------------')
betaTrue = 3     # Table 2 (notation: alpha*)
beta0True = 10   # Table 2 (notation: beta*)
print(c(betaTrue,'true slope, betaTrue = alpha*'))
print(c(beta0True,'true intercept, beta0True = beta*'))
varxTrue=4; varyTrue=9    # Table 2
print(c(varxTrue,varyTrue,'varxTrue,varyTrue'))
xiiTrue = c(18.3075,27.4873,25.3590,29.8017,14.2793,
            16.4007,18.2391,15.3589,26.7848,12.6755)
yiiTrue = c(64.9225,92.4619,86.0770,99.4051,52.8379,
            59.2021,64.7173,56.0767,90.3544,48.0265)
n = length(xiiTrue); print(c(n,'sample size n')) 
xiiTrueMean = mean(xiiTrue); yiiTrueMean = mean(yiiTrue); 
print(c(round(xiiTrueMean,4),round(yiiTrueMean,4),
        'xiiTrueMean,yiiTrueMean'))
print(' ---------------------------------------------------')
print('xii from normal PDF? Shapiro-Wilk test')
print(' ---------------------------------------------------')
testN = shapiro.test(xiiTrue)
pvalue = testN$p.value
print(c(round(pvalue,4),'pvalue'))
if (pvalue < 0.05) {
  print('normality rejected on alpha = 0.05 significance level')
}
if (pvalue >= 0.05) {
  print('normality not rejected on alpha = 0.05 significance level')
}
print(' ---------------------------------------------------')
print('Raw data (xi,yi), i = 1,2,...,n (Table 2):')
print(' ---------------------------------------------------')
x = c(18.6369,25.8483,25.0829,30.2416,14.7174,
      14.2656,17.2711,14.0370,28.1420,12.2802)
y = c(70.5998,96.9826,89.3344,99.3439,55.9705,
      57.3196,65.9757,55.7193,87.9387,54.2554) # raw data
print(' ---------------------------------------------------')
print('xi from normal PDF? Shapiro-Wilk test')
print(' ---------------------------------------------------')
testNx = shapiro.test(x)
pvaluex = testNx$p.value
print(c(round(pvaluex,4),'pvaluex'))
if (pvaluex < 0.05) {
  print('normality rejected on alpha = 0.05 significance level')
}
if (pvaluex >= 0.05) {
  print('normality not rejected on alpha = 0.05 significance level')
}
print(' ---------------------------------------------------')
print('ximean ... true lambda:')
print(' ---------------------------------------------------')
xmean = mean(x); print(c(round(xmean,4),round(xiiTrueMean,4),
                         'xmean,xiiTrueMean'))
ymean = mean(y); print(c(round(ymean,4),round(yiiTrueMean,4),
                         'ymean,yiiTrueMean'))
(SoSxx = sum((x-xiiTrueMean)^2))
(SoSyy = sum((y-yiiTrueMean)^2))
(SoSxy = sum((x-xiiTrueMean)*(y-yiiTrueMean)))
print(c(round(SoSxx,4),round(SoSyy,4),round(SoSxy,4),'SoSxx,SoSyy,SoSxy'))
lambdaTrue = varxTrue/varyTrue # ratio of error variances 
                  # (defined according to Casella & Berger, 2002)
print(c(round(lambdaTrue,4),'lambdaTrue'))
print(' ---------------------------------------------------')
print('Sum of squares:')
print(' ---------------------------------------------------')
(Sxx = sum((x-xmean)^2))
(Syy = sum((y-ymean)^2)) 
(Sxy = sum((x-xmean)*(y-ymean)))
print(c(round(Sxx,4),round(Syy,4),round(Sxy,4),'Sxx,Syy,Sxy'))
print(' ---------------------------------------------------')
print('Diagnostic: mean distances of x-values (begin):')
print(' ---------------------------------------------------')
sx = sort(x); xDiff = numeric(n-1)
for(j in 1:(n-1)) xDiff[j] = sx[j+1]-sx[j]
meanxDiff = mean(xDiff)
medianxDiff = median(xDiff)
print(c(round(meanxDiff,4),round(medianxDiff,4),'meanxDiff,medianxDiff'))
print(' ---------------------------------------------------')
print('Diagnostic: mean distances of x-values (end)')
print(' ---------------------------------------------------')
print(' ---------------------------------------------------')
print(' ---------------------------------------------------')
print('(2) Simple linear regression (SLR): y on x')
print(' ---------------------------------------------------')
SLRyonx = summary(lm(y ~ x))
(IcYonX    = as.numeric(SLRyonx$coefficients[1]))
(SlopeYonX = as.numeric(SLRyonx$coefficients[2]))
(uIcYonX    = as.numeric(SLRyonx$coefficients[3]))
(uSlopeYonX = as.numeric(SLRyonx$coefficients[4]))
b0est = IcYonX; b1est = SlopeYonX
ub0est = uIcYonX; ub1est = uSlopeYonX
print(c(betaTrue,round(SlopeYonX,4),'+-',round(uSlopeYonX,4),
        'betaTrue,SlopeYonX'))
print(c(beta0True,round(IcYonX,4),'+-',round(uIcYonX,4),
        'beta0True,IcYonX'))
(RsqSLR = SLRyonx$r.squared)
print(c(round(RsqSLR,4),'RsqSLR'))
print(' ---------------------------------------------------')
print(' (3)  Simple linear regression (SLR): via x on y')
print(' ---------------------------------------------------')
SLRxony = summary(lm(x ~ y))
a = as.numeric(SLRxony$coefficients[1])
b = as.numeric(SLRxony$coefficients[2])
ua = as.numeric(SLRxony$coefficients[3])
ub = as.numeric(SLRxony$coefficients[4])
print(c(round(a,4),'+-',round(ua,4),'a +- ua'))
print(c(round(b,4),'+-',round(ub,4),'b +- ub'))
# x=b*y+a -> y = (x-a)/b -> slope = 1/b; 
SlopeYonX1 = 1/b
IcYonX1 = -a/b
# Propagation of uncertainty (Monte Carlo):
Mr = 1000 # number of Monte Carlo values
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
print(c(betaTrue,round(mrSlopeYonX1,4),'+-',
        round(sdSlopeYonX1,4),'betaTrue,mrSlopeYonX1 (median,MADN)'))
print(c(beta0True,round(mrIcYonX1,4),'+-',
        round(sdIcYonX1,4),'beta0True,mrIcYonX1 (median,MADN)'))
print(c(betaTrue,round(SlopeYonX1,4),'betaTrue,SlopeYonX1'))
print(c(beta0True,round(IcYonX1,4),'beta0True,IcYonX1'))
print(' ---------------------------------------------------')
print('(4) Isobe et al. (1990): regression-based methods')
print(' ---------------------------------------------------')
print('(4a) Isobe et al. (1990) y on x etc.:')
print(' ---------------------------------------------------')
(beta1I90 = Sxy/Sxx) # y on x
(beta2I90 = Syy/Sxy) # via x on y
(beta3I90 = 1/(beta1I90+beta2I90)*(beta1I90*beta2I90-1+
              sqrt((1+beta1I90^2)*(1+beta2I90^2)))) # bisection
(beta4I90 = ((beta2I90-1/beta1I90)+sign(Sxy)*
               sqrt(4+(beta2I90-1/beta1I90)^2))/2) # orthogonal
(beta5I90 = sign(Sxy)*sqrt(beta1I90*beta2I90)) # geometric
print('Variances of slope estimates:')
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
print(c(round(Var1I90,5),'Var1I90 y on x'))
print(c(round(Var2I90,5),'Var2I90 via x on y'))
print(c(round(Var3I90,5),'Var3I90 bisection'))
print(c(round(Var4I90,5),'Var4I90 orthogonal'))
print(c(round(Var5I90,5),'Var5I90 geometric'))
print(c(round(Cov12I90,5),'Cov12I90'))
print('uncertainties (= standard errors):')
(SE1I90 = sqrt(Var1I90))
(SE2I90 = sqrt(Var2I90))
(SE3I90 = sqrt(Var3I90))
(SE4I90 = sqrt(Var4I90))
(SE5I90 = sqrt(Var5I90))
# print results:
print(c(betaTrue,round(beta1I90,4),'+-',round(SE1I90,4),
        'betaTrue,beta1I90+-SE y on x'))
print(c(betaTrue,round(beta2I90,4),'+-',round(SE2I90,4),
        'betaTrue,beta2I90+-SE via x on y'))
print(c(betaTrue,round(beta3I90,4),'+-',round(SE3I90,4),
        'betaTrue,beta3I90+-SE bisection'))
print(c(betaTrue,round(beta4I90,4),'+-',round(SE4I90,4),
        'betaTrue,beta4I90+-SE orthogonal'))
print(c(betaTrue,round(beta5I90,4),'+-',round(SE5I90,4),
        'betaTrue,beta5I90+-SE geometric'))
print('uncertainties (= standard deviations):')
print(' ---------------------------------------------------')
print('Bisection:')
print(' ---------------------------------------------------')
(angle1 = atan(beta1I90))    # 1.214322
(angle2 = atan(beta2I90))    # 1.229678
(angle3 = (angle1+angle2)/2) # 1.222
(beta3I90b = tan(angle3))    # 2.749784
print(c(round(beta3I90b,4),round(beta3I90,4),
        'beta3I90b,beta3I90'))
print(' ---------------------------------------------------')
print(' Isobe et al. (1990): intercepts & their uncertainties')
print('  uncertainty = standard error, i.e. proportional 1/sqrt(n)')
print(' ---------------------------------------------------')
(IC1I90 = ymean-beta1I90*xmean)
(IC2I90 = ymean-beta2I90*xmean)
(IC3I90 = ymean-beta3I90*xmean)
(IC4I90 = ymean-beta4I90*xmean)
(IC5I90 = ymean-beta5I90*xmean)
print(c(round(b0est,4),round(IC1I90,4),'b0est,IC1I90 y on x'))
print(c(round(IC2I90,4),'IC2I90 via x on y'))
print(c(round(IC3I90,4),'IC3I90 bisection'))
print(c(round(IC4I90,4),'IC4I90 orthogonal'))
print(c(round(IC5I90,4),'IC5I90 geometric'))
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
x0 = x-xmean; y0 = y-ymean # trial
(VarAlpha1I90 = sum(((y0-beta1I90*x0)-n*xmean*(gamma11/Sxx*
    x0*(y0-beta1I90*x0)+gamma21/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
print(c(round(VarAlpha1I90,4),'VarAlpha1I90'))
SigAlpha1I90 = sqrt(VarAlpha1I90)
print(c(round(ub0est,4),round(SigAlpha1I90,4),'ub0est, SigAlpha1I90'))
(VarAlpha2I90 = sum(((y0-beta2I90*x0)-n*xmean*(gamma12/Sxx*
      x0*(y0-beta1I90*x0)+gamma22/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
print(c(round(VarAlpha2I90,4),'VarAlpha2I90'))
SigAlpha2I90 = sqrt(VarAlpha2I90)
print(c(round(sdIcYonX1,4),round(SigAlpha2I90,4),
          'sdIcYonX1, SigAlpha2I90,x on y'))
(VarAlpha3I90 = sum(((y0-beta3I90*x0)-n*xmean*(gamma13/Sxx*
    x0*(y0-beta1I90*x0)+gamma23/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
print(c(round(VarAlpha3I90,4),'VarAlpha3I90'))
SigAlpha3I90 = sqrt(VarAlpha3I90)
print(c(round(SigAlpha3I90,4),'SigAlpha3I90, bisection'))
(VarAlpha4I90 = sum(((y0-beta4I90*x0)-n*xmean*(gamma14/Sxx*
    x0*(y0-beta1I90*x0)+gamma24/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
print(c(round(VarAlpha4I90,4),'VarAlpha4I90'))
SigAlpha4I90 = sqrt(VarAlpha4I90)
print(c(round(SigAlpha4I90,4),'SigAlpha4I90, orthogonal'))
(VarAlpha5I90 = sum(((y0-beta5I90*x0)-n*xmean*(gamma15/Sxx*
    x0*(y0-beta1I90*x0)+gamma25/Sxy*y0*(y0-beta2I90*x0)))^2)/n^2)
print(c(round(VarAlpha5I90,4),'VarAlpha5I90'))
SigAlpha5I90 = sqrt(VarAlpha5I90)
print(c(round(SigAlpha5I90,4),'SigAlpha5I90, geometric'))
print('----------------------------------------------------')
print('(5) Plots:')
print('----------------------------------------------------')
library(latex2exp)
# --------------------------------------------------------------
sflag = 0
if (sflag == 1) {  # plot data & lines
   xp = c(min(x,0),max(x))
    ypYonX = SlopeYonX*xp+IcYonX
    ypYonX1 = SlopeYonX1*xp+IcYonX1
    ypBis = beta3I90*xp+IC3I90
    ypGeo = beta5I90*xp+IC5I90
    ypTrue = betaTrue*xp+beta0True
# png('Jitjareonchai06Lines250406a.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='black',xlab='x',ylab='y',las=1,
     cex=0.6,cex.lab=1.5,
         xlim=c(min(c(x,0)),max(x)),
         ylim=c(min(c(y,ypYonX1,0)),max(y)*1.2))
    # ylim=c(min(ypXonY),max(ypXonY))) xlim=c(0,17),
    lines(xp,ypYonX,col='orange',lwd=4,lty=2)
    lines(xp,ypYonX1,col='red',lty=4)
    # lines(xp,ypXonY,col='red',lty=4)
    lines(xp,ypTrue,lwd=2,col='black')
    lines(xp,ypBis,lwd=2,col='magenta',lty=1)
    lines(xp,ypGeo,lwd=2,col='blue',lty=4)
    # dev.off()
  } # end of sflag = 1
if (sflag == 2) {  # plot data & lines
  xp = c(min(x),max(x))
  ypYonX = SlopeYonX*xp+IcYonX
  ypYonX1 = SlopeYonX1*xp+IcYonX1
  ypBis = beta3I90*xp+IC3I90
  ypGeo = beta5I90*xp+IC5I90
  ypTrue = betaTrue*xp+beta0True
  # png('Jitjareonchai06Lines250406b.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='black',xlab='x',ylab='y',las=1,
       cex=0.6,cex.lab=1.5,
       xlim=c(min(c(x)),max(x)),
       ylim=c(min(c(y,ypYonX1))*0.9,max(y)*1.05))
  # ylim=c(min(ypXonY),max(ypXonY))) xlim=c(0,17),
  lines(xp,ypYonX,col='orange',lwd=4,lty=2)
  lines(xp,ypYonX1,col='red',lty=4)
  # lines(xp,ypXonY,col='red',lty=4)
  lines(xp,ypTrue,lwd=2,col='black')
  lines(xp,ypBis,lwd=2,col='magenta',lty=1)
  lines(xp,ypGeo,lwd=2,col='blue',lty=4)
  # dev.off()
} # end of sflag = 2
# [1] "file: EIVregressions2504.R"
# [1] "Sun Apr  6 10:44:21 2025"
# [1] " ---------------------------------------------------"
# [1] "(1) Jitjareonchai et al. (2006, Table 2): true model parameters & data"
# [1] " ---------------------------------------------------"
# [1] "3"     "true slope, betaTrue = alpha*"
# [1] "10"    "true intercept, beta0True = beta*"
# [1] "4" "9" "varxTrue,varyTrue"
# [1] "10"    "sample size n"
# [1] "20.4694"   "71.4081"    "xiiTrueMean,yiiTrueMean"
# [1] " ---------------------------------------------------"
# [1] "xii from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.1857" "pvalue"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] " ---------------------------------------------------"
# [1] "Raw data (xi,yi), i = 1,2,...,n (Table 2):"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "xi from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.158"   "pvaluex"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] " ---------------------------------------------------"
# [1] "ximean ... true lambda:"
# [1] " ---------------------------------------------------"
# [1] "20.0523"           "20.4694"           "xmean,xiiTrueMean"
# [1] "73.344"            "71.4081"           "ymean,yiiTrueMean"
# [1] "398.18" "3036.3986" "1056.5316"        "SoSxx,SoSyy,SoSxy"
# [1] "0.4444" "lambdaTrue"       
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "396.4405"    "2998.9234"   "1064.6057"   "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (begin):"
# [1] " ---------------------------------------------------"
# [1] "1.9957"  "1.7568"      "meanxDiff,medianxDiff"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (end)"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(2) Simple linear regression (SLR): y on x"
# [1] " ---------------------------------------------------"
# [1]  "3"  "2.6854" "+-" "0.2101"    "betaTrue,SlopeYonX"
# [1] "10" "19.4953" "+-" "4.4161"    "beta0True,IcYonX"
# [1] "0.9533" "RsqSLR"
# [1] " ---------------------------------------------------"
# [1] " (3)  Simple linear regression (SLR): via x on y"
# [1] " ---------------------------------------------------"
# [1] "-5.9845" "+-"      "2.0932"  "a +- ua"
# [1]  "0.355"  "+-"      "0.0278"  "b +- ub"
# [1]  "3"  "2.8213" "+-" "0.2271"  "betaTrue,mrSlopeYonX1 (median,MADN)"
# [1] "10" "16.7368" "+-" "6.2755"  "beta0True,mrIcYonX1 (median,MADN)"
# [1]  "3"  "2.8169" "betaTrue,SlopeYonX1"
# [1] "10" "16.858"  "beta0True,IcYonX1"
# [1] " ---------------------------------------------------"
# [1] "(4) Isobe et al. (1990): regression-based methods"
# [1] " ---------------------------------------------------"
# [1] "(4a) Isobe et al. (1990) y on x etc.:"
# [1] " ---------------------------------------------------"
# [1] "Variances of slope estimates:"
# [1] "0.04078"        "Var1I90 y on x"
# [1] "0.04824"        "Var2I90 via x on y"
# [1] "0.04325"        "Var3I90 bisection"
# [1] "0.04778"        "Var4I90 orthogonal"
# [1] "0.04331"        "Var5I90 geometric"
# [1] "0.04224"        "Cov12I90"
# [1] "uncertainties (= standard errors):"
# [1] "3" "2.6854"  "+-"  "0.2019"  "betaTrue,beta1I90+-SE y on x"
# [1] "3" "2.8169"  "+-"  "0.2196"  "betaTrue,beta2I90+-SE via x on y"
# [1] "3" "2.7498"  "+-"  "0.208"   "betaTrue,beta3I90+-SE bisection"
# [1] "3" "2.8015"  "+-"  "0.2186"  "betaTrue,beta4I90+-SE orthogonal"
# [1] "3" "2.7504"  "+-"  "0.2081"                         
# [5] "betaTrue,beta5I90+-SE geometric"
# [1] "uncertainties (= standard deviations):"
# [1] " ---------------------------------------------------"
# [1] "Bisection:"
# [1] " ---------------------------------------------------"
# [1] "2.7498"  "2.7498"  "beta3I90b,beta3I90"
# [1] " ---------------------------------------------------"
# [1] " Isobe et al. (1990): intercepts & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] " ---------------------------------------------------"
# [1] "19.4953"   "19.4953"             "b0est,IC1I90 y on x"
# [1] "16.858"    "IC2I90 via x on y"
# [1] "18.2045"   "IC3I90 bisection"
# [1] "17.1674"   "IC4I90 orthogonal"
# [1] "18.1924"   "IC5I90 geometric"
# [1] "10.2248"   "VarAlpha1I90"
# [1]  "4.4161" "3.1976" "ub0est, SigAlpha1I90"
# [1] "12.4811"      "VarAlpha2I90"
# [1] "6.2755"  "3.5329"  "sdIcYonX1, SigAlpha2I90, x on y"
# [1] "10.7801"      "VarAlpha3I90"
# [1]  "3.2833"        "SigAlpha3I90, bisection"
# [1] "12.2725"      "VarAlpha4I90"
# [1]  "3.5032"        "SigAlpha4I90, orthogonal"
# [1] "10.7985"      "VarAlpha5I90"
# [1]  "3.2861"        "SigAlpha5I90, geometric"
# [1] "----------------------------------------------------"
# [1] "(5) Plots:"
# [1] "----------------------------------------------------"