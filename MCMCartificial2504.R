print('file: MCMCartificial2504.R')
print(date())
# history: MCMCartificial2412
# purpose: Errors-In-Variables (EIV)
#    analysis 4 artificial data set using SLR to MCMC
# created by: Dieter.Wolf-Gladrow@awi.de 
# updates:
#     4/2025 version 2.0: include bisection
#    12/2024 version 1.0
# --------------------------------------------------------------------------------------------------
print(' ---------------------------------------------------')
print('(1) Generate artificial data:')
# print(' ---------------------------------------------------')
set.seed(1953)
betaTrue = 3; beta0True = 2.5;           # for all data sets
sigxTrue = 1; sigyTrue = 1.5; ximean = 3 # for all data sets
DataSet = 1;  print(c('DataSet ',DataSet))
if (DataSet == 1) {n=24; xisigma = 1}
if (DataSet == 2) {n=24; xisigma = 2} # more spread
if (DataSet == 3) {n=48; xisigma = 1} # more data, less spread again
if (DataSet == 4) {n=48; xisigma = 4} # more data, more spread
varxTrue = sigxTrue^2; varyTrue = sigyTrue^2
print(c(n,'n sample size'))
print(c(betaTrue,beta0True,'betaTrue,beta0True'))
print(c(sigxTrue,sigyTrue,'sigxTrue,sigyTrue'))
lambdaTrue = (sigxTrue/sigyTrue)^2 # ratio of error variances 
# (def. according to Casella & Berger, 2002)
print(c(round(lambdaTrue,4),'lambdaTrue'))
# (1a) true xi values from a normal distribution:
xiiTrue = rnorm(n,ximean,xisigma)
print(c(ximean,xisigma,'ximean,xisigma'))
print(c(round(min(xiiTrue),2),round(max(xiiTrue),2),'min(xi),max(xi)'))
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
# (1b) yiiTrue: true gamma values
yiiTrue = betaTrue*xiiTrue+beta0True
print(c(round(min(yiiTrue),2),round(max(yiiTrue),2),'min(yiiTrue),max(yiiTrue)'))
# (1c) add normal noise (independent from each other) with mean 0:
errx = rnorm(n,0,sigxTrue); erry = rnorm(n,0,sigyTrue)
x = xiiTrue+errx   # 'observed' x values = raw data
y = yiiTrue+erry   # 'observed' y values = raw data
varxTrue = sigxTrue^2; varyTrue = sigyTrue^2
print(c(varxTrue,varyTrue,'varxTrue,varyTrue'))
print(' ---------------------------------------------------')
print('x from normal PDF? Shapiro-Wilk test')
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
xmean = mean(x); print(c(round(xmean,4),'xmean'))
ymean = mean(y); print(c(round(ymean,4),'ymean'))
SoSx = sum((x-xiiTrue)^2); SoSy = sum((y-yiiTrue)^2)
print(c(round(SoSx,1),round(SoSy,1),'SoSx,SoSy'))
lambdaTrue = varxTrue/varyTrue # ratio of error variances (defined according to Casella & Berger, 2002)
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
        'sdIcYonX1, SigAlpha2I90, via x on y'))
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
print(' ---------------------------------------------------')
print('MCMC:')
print(' ---------------------------------------------------')
MCMCflag = 1
if (MCMCflag == 1) {
  print('----------------------------------------------------')
  print('(7) MCMC pedestrian:') 
  print('----------------------------------------------------')
  # Sum of squares / sigma^2 follows a chi-squred PDF with n degrees   DROP THESE LINES
  # of freedom; the mode of this chi-squred PDF 
  # is located at n-2 (for n > 2) -> choose prior values for sum of squares for 
  # x that are away from the mode
  print('1. Set t=0 and select a set of initial/starting parameter')
  print('   values beta, beta0, varx, vary:')
  t=0
  betaStart = 0.9*betaTrue   # significantly different from true value
  beta0Start = 0.5*beta0True # significantly different from true value
  M = 1e6 # same as Jitjareonchai et al. (2006, Table 3) 
  print(c(M,'M number of Monte Carlo runs/cycles')) 
  beta = betaStart; beta0 = beta0Start
  print(c(betaTrue,round(betaStart,2),'betaTrue,betaStart'))
  print(c(beta0True,round(beta0Start,2),'beta0True,beta0Start'))
  k = n     # degrees of freedom for sum of squares; 
  # different from Jitjareonchai et al. (2006, p.129) who use k=4
  nu = n+k  # degrees of freedom for chisq prior
  print(c(k,nu,'k,nu'))
  # -------------------------------------------------------------
  # Start values for varx, vary not specified in Jitjareonchai et al. (2006)!!!
  # -> the following values are my (DWG) choice
  sigxStart = sigxTrue*0.1; sigyStart = sigyTrue*0.1 # tiny 'anchors'
  varx = sigxStart^2; vary = sigyStart^2 
  Px = (nu-2)*varx; Py = (nu-2)*vary 
  print(c(varxTrue,round(varx,4),'varxTrue,varx (prior value)'))
  print(c(varyTrue,round(vary,4),'varyTrue,vary (prior value)'))
  print(c(round(Px,4),round(Py,4),'Px, Py (prior value)'))
  print(' ---------------------------------------------------')
  print('Define arrays to store MCMC time series')
  print(' ---------------------------------------------------')
  betaArr = numeric(M); beta0Arr = numeric(M)
  varxArr = numeric(M); varyArr = numeric(M)
  xiiEst = matrix(data=NA,nrow=M,ncol=n)
  yiiEst = matrix(data=NA,nrow=M,ncol=n)
  print(' ---------------------------------------------------')
  print('MCMC iterations start:')
  print(' ---------------------------------------------------')
  xii = numeric(n)
  for(t in 1:M) {
    # print(' ---------------------------------------------------')
    # print('2. Sample each element of xi_t+1 from Eq. A-3:')
    # print(' ---------------------------------------------------')
    A3sd = sqrt(1/(1/varx+beta^2/vary))
    for(i in 1:n) {
      A3mean =(x[i]*vary+beta*varx*(y[i]-beta0))/(vary+beta^2*varx)
      xii[i] = rnorm(1,A3mean,A3sd) # random sample from normal PDF
      xiiEst[t,i] = xii[i]                         # store time series values
      yiiEst[t,i] = beta*xii[i]+beta0     # store time series values
    }
    # print(' ---------------------------------------------------')
    # print('3. Sample beta_t+1 from A-6:')
    # print(' ---------------------------------------------------')
    sum1 = sum(xii*(y-beta0)); sum2 = sum(xii^2)
    A6mean = sum1/sum2; A6sd = sqrt(vary/sum2)
    beta = rnorm(1,A6mean,A6sd); betaArr[t] = beta
    # print(' ---------------------------------------------------')
    # print('4. Sample beta0_t+1 from A-8:')
    # print(' ---------------------------------------------------')
    A8mean = sum(y-beta*xii)/n; A8sd = sqrt(vary/n)
    beta0 = rnorm(1,A8mean,A8sd); beta0Arr[t] = beta0
    # print(' ---------------------------------------------------')
    # print('5. Sample varx_t+1 from A-12:')
    # print(' ---------------------------------------------------')
    Sx = sum((x-xii)^2); varx = (Px+Sx)/rchisq(1,df=nu); varxArr[t] = varx
    # print(' ---------------------------------------------------')
    # print('6. Sample vary_t+1 from A-13:')
    # print(' ---------------------------------------------------')
    Sy = sum((y-beta*xii-beta0)^2); vary = (Py+Sy)/rchisq(1,df=nu); varyArr[t] = vary
  } # end of t-loop
  print(' ---------------------------------------------------')
  print('Analyze MCMC time series:')
  print(' ---------------------------------------------------')
  print('(A) All time series data:')
  print(' ---------------------------------------------------')
  betaArrMean = mean(betaArr); betaArrSD = sd(betaArr)
  beta0ArrMean = mean(beta0Arr); beta0ArrSD = sd(beta0Arr)
  print(c(betaTrue,round(betaArrMean,4),'+-',round(betaArrSD,4),
          'betaTrue,betaArrMean'))
  print(c(beta0True,round(beta0ArrMean,4),'+-',round(beta0ArrSD,4),
          'beta0True,beta0ArrMean'))
  print(' ---------------------------------------------------')
  print('(B) Cut out first 1.5% of time series (burn-in region):')
  print(' ---------------------------------------------------')
  m1 = round(0.015*M)+1; print(c(m1,'m1 (cut out + 1)'))
  betaArrMean = mean(betaArr[m1:M]); betaArrSD = sd(betaArr)
  beta0ArrMean = mean(beta0Arr[m1:M]); beta0ArrSD = sd(beta0Arr)
  print(c(betaTrue,round(betaArrMean,4),'+-',round(betaArrSD,4),
          'betaTrue,betaArrMean'))
  print(c(beta0True,round(beta0ArrMean,4),'+-',round(beta0ArrSD,4),
          'beta0True,beta0ArrMean'))
  betaArrMedian = median(betaArr[m1:M])
  beta0ArrMedian = median(beta0Arr[m1:M])
  print(c(betaTrue,round(betaArrMedian,4),
          'betaTrue,betaArrMedian'))
  print(c(beta0True,round(beta0ArrMedian,4),
          'beta0True,beta0ArrMedian'))
  # ---------------------------------------
  varxArrMean = mean(varxArr[m1:M]); varxArrSD = sd(varxArr[m1:M])
  varyArrMean = mean(varyArr[m1:M]); varyArrSD = sd(varyArr[m1:M])
  print(c(varxTrue,round(varxArrMean,4),'+-',round(varxArrSD,4),
          'varxTrue,varxMCMC'))
  print(c(varyTrue,round(varyArrMean,4),'+-',round(varyArrSD,4),
          'varyTrue,varyMCMC'))
  varxArrMedian = median(varxArr[m1:M]); varyArrMedian = median(varyArr[m1:M])
  print(c(round(varxArrMedian,4),round(varyArrMedian,4),
          'varxMedian,varyMedian'))
  LambdaMCMC = varxArrMean/varyArrMean
  print(c(round(lambdaTrue,4),round(LambdaMCMC,4),'lambdaTrue, LambdaMCMC'))
  xiEst = numeric(n); xiiMedian = numeric(n)
  for(i in 1:n) {
    xiEst[i] = mean(xiiEst[m1:M,i])
    xiiMedian[i] = median(xiiEst[m1:M,i])
  }
  yiEst = numeric(n); yiiMedian = numeric(n)
  for(i in 1:n) {
    yiEst[i] = mean(yiiEst[m1:M,i])
    yiiMedian[i] = median(yiiEst[m1:M,i])
  }
  print(c(round(xiEst,4),'xiEst'))
  print(c(round(yiEst,4),'yiEst'))
  print(c(round(xiiMedian,4),'xiiMedian'))
  print(c(round(yiiMedian,4),'yiiMedian'))
  print(' ---------------------------------------------------')
  print('(8) Mean squared error (MSE):')
  print(' ---------------------------------------------------')
  MSEx = sum((xiiTrue-x)^2)/n                        # raw data
  print(c(round(MSEx,4),'MSEx'))
  MSEy = sum((yiiTrue-y)^2)/n
  print(c(round(MSEy,4),'MSEy'))
  # ---
  MSExi = sum((xiiTrue-xiEst)^2)/n
  print(c(round(MSExi,4),'MSExi'))
  MSEyi = sum((yiiTrue-yiEst)^2)/n
  print(c(round(MSEyi,4),'MSEyi'))
print('----------------------------------------------------')
print('(9) Plots (only possible when running MCMC; switch on by setting sflag values):')
print('----------------------------------------------------')
library(latex2exp)
# ---------------------------------------------------------------------------------------------------------
sflag = 1
# --------------------------------------------------------------
if (sflag == 1) { # plot data & lines
print('----------------------------------------------------')
print('(P1) Plot data & estimated lines:')
print('----------------------------------------------------')
xp = c(min(x),max(x))
ypYonX = SlopeYonX*xp+IcYonX
ypYonX1 = SlopeYonX1*xp+IcYonX1
ypGeo = beta5I90*xp+IC5I90
ypTrue = betaTrue*xp+beta0True
ypMCMC = betaArrMean*xp+beta0ArrMean
# png('MCMCFig1DS1x250406.png',width=16,height=16,units='cm',res=300)
# png('MCMCFig1DS2x250406.png',width=16,height=16,units='cm',res=300)
# png('MCMCFig1DS3x250406.png',width=16,height=16,units='cm',res=300)
# png('MCMCFig1DS4x250406.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(1,1))
plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,
         cex.lab=1.5,
         xlim=c(min(c(x,0)),max(x)),
         ylim=c(min(c(y,ypYonX1,0)),max(y)*1.2))
lines(xp,ypYonX,col='orange',lwd=4,lty=2)
lines(xp,ypYonX1,col='red',lty=4)
lines(xp,ypGeo,lwd=2,col='magenta',lty=1)
lines(xp,ypTrue,lwd=2,col='black')
lines(xp,ypMCMC,lwd=2,col='green')
# dev.off()
} # end of sflag = 1
# -------------------------------------------------------------
sflag = 0
if (sflag == 2) {
print('----------------------------------------------------')
print('(P2) Plot complete times series:')
print('----------------------------------------------------')
# png('MCMCFig2DS1x250408c.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(2,1))
plot(betaArr,type='p',lwd=1,col='blue',xlab='Cycle',ylab='Slope',
         las=1,cex=0.1,cex.lab=1.5)
abline(h=betaTrue,col='black',lty=1)
abline(h=betaArrMean,col='yellow',lty=1)
abline(h=betaArrMean+betaArrSD,col='yellow',lty=2)
abline(h=betaArrMean-betaArrSD,col='yellow',lty=2)
plot(beta0Arr,type='p',lwd=1,col='blue',xlab='Cycle',ylab='Intercept',
         las=1,cex=0.1,cex.lab=1.5)
abline(h=beta0True,col='black',lty=1)
abline(h=beta0ArrMean,col='yellow',lty=1)
abline(h=beta0ArrMean+beta0ArrSD,col='yellow',lty=2)
abline(h=beta0ArrMean-beta0ArrSD,col='yellow',lty=2)
# dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 3) {
print('----------------------------------------------------')
print('(P3) Plot confidence ellipses:')
print('----------------------------------------------------')
library(ConfidenceEllipse)
Mshort = M # use all (M) or fewer data
xEll = betaArr[m1:Mshort]; yEll = beta0Arr[m1:Mshort]; Ell=data.frame(xEll,yEll)
names(Ell) # "xEll" "yEll"
ellipse75=confidence_ellipse(Ell,x=xEll,y=yEll,conf_level=0.75)
ellipse90=confidence_ellipse(Ell,x=xEll,y=yEll,conf_level=0.9)
ellipse95=confidence_ellipse(Ell,x=xEll,y=yEll,conf_level=0.95)
# png('MCMCFig5DS1x250408.png',width=16,height=16,units='cm',res=300)
# png('MCMCFig5DS2x250408.png',width=16,height=16,units='cm',res=300)
# png('MCMCFig5DS3x250408.png',width=16,height=16,units='cm',res=300)
# png('MCMCFig5DS4x250408.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(1,1))
if (DataSet == 1) {
  plot(ellipse95,type='l',lwd=2,col='black',xlab=NA,ylab=NA,
       las=1,cex=0.6,cex.lab=1.5) #,xlim=c(1.5,3.2),ylim=c(2,8))
    text(2.5,6,'95%',col='black',cex.lab=1.5,pos=4)
    text(2.7,5.5,'90%',col='blue',cex.lab=1.5,pos=4)
    text(2.9,5,'75%',col='red',cex.lab=1.5,pos=4)
}
if (DataSet == 2) {
  plot(ellipse95,type='l',lwd=2,col='black',xlab=NA,ylab=NA,
       las=1,cex=0.6,cex.lab=1.5) #,xlim=c(1.5,3.2),ylim=c(2,8))
    text(2.8,5,'95%',col='black',cex.lab=1.5,pos=4)
    text(3.0,4.5,'90%',col='blue',cex.lab=1.5,pos=4)
    text(3.2,4,'75%',col='red',cex.lab=1.5,pos=4)
}
if (DataSet == 3) {
  plot(ellipse95,type='l',lwd=2,col='black',xlab=NA,ylab=NA,
       las=1,cex=0.6,cex.lab=1.5,xlim=c(1.5,3.2),ylim=c(2,8))
    text(2.1,6.5,'95%',col='black',cex.lab=1.5,pos=4)
    text(2.25,6,'90%',col='blue',cex.lab=1.5,pos=4)
    text(2.4,5.5,'75%',col='red',cex.lab=1.5,pos=4)
}
if (DataSet == 4) {
  plot(ellipse95,type='l',lwd=2,col='black',xlab=NA,ylab=NA,
       las=1,cex=0.6,cex.lab=1.5,xlim=c(2.5,3.2),ylim=c(2,5))
    text(2.58,3.3,'95%',col='black',cex.lab=1.5,pos=4)
    text(2.61,3.0,'90%',col='blue',cex.lab=1.5,pos=4)
    text(2.64,2.7,'75%',col='red',cex.lab=1.5,pos=4)
}
lines(ellipse75,lwd=2,col='red',lty=2)
lines(ellipse90,lwd=2,col='blue',lty=3)
title(xlab=TeX('Slope $\\beta$')) #,line=2.5)
title(ylab=TeX('Intercept $\\beta_0$'),line=2.5)
points(betaTrue,beta0True,col='black',lwd=4,cex=0.6,pch=20)
# points(betaArrMedian,beta0ArrMedian,col='magenta',lwd=4,cex=0.6,pch=22)
points(betaArrMean,beta0ArrMean,col='blue',lwd=4,cex=0.6,pch=24)
# dev.off()
} 
} # end of MCMCflag
(corxy=cor(x,y))
print(c(round(corxy,4),'cor(x,y)'))
print(' ---------------------------------------------------')
print('MCMC (end)')
print(' ---------------------------------------------------')
# [1] "file: R_MCMCartificial2504.R"
# [1] "Sun Apr  6 19:16:41 2025"
# [1] " ---------------------------------------------------"
# [1] "(1) Generate artificial data:"
# [1] "DataSet " "1"       
# [1] "24"            "n sample size"
# [1] "3"                  "2.5"                "betaTrue,beta0True"
# [1] "1"                 "1.5"               "sigxTrue,sigyTrue"
# [1] "0.4444" "lambdaTrue"       
# [1] "3"              "1"              "ximean,xisigma"
# [1] "0.67"            "5.5"             "min(xi),max(xi)"
# [1] " ---------------------------------------------------"
# [1] "xii from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.2934" "pvalue"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] "4.51"                      "19.01"                    
# [3] "min(yiiTrue),max(yiiTrue)"
# [1] "1"                 "2.25"              "varxTrue,varyTrue"
# [1] " ---------------------------------------------------"
# [1] "x from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.0086"  "pvaluex"
# [1] "normality rejected on alpha = 0.05 significance level"
# [1] " ---------------------------------------------------"
# [1] "ximean ... true lambda:"
# [1] " ---------------------------------------------------"
# [1] "2.8523" "xmean" 
# [1] "11.4277" "ymean"  
# [1] "19.3"      "68.9"      "SoSx,SoSy"
# [1] "0.4444"     "lambdaTrue"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "42.7586"     "271.4446"    "64.6447"     "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (begin):"
# [1] " ---------------------------------------------------"
# [1] "0.2554"  "0.13"  "meanxDiff,medianxDiff"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (end)"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(2) Simple linear regression (SLR): y on x"
# [1] " ---------------------------------------------------"
# [1] "3"   "1.5119" "+-" "0.4297"  "betaTrue,SlopeYonX"
# [1] "2.5" "7.1154" "+-" "1.3533". "beta0True,IcYonX"
# [1] "0.36"   "RsqSLR"
# [1] " ---------------------------------------------------"
# [1] " (3)  Simple linear regression (SLR): via x on y"
# [1] " ---------------------------------------------------"
# [1] "0.1308"  "+-"      "0.8064"  "a +- ua"
# [1] "0.2382"  "+-"      "0.0677"  "b +- ub"
# [1] "3"    "4.2365" "+-"  "1.1883"  "betaTrue,mrSlopeYonX1 (median,MADN)"
# [1] "2.5" "-0.502"  "+-"  "3.5182"  "beta0True,mrIcYonX1 (median,MADN)"
# [1] "3"    "4.199"   "betaTrue,SlopeYonX1"
# [1] "2.5" "-0.5493"  "beta0True,IcYonX1"
# [1] " ---------------------------------------------------"
# [1] "(4) Isobe et al. (1990): regression-based methods"
# [1] " ---------------------------------------------------"
# [1] "(4a) Isobe et al. (1990) y on x etc.:"
# [1] " ---------------------------------------------------"
# [1] "Variances of slope estimates:"
# [1] "0.13825"   "Var1I90 y on x"
# [1] "1.89025"   "Var2I90 via x on y"
# [1] "0.2199"    "Var3I90 bisection"
# [1] "1.76485"   "Var4I90 orthogonal"
# [1] "0.32382"   "Var5I90 geometric"
# [1] "0.11536"   "Cov12I90"
# [1] "uncertainties (= standard errors):"
# [1] "3" "1.5119"  "+-" "0.3718" "betaTrue,beta1I90+-SE y on x"
# [1] "3" "4.199"   "+-" "1.3749" "betaTrue,beta2I90+-SE via x on y"
# [1] "3" "2.3066"  "+-" "0.4689" "betaTrue,beta3I90+-SE bisection"
# [1] "3" "3.8007"  "+-" "1.3285" "betaTrue,beta4I90+-SE orthogonal"
# [1] "3" "2.5196"  "+-" "0.5691" "betaTrue,beta5I90+-SE geometric"
# [1] "uncertainties (= standard deviations):"
# [1] " ---------------------------------------------------"
# [1] " Isobe et al. (1990): intercepts & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] " ---------------------------------------------------"
# [1] "7.1154"  "7.1154"  "b0est,IC1I90 y on x"
# [1] "-0.5493"           "IC2I90 via x on y"
# [1] "4.8486"            "IC3I90 bisection"
# [1] "0.5869"            "IC4I90 orthogonal"
# [1] "4.241"             "IC5I90 geometric"
# [1] "1.2317"               "VarAlpha1I90"
# [1] "1.3533"  "1.1098"  "ub0est, SigAlpha1I90"
# [1] "18.3527"              "VarAlpha2I90"
# [1] "3.5182"  "4.284"   "sdIcYonX1, SigAlpha2I90,x on y"
# [1] "2.0817"               "VarAlpha3I90"
# [1] "1.4428"            "SigAlpha3I90, bisection"
# [1] "16.7086"              "VarAlpha4I90"
# [1] "4.0876"            "SigAlpha4I90, orthogonal"
# [1] "3.0596"               "VarAlpha5I90"
# [1] "1.7492"            "SigAlpha5I90, geometric"
# [1] "----------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC:"
# [1] " ---------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(7) MCMC pedestrian:"
# [1] "----------------------------------------------------"
# [1] "1. Set t=0 and select a set of initial/starting parameter"
# [1] "   values beta, beta0, varx, vary:"
# [1] "1e+06"                              
# [2] "M number of Monte Carlo runs/cycles"
# [1] "3"    "2.7"      "betaTrue,betaStart"
# [1] "2.5"  "1.25"     "beta0True,beta0Start"
# [1] "24"   "48"   "k,nu"
# [1] "1"    "0.01"  "varxTrue,varx (prior value)"
# [1] "2.25"  "0.0225"  "varyTrue,vary (prior value)"
# [1] "0.46"  "1.035"   "Px, Py (prior value)"
# [1] " ---------------------------------------------------"
# [1] "Define arrays to store MCMC time series"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC iterations start:"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "Analyze MCMC time series:"
# [1] " ---------------------------------------------------"
# [1] "(A) All time series data:"
# [1] " ---------------------------------------------------"
# [1] "3"   "2.6003" "+-" "0.3625" "betaTrue,betaArrMean"
# [1] "2.5" "4.0122" "+-" "1.1381" "beta0True,beta0ArrMean"
# [1] " ---------------------------------------------------"
# [1] "(B) Cut out first 1.5% of time series (burn-in region):"
# [1] " ---------------------------------------------------"
# [1] "15001"            "m1 (cut out + 1)"
# [1] "3"   "2.6003"  "+-"  "0.3625" "betaTrue,betaArrMean"
# [1] "2.5" "4.0122"  "+-"  "1.1381" "beta0True,beta0ArrMean"
# [1] "3"    "2.5714"     "betaTrue,betaArrMedian"
# [1] "2.5"  "4.0924"     "beta0True,beta0ArrMedian"
# [1] "1"    "0.7673" "+-" ""0.199"  "varxTrue,varxMCMC"
# [1] "2.25" "0.0477" "+-" "0.0153" "varyTrue,varyMCMC"
# [1] "0.74" "0.0449"              "varxMedian,varyMedian"
# [1] "0.4444"  "16.0943"    "lambdaTrue, LambdaMCMC"
# [1] "2.8832" "2.1117" "2.5291" "4.2925" "1.6526" "2.6652" "4.8816" "4.0248"
# [9] "2.6701" "1.9143" "3.0253" "3.3328" "2.9123" "0.8673" "0.9919" "0.6377"
# [17] "4.031"  "3.1519" "6.4747" "1.6606" "2.1951" "3.5666" "3.7516" "2.2128"
# [25] "xiEst" 
# [1] "11.5085" "9.5396"  "10.6048" "15.105"  "8.3681"  "10.9521" "16.6084"
# [8] "14.4218" "10.9648" "9.036"   "11.8712" "12.6559" "11.5829" "6.3642" 
# [15] "6.682"   "5.7782"  "14.4377" "12.1942" "20.6738" "8.3883"  "9.7524" 
# [22] "13.2526" "13.7247" "9.7978"  "yiEst"  
# [1] "2.8824"    "2.1188"    "2.5323"    "4.2796"    "1.6638"    "2.667"    
# [7] "4.8655"    "4.0139"    "2.672"     "1.9234"    "3.023"     "3.3274"   
# [13] "2.9112"    "0.8841"    "1.0085"    "0.657"     "4.0204"    "3.1484"   
# [19] "6.447"     "1.6719"    "2.2014"    "3.5594"    "3.743"     "2.2192"   
# [25] "xiiMedian"
# [1] "11.508"    "9.539"     "10.6033"   "15.108"    "8.3682"    "10.9506"  
# [7] "16.6089"   "14.4225"   "10.9634"   "9.0358"    "11.8712"   "12.6579"  
# [13] "11.583"    "6.3667"    "6.6801"    "5.7783"    "14.4377"   "12.1933"  
# [19] "20.6765"   "8.3871"    "9.7515"    "13.2517"   "13.7251"   "9.796"    
# [25] "yiiMedian"
# [1] " ---------------------------------------------------"
# [1] "(8) Mean squared error (MSE):"
# [1] " ---------------------------------------------------"
# [1] "0.8036" "MSEx"  
# [1] "2.87".  "MSEy"
# [1] "0.4931" "MSExi" 
# [1] "2.8025" "MSEyi" 
# [1] "----------------------------------------------------"
# [1] "(9) Plots (only possible when running MCMC; switch on by setting sflag values):"
# [1] "----------------------------------------------------"
# [1] "0.6"      "cor(x,y)"

# [1] "file: R_MCMCartificial2504.R"
# [1] "Mon Apr  7 07:47:43 2025"
# [1] " ---------------------------------------------------"
# [1] "(1) Generate artificial data:"
# [1] "DataSet " "2"       
# [1] "24"            "n sample size"
# [1] "3"                  "2.5"                "betaTrue,beta0True"
# [1] "1"                 "1.5"               "sigxTrue,sigyTrue"
# [1] "0.4444"     "lambdaTrue"
# [1] "3"              "2"              "ximean,xisigma"
# [1] "-1.66"           "8.01"            "min(xi),max(xi)"
# [1] " ---------------------------------------------------"
# [1] "xii from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.2934" "pvalue"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] "-2.48"                     "26.53"                    
# [3] "min(yiiTrue),max(yiiTrue)"
# [1] "1"                 "2.25"              "varxTrue,varyTrue"
# [1] " ---------------------------------------------------"
# [1] "x from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.0699"  "pvaluex"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] " ---------------------------------------------------"
# [1] "ximean ... true lambda:"
# [1] " ---------------------------------------------------"
# [1] "2.9079" "xmean" 
# [1] "11.5944" "ymean"  
# [1] "19.3"      "68.9"      "SoSx,SoSy"
# [1] "0.4444"     "lambdaTrue"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "112.1978"    "880.1698"    "270.2577"    "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (begin):"
# [1] " ---------------------------------------------------"
# [1] "0.4513"                "0.223"                
# [3] "meanxDiff,medianxDiff"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (end)"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(2) Simple linear regression (SLR): y on x"
# [1] " ---------------------------------------------------"
# [1] "3"    "2.4088"  "+-"  "0.3047"  "betaTrue,SlopeYonX"
# [1] "2.5"  "4.59"    "+-"  "1.1042"  "beta0True,IcYonX"
# [1] "0.7396" "RsqSLR"
# [1] " ---------------------------------------------------"
# [1] " (3)  Simple linear regression (SLR): via x on y"
# [1] " ---------------------------------------------------"
# [1] "-0.6522"  "+-"      "0.5081"  "a +- ua"
# [1]  "0.3071"  "+-"      "0.0388"  "b +- ub"
# [1] "3" "3.2697" "+-"    "0.4205"  "betaTrue,mrSlopeYonX1 (median,MADN)"
# [1] "2.5"  "2.1171" "+-" "1.6996"  "beta0True,mrIcYonX1 (median,MADN)"
# [1] "3"    "3.2568"            "betaTrue,SlopeYonX1"
# [1] "2.5"  "2.124"             "beta0True,IcYonX1"
# [1] " ---------------------------------------------------"
# [1] "(4) Isobe et al. (1990): regression-based methods"
# [1] " ---------------------------------------------------"
# [1] "(4a) Isobe et al. (1990) y on x etc.:"
# [1] " ---------------------------------------------------"
# [1] "Variances of slope estimates:"
# [1] "0.13342"        "Var1I90 y on x"
# [1] "0.29745"        "Var2I90 via x on y"
# [1] "0.17319"        "Var3I90 bisection"
# [1] "0.29442"        "Var4I90 orthogonal"
# [1] "0.17988"        "Var5I90 geometric"
# [1] "0.15957"        "Cov12I90"
# [1] "uncertainties (= standard errors):"
# [1] "3" "2.4088"  "+-"  "0.3653"  "betaTrue,beta1I90+-SE y on x"
# [1] "3" "3.2568"  "+-"  "0.5454"  "betaTrue,beta2I90+-SE via x on y"
# [1] "3" "2.7765"  "+-"  "0.4162"  "betaTrue,beta3I90+-SE bisection"
# [1] "3" "3.1583"  "+-"  "0.5426"  "betaTrue,beta4I90+-SE orthogonal"
# [1] "3" "2.8009"  "+-"  "0.4241"  "betaTrue,beta5I90+-SE geometric"
# [1] "uncertainties (= standard deviations):"
# [1] " ---------------------------------------------------"
# [1] " Isobe et al. (1990): intercepts & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] " ---------------------------------------------------"
# [1] "4.59"  "4.59"    "b0est,IC1I90 y on x"
# [1] "2.124"            "IC2I90 via x on y"
# [1] "3.5207"           "IC3I90 bisection"
# [1] "2.4105"           "IC4I90 orthogonal"
# [1] "3.4498"           "IC5I90 geometric"
# [1] "1.3167"                "VarAlpha1I90"
# [1] "1.1042"  "1.1475"  "ub0est, SigAlpha1I90"
# [1] "3.6592"                "VarAlpha2I90"
# [1] "1.6996"  "1.9129"   "sdIcYonX1, SigAlpha2I90,x on y"
# [1] "1.9474"                "VarAlpha3I90"
# [1] "1.3955"              "SigAlpha3I90, bisection"
# [1] "3.498"                  "VarAlpha4I90"
# [1] "1.8703"               "SigAlpha4I90, orthogonal"
# [1] "2.0338"                 "VarAlpha5I90"
# [1] "1.4261"               "SigAlpha5I90, geometric"
# [1] "----------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC:"
# [1] " ---------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(7) MCMC pedestrian:"
# [1] "----------------------------------------------------"
# [1] "1. Set t=0 and select a set of initial/starting parameter"
# [1] "   values beta, beta0, varx, vary:"
# [1] "1e+06"     "M number of Monte Carlo runs/cycles"
# [1] "3"   "2.7"     "betaTrue,betaStart"
# [1] "2.5" "1.25"    "beta0True,beta0Start"
# [1] "24"   "48"   "k,nu"
# [1] "1"   "0.01"  "varxTrue,varx (prior value)"
# [1] "2.25"  "0.0225" "varyTrue,vary (prior value)"
# [1] "0.46"                 "1.035"               
# [3] "Px, Py (prior value)"
# [1] " ---------------------------------------------------"
# [1] "Define arrays to store MCMC time series"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC iterations start:"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "Analyze MCMC time series:"
# [1] " ---------------------------------------------------"
# [1] "(A) All time series data:"
# [1] " ---------------------------------------------------"
# [1] "3" "2.835"    "+-"   "0.2326"  "betaTrue,betaArrMean"
# [1] "2.5" "3.351"  "+-"   "0.8411"  "beta0True,beta0ArrMean"
# [1] " ---------------------------------------------------"
# [1] "(B) Cut out first 1.5% of time series (burn-in region):"
# [1] " ---------------------------------------------------"
# [1] "15001"            "m1 (cut out + 1)"
# [1] "3"   "2.8339" "+-" "0.2326"  "betaTrue,betaArrMean"
# [1] "2.5" "3.3557" "+-" "0.8411"  "beta0True,beta0ArrMean"
# [1] "3"                      "2.8232"                
# [3] "betaTrue,betaArrMedian"
# [1] "2.5"                      "3.3901"                  
# [3] "beta0True,beta0ArrMedian"
# [1] "1"                 "0.7122"            "+-"               
# [4] "0.1668"            "varxTrue,varxMCMC"
# [1] "2.25"              "0.0476"            "+-"               
# [4] "0.0153"            "varyTrue,varyMCMC"
# [1] "0.688"                 "0.0448"               
# [3] "varxMedian,varyMedian"
# [1] "0.4444"                 "14.9608"               
# [3] "lambdaTrue, LambdaMCMC"
# [1] "2.8999"  "1.2134"  "2.9952"  "4.3546"  "2.006"   "2.9296" 
# [7] "6.2701"  "5.0835"  "2.6781"  "0.484"   "2.8643"  "3.4604" 
# [13] "2.8227"  "-1.4341" "0.6031"  "-0.7771" "4.7613"  "3.7276" 
# [19] "8.8035"  "1.4903"  "2.4669"  "4.0009"  "4.1909"  "1.8753" 
# [25] "xiEst"  
# [1] "11.5742" "6.8261"  "11.8425" "15.6696" "9.0575"  "11.6577"
# [7] "21.0623" "17.7215" "10.9497" "4.7727"  "11.474"  "13.1522"
# [13] "11.3569" "-0.6272" "5.1079"  "1.2225"  "16.8146" "13.9043"
# [19] "28.1945" "7.6057"  "10.355"  "14.6739" "15.2088" "8.6897" 
# [25] "yiEst"  
# [1] "2.8995"    "1.2181"    "2.9946"    "4.3489"    "2.0084"   
# [6] "2.9296"    "6.2607"    "5.0759"    "2.6788"    "0.4908"   
# [11] "2.8642"    "3.4578"    "2.8226"    "-1.4231"   "0.6098"   
# [16] "-0.7669"   "4.7548"    "3.724"     "8.7879"    "1.4946"   
# [21] "2.4681"    "3.9964"    "4.1859"    "1.8785"    "xiiMedian"
# [1] "11.5736"   "6.8256"    "11.841"    "15.6724"   "9.0577"   
# [6] "11.6562"   "21.0627"   "17.7223"   "10.9484"   "4.7724"   
# [11] "11.474"    "13.1542"   "11.3569"   "-0.6248"   "5.1062"   
# [16] "1.2227"    "16.8144"   "13.9035"   "28.197"    "7.6046"   
# [21] "10.3542"   "14.673"    "15.2092"   "8.688"     "yiiMedian"
# [1] " ---------------------------------------------------"
# [1] "(8) Mean squared error (MSE):"
# [1] " ---------------------------------------------------"
# [1] "0.8036" "MSEx"  
# [1] "2.87" "MSEy"
# [1] "0.4058" "MSExi" 
# [1] "2.8092" "MSEyi" 
# [1] "----------------------------------------------------"
# [1] "(9) Plots (only possible when running MCMC; switch on by setting sflag values):"
# [1] "----------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(1) Plot data & estimated lines:"
# [1] "----------------------------------------------------"

# [1] "file: R_MCMCartificial2504.R"
# [1] "Mon Apr  7 08:20:15 2025"
# [1] " ---------------------------------------------------"
# [1] "(1) Generate artificial data:"
# [1] "DataSet " "3"       
# [1] "48"            "n sample size"
# [1] "3"                  "2.5"                "betaTrue,beta0True"
# [1] "1"                 "1.5"               "sigxTrue,sigyTrue"
# [1] "0.4444"     "lambdaTrue"
# [1] "3"              "1"              "ximean,xisigma"
# [1] "0.67"            "5.5"             "min(xi),max(xi)"
# [1] " ---------------------------------------------------"
# [1] "xii from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.0899" "pvalue"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] "4.51"                      "19.01"                    
# [3] "min(yiiTrue),max(yiiTrue)"
# [1] "1"                 "2.25"              "varxTrue,varyTrue"
# [1] " ---------------------------------------------------"
# [1] "x from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.9375"  "pvaluex"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] " ---------------------------------------------------"
# [1] "ximean ... true lambda:"
# [1] " ---------------------------------------------------"
# [1] "2.788" "xmean"
# [1] "11.4768" "ymean"  
# [1] "48.4"      "105.7"     "SoSx,SoSy"
# [1] "0.4444"     "lambdaTrue"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "87.3823"     "363.2211"    "113.909"     "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (begin):"
# [1] " ---------------------------------------------------"
# [1] "0.152"                 "0.0626"               
# [3] "meanxDiff,medianxDiff"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (end)"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(2) Simple linear regression (SLR): y on x"
# [1] " ---------------------------------------------------"
# [1] "3"   "1.3036"  "+-" "0.2311" "betaTrue,SlopeYonX"
# [1] "2.5" "7.8425"  "+-" "0.7159" "beta0True,IcYonX"
# [1] "0.4088" "RsqSLR"
# [1] " ---------------------------------------------------"
# [1] " (3)  Simple linear regression (SLR): via x on y"
# [1] " ---------------------------------------------------"
# [1] "-0.8113" "+-"      "0.6562"  "a +- ua"
# [1] "0.3136"  "+-"      "0.0556"  "b +- ub"
# [1] "3"    "3.1957"  "+-"  "0.5816"  "betaTrue,mrSlopeYonX1 (median,MADN)"
# [1] "2.5"  "2.5341"  "+-"  "2.1635"  "beta0True,mrIcYonX1 (median,MADN)"
# [1] "3"     "3.1887"            "betaTrue,SlopeYonX1"
# [1] "2.5"   "2.5869"            "beta0True,IcYonX1"
# [1] " ---------------------------------------------------"
# [1] "(4) Isobe et al. (1990): regression-based methods"
# [1] " ---------------------------------------------------"
# [1] "(4a) Isobe et al. (1990) y on x etc.:"
# [1] " ---------------------------------------------------"
# [1] "Variances of slope estimates:"
# [1] "0.04766"        "Var1I90 y on x"
# [1] "0.31734"        "Var2I90 via x on y"
# [1] "0.04413"           "Var3I90 bisection"
# [1] "0.24631"            "Var4I90 orthogonal"
# [1] "0.0532"            "Var5I90 geometric"
# [1] "-0.01677" "Cov12I90"
# [1] "uncertainties (= standard errors):"
# [1] "3" "1.3036" "+-" "0.2183" "betaTrue,beta1I90+-SE y on x"
# [1] "3" "3.1887" "+-" "0.5633" "betaTrue,beta2I90+-SE via x on y"
# [1] "3" "1.9249" "+-" "0.2101" "betaTrue,beta3I90+-SE bisection"
# [1] "3" "2.7811" "+-" "0.4963" "betaTrue,beta4I90+-SE orthogonal"
# [1] "3" "2.0388" "+-" "0.2306" "betaTrue,beta5I90+-SE geometric"
# [1] "uncertainties (= standard deviations):"
# [1] " ---------------------------------------------------"
# [1] " Isobe et al. (1990): intercepts & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] " ---------------------------------------------------"
# [1] "7.8425"  "7.8425"  "b0est,IC1I90 y on x"
# [1] "2.5869"            "IC2I90 via x on y"
# [1] "6.1103"            "IC3I90 bisection"
# [1] "3.7231"            "IC4I90 orthogonal"
# [1] "5.7928"            "IC5I90 geometric"
# [1] "0.5239"                 "VarAlpha1I90"
# [1] "0.7159"  "0.7238"  "ub0est, SigAlpha1I90"
# [1] "2.7924"                  "VarAlpha2I90"
# [1] "2.1635"  "1.671"   "sdIcYonX1, SigAlpha2I90, via x on y"
# [1] "0.5651"                  "VarAlpha3I90"
# [1] "0.7517"             "SigAlpha3I90, bisection"
# [1] "2.2652"                  "VarAlpha4I90"
# [1] "1.5051"                   "SigAlpha4I90, orthogonal"
# [1] "0.6577"                   "VarAlpha5I90"
# [1] "0.811"               "SigAlpha5I90, geometric"
# [1] "----------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC:"
# [1] " ---------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(7) MCMC pedestrian:"
# [1] "----------------------------------------------------"
# [1] "1. Set t=0 and select a set of initial/starting parameter"
# [1] "   values beta, beta0, varx, vary:"
# [1] "1e+06"                              
# [2] "M number of Monte Carlo runs/cycles"
# [1] "3"                  "2.7"                "betaTrue,betaStart"
# [1] "2.5"                  "1.25"                
# [3] "beta0True,beta0Start"
# [1] "48"   "96"   "k,nu"
# [1] "1"                           "0.01"                       
# [3] "varxTrue,varx (prior value)"
# [1] "2.25"                        "0.0225"                     
# [3] "varyTrue,vary (prior value)"
# [1] "0.94"                 "2.115"               
# [3] "Px, Py (prior value)"
# [1] " ---------------------------------------------------"
# [1] "Define arrays to store MCMC time series"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC iterations start:"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "Analyze MCMC time series:"
# [1] " ---------------------------------------------------"
# [1] "(A) All time series data:"
# [1] " ---------------------------------------------------"
# [1] "3"                    "2.052"               
# [3] "+-"                   "0.1834"              
# [5] "betaTrue,betaArrMean"
# [1] "2.5"                    "5.749"                 
# [3] "+-"                     "0.5682"                
# [5] "beta0True,beta0ArrMean"
# [1] " ---------------------------------------------------"
# [1] "(B) Cut out first 1.5% of time series (burn-in region):"
# [1] " ---------------------------------------------------"
# [1] "15001"            "m1 (cut out + 1)"
# [1] "3"   "2.0511" "+-"  "0.1834"    "betaTrue,betaArrMean"
# [1] "2.5" "5.7516" "+-"  "0.5682"    "beta0True,beta0ArrMean"
# [1] "3"                      "2.0398"                
# [3] "betaTrue,betaArrMedian"
# [1] "2.5"                      "5.7849"                  
# [3] "beta0True,beta0ArrMedian"
# [1] "1"                 "0.679"             "+-"               
# [4] "0.1187"            "varxTrue,varxMCMC"
# [1] "2.25"              "0.0469"            "+-"               
# [4] "0.0102"            "varyTrue,varyMCMC"
# [1] "0.6656"                "0.0455"               
# [3] "varxMedian,varyMedian"
# [1] "0.4444"                 "14.4881"               
# [3] "lambdaTrue, LambdaMCMC"
# [1] "3.3215"  "1.3593"  "4.2992"  "2.7955"  "2.219"   "3.0973" 
# [7] "4.8134"  "4.9213"  "0.9384"  "1.855"   "1.8356"  "2.8902" 
# [13] "3.1261"  "1.3252"  "1.5822"  "0.8712"  "2.9459"  "3.3521" 
# [19] "6.1963"  "2.9867"  "3.2657"  "3.4825"  "4.2996"  "3.2146" 
# [25] "3.7689"  "4.299"   "2.7538"  "1.2313"  "0.6692"  "3.3805" 
# [31] "2.6927"  "2.9033"  "3.4304"  "2.7546"  "3.2531"  "1.0443" 
# [37] "2.9088"  "-1.0424" "2.7227"  "1.9571"  "4.3976"  "4.1981" 
# [43] "0.5536"  "2.3658"  "1.2626"  "3.5365"  "3.7076"  "4.2387" 
# [49] "xiEst"  
# [1] "12.5561" "8.562"   "14.5462" "11.4853" "10.3119" "12.0997"
# [7] "15.5928" "15.8125" "7.7052"  "9.5708"  "9.5314"  "11.678" 
# [13] "12.1582" "8.4924"  "9.0156"  "7.5685"  "11.7914" "12.6184"
# [19] "18.4077" "11.8746" "12.4424" "12.8838" "14.547"  "12.3384"
# [25] "13.4667" "14.5457" "11.4004" "8.3015"  "7.1573"  "12.6761"
# [31] "11.2761" "11.7047" "12.7777" "11.402"  "12.4168" "7.9209" 
# [37] "11.716"  "3.6733"  "11.3371" "9.7788"  "14.7466" "14.3404"
# [43] "6.9218"  "10.6106" "8.365"   "12.9937" "13.3419" "14.423" 
# [49] "yiEst"  
# [1] "3.3189"    "1.365"     "4.2924"    "2.7953"    "2.222"    
# [6] "3.0959"    "4.8054"    "4.913"     "0.945"     "1.8592"   
# [11] "1.8399"    "2.8895"    "3.1242"    "1.331"     "1.5873"   
# [16] "0.8777"    "2.945"     "3.3492"    "6.185"     "2.9859"   
# [21] "3.263"     "3.4791"    "4.293"     "3.2123"    "3.7644"   
# [26] "4.2924"    "2.7537"    "1.2372"    "0.6763"    "3.3773"   
# [31] "2.6935"    "2.9027"    "3.427"     "2.7549"    "3.2507"   
# [36] "1.0508"    "2.9081"    "-1.0333"   "2.723"     "1.9612"   
# [41] "4.3907"    "4.1918"    "0.5609"    "2.3678"    "1.2687"   
# [46] "3.533"     "3.7029"    "4.232"     "xiiMedian"
# [1] "12.5561"   "8.5611"    "14.5481"   "11.4833"   "10.3131"  
# [6] "12.1001"   "15.5929"   "15.8136"   "7.7038"    "9.57"     
# [11] "9.5297"    "11.6775"   "12.1587"   "8.4918"    "9.0165"   
# [16] "7.5688"    "11.7907"   "12.6188"   "18.4072"   "11.876"   
# [21] "12.4434"   "12.8834"   "14.547"    "12.3391"   "13.4667"  
# [26] "14.5457"   "11.4009"   "8.3016"    "7.1561"    "12.6767"  
# [31] "11.2765"   "11.7041"   "12.777"    "11.4026"   "12.4175"  
# [36] "7.9222"    "11.7161"   "3.6718"    "11.3362"   "9.7786"   
# [41] "14.7472"   "14.3415"   "6.9204"    "10.6102"   "8.3636"   
# [46] "12.9944"   "13.3427"   "14.4228"   "yiiMedian"
# [1] " ---------------------------------------------------"
# [1] "(8) Mean squared error (MSE):"
# [1] " ---------------------------------------------------"
# [1] "1.0074" "MSEx"  
# [1] "2.2012" "MSEy"  
# [1] "0.5349" "MSExi" 
# [1] "2.1586" "MSEyi" 
# [1] "----------------------------------------------------"
# [1] "(9) Plots (only possible when running MCMC; switch on by setting sflag values):"
# [1] "----------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(1) Plot data & estimated lines:"
# [1] "----------------------------------------------------"
# [1] "0.6394"   "cor(x,y)"
# [1] " ---------------------------------------------------"
# [1] "MCMC (end)"
# [1] " ---------------------------------------------------"

# [1] "file: R_MCMCartificial2504.R"
# [1] "Mon Apr  7 08:24:13 2025"
# [1] " ---------------------------------------------------"
# [1] "(1) Generate artificial data:"
# [1] "DataSet " "4"       
# [1] "48"            "n sample size"
# [1] "3"                  "2.5"                "betaTrue,beta0True"
# [1] "1"                 "1.5"               "sigxTrue,sigyTrue"
# [1] "0.4444"     "lambdaTrue"
# [1] "3"              "4"              "ximean,xisigma"
# [1] "-6.32"           "13.02"           "min(xi),max(xi)"
# [1] " ---------------------------------------------------"
# [1] "xii from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.0899" "pvalue"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] "-16.47"                    "41.56"                    
# [3] "min(yiiTrue),max(yiiTrue)"
# [1] "1"                 "2.25"              "varxTrue,varyTrue"
# [1] " ---------------------------------------------------"
# [1] "x from normal PDF? Shapiro-Wilk test"
# [1] " ---------------------------------------------------"
# [1] "0.1858"  "pvaluex"
# [1] "normality not rejected on alpha = 0.05 significance level"
# [1] " ---------------------------------------------------"
# [1] "ximean ... true lambda:"
# [1] " ---------------------------------------------------"
# [1] "2.5665" "xmean" 
# [1] "10.8124" "ymean"  
# [1] "48.4"      "105.7"     "SoSx,SoSy"
# [1] "0.4444"     "lambdaTrue"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "706.2775"    "5633.0848"   "1920.5624"   "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (begin):"
# [1] " ---------------------------------------------------"
# [1] "0.4139"                "0.2307"               
# [3] "meanxDiff,medianxDiff"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (end)"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(2) Simple linear regression (SLR): y on x"
# [1] " ---------------------------------------------------"
# [1] "3"   "2.7193"  "+-"   "0.1124" "betaTrue,SlopeYonX"
# [1] "2.5" "3.8334"  "+-"   "0.5188" "beta0True,IcYonX"
# [1] "0.9271" "RsqSLR"
# [1] " ---------------------------------------------------"
# [1] " (3)  Simple linear regression (SLR): via x on y"
# [1] " ---------------------------------------------------"
# [1] "-1.1199" "+-"      "0.2157"  "a +- ua"
# [1] "0.3409"  "+-"      "0.0141"  "b +- ub"
# [1] "3"    "2.9345" "+-" "0.1253"  "betaTrue,mrSlopeYonX1 (median,MADN)"
# [1] "2.5"  "3.2681" "+-" "0.6405"  "beta0True,mrIcYonX1 (median,MADN)"
# [1] "3"    "2.933"               "betaTrue,SlopeYonX1"
# [1] "2.5"  "3.2848"            "beta0True,IcYonX1"
# [1] " ---------------------------------------------------"
# [1] "(4) Isobe et al. (1990): regression-based methods"
# [1] " ---------------------------------------------------"
# [1] "(4a) Isobe et al. (1990) y on x etc.:"
# [1] " ---------------------------------------------------"
# [1] "Variances of slope estimates:"
# [1] "0.01053"        "Var1I90 y on x"
# [1] "0.01269"            "Var2I90 via x on y"
# [1] "0.01056"           "Var3I90 bisection"
# [1] "0.01238"            "Var4I90 orthogonal"
# [1] "0.01058"           "Var5I90 geometric"
# [1] "0.0096"   "Cov12I90"
# [1] "uncertainties (= standard errors):"
# [1] "3" "2.7193" "+-" "0.1026" "betaTrue,beta1I90+-SE y on x"
# [1] "3" "2.933"  "+-" "0.1127" "betaTrue,beta2I90+-SE via x on y"
# [1] "3" "2.8226" "+-" "0.1028" "betaTrue,beta3I90+-SE bisection"
# [1] "3" "2.909"  "+-" "0.1113" "betaTrue,beta4I90+-SE orthogonal"
# [1] "3" "2.8241" "+-" "0.1029" "betaTrue,beta5I90+-SE geometric"
# [1] "uncertainties (= standard deviations):"
# [1] " ---------------------------------------------------"
# [1] " Isobe et al. (1990): intercepts & their uncertainties"
# [1] "  uncertainty = standard error, i.e. proportional 1/sqrt(n)"
# [1] " ---------------------------------------------------"
# [1] "3.8334" "3.8334"   "b0est,IC1I90 y on x"
# [1] "3.2848"            "IC2I90 via x on y"
# [1] "3.5683"            "IC3I90 bisection"
# [1] "3.3464"            "IC4I90 orthogonal"
# [1] "3.5643"            "IC5I90 geometric"
# [1] "0.3011"                  "VarAlpha1I90"
# [1] "0.5188"  "0.5487" "ub0est, SigAlpha1I90"
# [1] "0.2848"       "VarAlpha2I90"
# [1] "0.6405"  "0.5337"  "sdIcYonX1, SigAlpha2I90, via x on y"
# [1] "0.2844"                      "VarAlpha3I90"
# [1] "0.5333"       "SigAlpha3I90, bisection"
# [1] "0.2856"                "VarAlpha4I90"
# [1] "0.5344"        "SigAlpha4I90, orthogonal"
# [1] "0.2841"                "VarAlpha5I90"
# [1] "0.533"         "SigAlpha5I90, geometric"
# [1] "----------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC:"
# [1] " ---------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(7) MCMC pedestrian:"
# [1] "----------------------------------------------------"
# [1] "1. Set t=0 and select a set of initial/starting parameter"
# [1] "   values beta, beta0, varx, vary:"
# [1] "1e+06"                              
# [2] "M number of Monte Carlo runs/cycles"
# [1] "3"                  "2.7"                "betaTrue,betaStart"
# [1] "2.5"                  "1.25"                
# [3] "beta0True,beta0Start"
# [1] "48"   "96"   "k,nu"
# [1] "1"                           "0.01"                       
# [3] "varxTrue,varx (prior value)"
# [1] "2.25"                        "0.0225"                     
# [3] "varyTrue,vary (prior value)"
# [1] "0.94"                 "2.115"               
# [3] "Px, Py (prior value)"
# [1] " ---------------------------------------------------"
# [1] "Define arrays to store MCMC time series"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC iterations start:"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "Analyze MCMC time series:"
# [1] " ---------------------------------------------------"
# [1] "(A) All time series data:"
# [1] " ---------------------------------------------------"
# [1] "3"                    "2.8252"              
# [3] "+-"                   "0.0806"              
# [5] "betaTrue,betaArrMean"
# [1] "2.5"                    "3.5506"                
# [3] "+-"                     "0.3714"                
# [5] "beta0True,beta0ArrMean"
# [1] " ---------------------------------------------------"
# [1] "(B) Cut out first 1.5% of time series (burn-in region):"
# [1] " ---------------------------------------------------"
# [1] "15001"            "m1 (cut out + 1)"
# [1] "3"   "2.8251" "+-"  "0.0806" "betaTrue,betaArrMean"
# [1] "2.5" "3.5509" "+-"  "0.3714" "beta0True,beta0ArrMean"
# [1] "3"                      "2.8232"                
# [3] "betaTrue,betaArrMedian"
# [1] "2.5"                      "3.5562"                  
# [3] "beta0True,beta0ArrMedian"
# [1] "1"                 "0.5717"            "+-"               
# [4] "0.0876"            "varxTrue,varxMCMC"
# [1] "2.25"              "0.0465"            "+-"               
# [4] "0.01"              "varyTrue,varyMCMC"
# [1] "0.563"                 "0.0452"               
# [3] "varxMedian,varyMedian"
# [1] "0.4444"                 "12.2915"               
# [3] "lambdaTrue, LambdaMCMC"
# [1] "3.26"    "-1.1149" "5.2183"  "3.3988"  "3.1296"  "3.7794" 
# [7] "8.9971"  "7.8529"  "1.4461"  "-2.4025" "1.6883"  "3.4009" 
# [13] "2.8073"  "-5.6835" "0.2654"  "-3.4203" "5.4382"  "5.0295" 
# [19] "13.2517" "2.122"   "3.7944"  "4.8148"  "5.4727"  "1.9388" 
# [25] "4.2668"  "5.8733"  "3.4491"  "-3.0724" "-3.8413" "5.3182" 
# [31] "2.3872"  "0.8187"  "5.8044"  "3.7926"  "3.876"   "-3.3088"
# [37] "2.2587"  "-6.5598" "3.5369"  "-0.9872" "5.036"   "4.9759" 
# [43] "-3.1476" "1.1273"  "0.5933"  "6.5418"  "2.7754"  "7.377"  
# [49] "xiEst"  
# [1] "12.7589"  "0.4096"   "18.2869"  "13.1508"  "12.3908"  "14.2253" 
# [7] "28.9537"  "25.724"   "7.6388"   "-3.2249"  "8.3225"   "13.1568" 
# [13] "11.4813"  "-12.4866" "4.306"    "-6.0981"  "18.9077"  "17.754"  
# [19] "40.9636"  "9.5468"   "14.2675"  "17.148"   "19.005"   "9.0294"  
# [25] "15.601"   "20.1359"  "13.2927"  "-5.1159"  "-7.2865"  "18.5689" 
# [31] "10.2953"  "5.8679"   "19.9414"  "14.2623"  "14.4978"  "-5.7834" 
# [37] "9.9325"   "-14.9603" "13.5407"  "0.77"     "17.7724"  "17.6026" 
# [43] "-5.3285"  "6.739"    "5.2315"   "22.0229"  "11.3911"  "24.3806" 
# [49] "yiEst"   
# [1] "3.2596"    "-1.1142"   "5.2168"    "3.3983"    "3.1292"   
# [6] "3.7787"    "8.9945"    "7.8507"    "1.4463"    "-2.4016"  
# [11] "1.6885"    "3.4004"    "2.8071"    "-5.6824"   "0.2662"   
# [16] "-3.4194"   "5.4366"    "5.028"     "13.2487"   "2.1222"   
# [21] "3.7933"    "4.8137"    "5.4712"    "1.9389"    "4.2659"   
# [26] "5.8716"    "3.4484"    "-3.0715"   "-3.8404"   "5.3166"   
# [31] "2.3873"    "0.8192"    "5.8027"    "3.7917"    "3.8752"   
# [36] "-3.3082"   "2.2587"    "-6.5589"   "3.5364"    "-0.9865"  
# [41] "5.0346"    "4.9744"    "-3.1466"   "1.1277"    "0.5938"   
# [46] "6.5399"    "2.7749"    "7.3749"    "xiiMedian"
# [1] "12.7589"   "0.409"     "18.2882"   "13.1491"   "12.3919"  
# [6] "14.2256"   "28.9537"   "25.7247"   "7.6381"    "-3.2256"  
# [11] "8.3211"    "13.1564"   "11.4817"   "-12.4871"  "4.3069"   
# [16] "-6.0976"   "18.9072"   "17.7544"   "40.9629"   "9.5479"   
# [21] "14.2682"   "17.1476"   "19.0048"   "9.03"      "15.6009"  
# [26] "20.1357"   "13.2932"   "-5.1157"   "-7.2872"   "18.5693"  
# [31] "10.2956"   "5.8673"    "19.9408"   "14.2628"   "14.4983"  
# [36] "-5.782"    "9.9326"    "-14.9611"  "13.54"     "0.77"     
# [41] "17.7726"   "17.6033"   "-5.3294"   "6.7387"    "5.2305"   
# [46] "22.0234"   "11.3916"   "24.3803"   "yiiMedian"
# [1] " ---------------------------------------------------"
# [1] "(8) Mean squared error (MSE):"
# [1] " ---------------------------------------------------"
# [1] "1.0074" "MSEx"  
# [1] "2.2012" "MSEy"  
# [1] "0.2695" "MSExi" 
# [1] "2.1765" "MSEyi" 
# [1] "----------------------------------------------------"
# [1] "(9) Plots (only possible when running MCMC; switch on by setting sflag values):"
# [1] "----------------------------------------------------"
# [1] "----------------------------------------------------"
# [1] "(1) Plot data & estimated lines:"
# [1] "----------------------------------------------------"
# [1] "0.9629"   "cor(x,y)"
# [1] " ---------------------------------------------------"
# [1] "MCMC (end)"
# [1] " ---------------------------------------------------"
