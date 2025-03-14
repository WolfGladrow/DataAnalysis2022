print('file: Jitjareonchai06MCMC2503.R')
print(date())
# purpose: Errors-In-Variables (EIV)
# analyse example Jitjareonchai et al. (2006) using SLR to MCMC
# created by: Dieter.Wolf-Gladrow@awi.de 
#    12/2024 version 1.0
# update: 3/2025 Isobe et al. (1990) estimates added
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
xiiTrueMean = mean(xiiTrue); SSx = sum((xiiTrue-xiiTrueMean)^2)
print(c(round(xiiTrueMean,4),'xiiTrueMean'))
print(c(round(SSx,4),'SSx'))
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
xmean = mean(x); print(c(round(xmean,4),'xmean'))
ymean = mean(y); print(c(round(ymean,4),'ymean'))
SoSx = sum((x-xiiTrue)^2); SoSy = sum((y-yiiTrue)^2)
print(c(round(SoSx,1),round(SoSy,1),'SoSx,SoSy'))
lambdaTrue = varxTrue/varyTrue # ratio of error variances (defined according to Casella & Berger, 2002)
print(c(lambdaTrue,'lambdaTrue'))
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
IcYonX    = as.numeric(SLRyonx$coefficients[1])
SlopeYonX = as.numeric(SLRyonx$coefficients[2])
uIcYonX    = as.numeric(SLRyonx$coefficients[3])
uSlopeYonX = as.numeric(SLRyonx$coefficients[4])
print(c(betaTrue,round(SlopeYonX,2),'+-',round(uSlopeYonX,2),'betaTrue,SlopeYonX'))
print(c(beta0True,round(IcYonX,2),'+-',round(uIcYonX,2),'beta0True,IcYonX'))
(RsqSLR = SLRyonx$r.squared)
print(c(round(RsqSLR,5),'RsqSLR'))
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
print(c(betaTrue,round(mrSlopeYonX1,2),'+-',
        round(sdSlopeYonX1,2),'betaTrue,mrSlopeYonX1 (median, random)'))
print(c(beta0True,round(mrIcYonX1,2),'+-',
        round(sdIcYonX1,2),'beta0True,mrIcYonX1 (median, random)'))
print(c(betaTrue,round(SlopeYonX1,2),'betaTrue,SlopeYonX1'))
print(c(beta0True,round(IcYonX1,2),'beta0True,IcYonX1'))
print(' ---------------------------------------------------')
print('(4) Geometric mean of regression slopes:')
# (also called: geometric mean regression, Legendre & Legendre, 2012, p.550)
print(' ---------------------------------------------------')
SlopeGeo = sqrt(SlopeYonX*SlopeYonX1)
xc = mean(x); yc = mean(y) # centroid
# line with slope SlopeGeo through centroid:
# y-yc = SlopeGeo*(x-xc)
# x = 0 -> y = IcGeo = yc-SlopeGeo*xc
IcGeo = yc-SlopeGeo*xc
print(c(betaTrue,round(SlopeGeo,2),'betaTrue,SlopeGeo'))
print(c(beta0True,round(IcGeo,2),'beta0True,IcGeo'))
# Propagation of uncertainty (Monte Carlo):
Mr = 1000 # number of Monte Carlo values
rSlopeYonX = rnorm(Mr,SlopeYonX,uSlopeYonX)
rSlopeGeo = sqrt(rSlopeYonX*rSlopeYonX1)
rIcGeo = yc-rSlopeGeo*xc
# hist(rSlopeGeo,col='blue',breaks=round(sqrt(Mr)),las=1,main='') # check normality
# hist(rIcGeo,col='blue',breaks=round(sqrt(Mr)),las=1,main='')       # check normality
# rSlopeGeo looks almost symmetric around its mode
# rIcGeo with long left tail -> apply robust estimation
mrSlopeGeo = median(rSlopeGeo)
mrIcGeo = median(rIcGeo)
sdSlopeGeo = myMADN(rSlopeGeo)
sdIcGeo = myMADN(rIcGeo)
print(c(betaTrue,round(mrSlopeGeo,2),'+-',
        round(sdSlopeGeo,2),'betaTrue,mrSlopeGeo'))
print(c(beta0True,round(mrIcGeo,2),'+-',
        round(sdIcGeo,2),'beta0True,mrIcGeo'))
print(' ---------------------------------------------------')
print('(5) Split into groups:')
print(' ---------------------------------------------------')
print('(9a) Split into two groups: Wald (1940) 1:1')
print(' ---------------------------------------------------')
fs = sort(x,index.return = TRUE)$ix; 
xs = x[fs]; ys = y[fs] # sort data
n1W = round(n/2); n2W = n-n1W+1 
(Length1 = length(xs[1:n1W]))
(Length2 = length(xs[n2W:n]))
if (Length1 != Length2) print('Error: groups differ in length')
xsc1 = mean(xs[1:n1W]); ysc1 = mean(ys[1:n1W])  # group mean values
xsc2 = mean(xs[n2W:n]); ysc2 = mean(ys[n2W:n])  # group mean values
SlopeWald = (ysc2-ysc1)/(xsc2-xsc1)
# slope through (xsc1,ysc1): (y-ysc1) = SlopeWald*(x-xsc1) 
# x = 0 --> IcWald
IcWald = ysc1-SlopeWald*xsc1
print(c(betaTrue,round(SlopeWald,2),'betaTrue,SlopeWald'))
print(c(beta0True,round(IcWald,2),'beta0True,IcWald'))
print(' ---------------------------------------------------')
print('(5b) Split into three groups: Bartlett (1949) 1:1:1')
print(' ---------------------------------------------------')
fs = sort(x,index.return = TRUE)$ix; xs = x[fs]; ys = y[fs] # sort data
n1B = round(n/3); n2B = n-n1B+1
(Length1 = length(xs[1:n1B]))
(Length2 = length(xs[n2B:n]))
if (Length1 != Length2) print('Error: groups differ in length')
xsc1 = mean(xs[1:n1B]); ysc1 = mean(ys[1:n1B]) # group mean values
xsc2 = mean(xs[n2B:n]); ysc2 = mean(ys[n2B:n]) # group mean values
SlopeBartlett = (ysc2-ysc1)/(xsc2-xsc1)
IcBartlett = ysc1-SlopeBartlett*xsc1
print(c(betaTrue,round(SlopeBartlett,2),'betaTrue,SlopeBartlett'))
print(c(beta0True,round(IcBartlett,2),'beta0True,IcBartlett'))
print(' ---------------------------------------------------')
print('(5c) Split into groups: Gibson & Jowett (1957) 1:2:1')
print(' ---------------------------------------------------')
fs = sort(x,index.return = TRUE)$ix; xs = x[fs]; ys = y[fs] # sort data
n1G = round(n/4); n2G = n-n1G+1
(Length1 = length(xs[1:n1G]))
(Length2 = length(xs[n2G:n]))
if (Length1 != Length2) print('Error: groups differ in length')
xsc1 = mean(xs[1:n1G]); ysc1 = mean(ys[1:n1G]) # group mean values
xsc2 = mean(xs[n2G:n]); ysc2 = mean(ys[n2G:n]) # group mean values
SlopeGibson = (ysc2-ysc1)/(xsc2-xsc1)
IcGibson = ysc1-SlopeGibson*xsc1
print(c(betaTrue,round(SlopeGibson,2),'betaTrue,SlopeGibson'))
print(c(beta0True,round(IcGibson,2),'beta0True,IcGibson'))
print(' ---------------------------------------------------')
print('(6) MLE with true lambda (Casalla & Berger, 2002):')
print(' ---------------------------------------------------')
lambda = lambdaTrue
# ----------------------------------------------------- added 3/2025
Sxx = sum((x-xmean)^2); Syy = sum((y-ymean)^2); 
Sxy = sum((x-xmean)*(y-ymean));
SlopeMLE=(-(Sxx-lambda*Syy)+sqrt((Sxx-lambda*Syy)^2+4*lambda*Sxy^2))/(2*lambda*Sxy)
IcMLE = yc-SlopeMLE*xc
print(c(betaTrue,round(SlopeMLE,2),'betaTrue,SlopeMLE'))
print(c(beta0True,round(IcMLE,2),'beta0True,IcMLE'))
# ----------------------------------------------------- 3/2025
# calculate R^2:
RsquaredFct = function(slope,intercept) {
  # Zar p.338-380; based on data (x,y)
  ymean = mean(y); totalSS = sum((y-ymean)^2); yEst = slope*x+intercept
  regressionSS = sum((yEst-ymean)^2); q = regressionSS/totalSS; return(q)}
RsquaredFct(SlopeMLE,IcMLE)
RsquaredSLR = RsquaredFct(SlopeYonX,IcYonX)
print(c(round(RsquaredSLR,5),'RsquaredSLR'))
print(' ---------------------------------------------------')
print('(7) Isobe et al. (1920):')
print(' ---------------------------------------------------')
# print('(7a) Isobe et al. (1920) OLS y on x:')
# print(' ---------------------------------------------------')
(beta1I90 = Sxy/Sxx) # OLS y on x
(beta2I90 = Syy/Sxy) # OLS x on y
(beta3I90 = 1/(beta1I90+beta2I90)*(beta1I90*beta2I90-1+
              sqrt((1+beta1I90^2)*(1+beta2I90^2)))) # bisect
(beta5I90 = sign(Sxy)*sqrt(beta1I90*beta2I90)) # geometric
# Variances:
(Var1I90 = (sum((x-xmean)^2*(y-beta1I90*x-ymean+beta1I90*xmean)^2))/Sxx^2)
(Var2I90 = (sum((y-ymean)^2*(y-beta2I90*x-ymean+beta2I90*xmean)^2))/Sxy^2)
(Cov12I90 = 1/(beta1I90*Sxx^2)*
    sum((x-xmean)*(y-ymean)*(y-ymean-beta1I90*(x-xmean))*
          (y-ymean-beta2I90*(x-xmean))))
(Var3I90 = beta3I90^2/((beta1I90+beta2I90)^2*(1+beta1I90^2)*
                        (1+beta2I90^2))*
  ((1+beta2I90^2)^2*Var1I90+
     2*(1+beta1I90^2)*(1+beta2I90^2)*Cov12I90+
     (1+beta1I90^2)^2*Var2I90))
(Var5I90 = (beta2I90/beta1I90*Var1I90+2*Cov12I90+
              beta1I90/beta2I90*Var2I90)/4)
# uncertainties (standard variances):
(sig1I90 = sqrt(Var1I90))
(sig2I90 = sqrt(Var2I90))
(sig3I90 = sqrt(Var3I90))
(sig5I90 = sqrt(Var5I90))
# print results:
print(c(betaTrue,round(beta1I90,4),'+-',round(sig1I90,4),
        'betaTrue,beta1I90'))
print(c(betaTrue,round(beta2I90,4),'+-',round(sig2I90,4),
        'betaTrue,beta2I90'))
print(c(betaTrue,round(beta3I90,4),'+-',round(sig3I90,4),
        'betaTrue,beta3I90'))
print(c(betaTrue,round(beta5I90,4),'+-',round(sig5I90,4),
        'betaTrue,beta5I90'))
# intercepts: IC = ymean - beta*xmean
(IC1I90 = ymean-beta1I90*xmean)
(IC2I90 = ymean-beta2I90*xmean)
(IC3I90 = ymean-beta3I90*xmean)
(IC5I90 = ymean-beta5I90*xmean)
print(c(beta0True,round(IC1I90,2),'beta0True,IC1I90'))
print(c(beta0True,round(IC2I90,2),'beta0True,IC2I90'))
print(c(beta0True,round(IC3I90,2),'beta0True,IC3I90'))
print(c(beta0True,round(IC5I90,2),'beta0True,IC5I90'))
print(' ---------------------------------------------------')
print('(8) MCMC (optional):')
print(' ---------------------------------------------------')
MCMCflag = 0
if (MCMCflag == 1) {
  print('----------------------------------------------------')
  print('(7) MCMC pedestrian:') 
  print('----------------------------------------------------')
  # Sum of squares / sigma^2 follows a chi-squred PDF with n degrees   DROP THESE LINES
  # of freedom; the mode of this chi-squred PDF 
  # is located at n-2 (for n > 2) -> choose prior values for sum of squares for 
  # x that are away from the mode
  print('1. Set t=0 and select a set of starting/initial parameter')
  print('   values beta, beta0 varx, vary:')
  t=0
  betaStart = 10  # Jitjareonchai et al. (2006, Fig.2 & 3)
  beta0Start = -35 # Jitjareonchai et al. (2006, Fig.2 & 3)
  M = 1e6 # Jitjareonchai et al. (2006, Table 3) and p.130: ''The results were obtained from a 
  # Gibbs Sampler run of 1 000 000 cycles ...''
  print(c(M,'M number of Monte Carlo runs')) 
  beta = betaStart; beta0 = beta0Start
  print(c(betaTrue,round(betaStart,2),'betaTrue,betaStart'))
  print(c(beta0True,round(beta0Start,2),'beta0True,beta0Start'))
  k = 4     # degrees of freedom for sum of squares; Jitjareonchai et al. (2006, p.129)
  nu = n+k  # degrees of freedom for chisq prior
  print(c(k,nu,'k,nu'))
  # -------------------------------------------------------------
  # Start values for varx, vary not specified in Jitjareonchai et al. (2006)!!!
  # -> the following values are my (DWG) choice:
  varx = varxTrue*0.4^2; vary = varyTrue*0.6^2
  Px = 14.7; Py = 44.3 # Jitjareonchai et al. (2006, p.129)
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
  print(c(betaTrue,round(betaArrMean,2),'+-',round(betaArrSD,2),
          'betaTrue,betaArrMean'))
  print(c(beta0True,round(beta0ArrMean,2),'+-',round(beta0ArrSD,2),
          'beta0True,beta0ArrMean'))
  print(' ---------------------------------------------------')
  print('(B) Cut out first 1.5% of time series (burn-in region):')
  print(' ---------------------------------------------------')
  m1 = round(0.015*M)+1; print(c(m1,'m1 (cut out + 1)'))
  betaArrMean = mean(betaArr[m1:M]); betaArrSD = sd(betaArr)
  beta0ArrMean = mean(beta0Arr[m1:M]); beta0ArrSD = sd(beta0Arr)
  print(c(betaTrue,round(betaArrMean,2),'+-',round(betaArrSD,2),
          'betaTrue,betaArrMean'))
  print(c(beta0True,round(beta0ArrMean,2),'+-',round(beta0ArrSD,2),
          'beta0True,beta0ArrMean'))
  betaArrMedian = median(betaArr[m1:M])
  beta0ArrMedian = median(beta0Arr[m1:M])
  print(c(betaTrue,round(betaArrMedian,2),
          'betaTrue,betaArrMedian'))
  print(c(beta0True,round(beta0ArrMedian,2),
          'beta0True,beta0ArrMedian'))
  # ---------------------------------------
  varxArrMean = mean(varxArr[m1:M]); varxArrSD = sd(varxArr[m1:M])
  varyArrMean = mean(varyArr[m1:M]); varyArrSD = sd(varyArr[m1:M])
  print(c(varxTrue,round(varxArrMean,2),'+-',round(varxArrSD,2),
          'varxTrue,varxMCMC'))
  print(c(varyTrue,round(varyArrMean,2),'+-',round(varyArrSD,2),
          'varyTrue,varyMCMC'))
  varxArrMedian = median(varxArr[m1:M]); varyArrMedian = median(varyArr[m1:M])
  print(c(round(varxArrMedian,2),round(varyArrMedian,2),
          'varxMedian,varyMedian'))
  LambdaMCMC = varxArrMean/varyArrMean
  print(c(lambdaTrue,round(LambdaMCMC,2),'lambdaTrue, LambdaMCMC'))
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
  print('MCMC estimates of (xii,yii) by Jitjareonchai et al. (2006, Table 2):')
  print(' ---------------------------------------------------')
  xiEstJ = c(18.8592,28.0712,25.8652,30.2092,13.7835,
             13.9688,17.212,13.4836,26.5858,12.4958)
  yiEstJ = c(70.3378,93.4900,87.9729,98.9411,57.5709,
             58.0115,66.1884,56.7921,89.8429,54.2846)
  print(' ---------------------------------------------------')
  print('(8) Mean squared error (MSE):')
  print(' ---------------------------------------------------')
  MSExiJ = sum((xiiTrue-xiEstJ)^2)/n
  print(c(round(MSExiJ,4),'MSExiJ (Table 2: 1.1871)'))
  MSEyiJ = sum((yiiTrue-yiEstJ)^2)/n
  print(c(round(MSEyiJ,4),'MSEyiJ (Table 2: 10.0112)'))
  # ---
  MSEx = sum((xiiTrue-x)^2)/n                        # raw data
  print(c(round(MSEx,4),'MSEx (Table 2: 1.2498)'))
  MSEy = sum((yiiTrue-y)^2)/n
  print(c(round(MSEy,4),'MSEy (Table 2: 12.2986)'))
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
  sflag = 0
  if (sflag == 2) {
    print('----------------------------------------------------')
    print('Fig.2 redrawn:')
    # png('Jitjareonchai06Fig2r241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(2,1))
    plot(betaArr[1:2000],type='p',lwd=1,col='blue',xlab='Cycle',ylab='Slope',
         las=1,cex=0.6,cex.lab=1.5)
    abline(v=400,lty=2,col='magenta')
    plot(beta0Arr[1:2000],type='p',lwd=1,col='blue',xlab='Cycle',ylab='Intercept',
         las=1,cex=0.6,cex.lab=1.5)
    abline(v=400,lty=2,col='magenta')
    # dev.off()
  }
  # ---------------------------------------------------------------------------------------------------------
  sflag = 0
  if (sflag == 201) {
    print('----------------------------------------------------')
    print('Fig.2, however, complete time series:')
    # png('Jitjareonchai06Fig2r241213c.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(2,1))
    plot(betaArr,type='p',lwd=1,col='blue',xlab='Cycle',ylab='Slope',
         las=1,cex=0.1,cex.lab=1.5)
    # abline(v=400,lty=2,col='magenta')
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
    # abline(v=400,lty=2,col='magenta')
    # dev.off()
}
# ---------------------------------------------------------------------------------------------------------
sflag = 0
if (sflag == 301) {
    print('----------------------------------------------------')
    print('Fig.3A redrawn:')
    # png('Jitjareonchai06Fig3Ar241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(betaArr[m1:M],from=1.5,to=4),lwd=3,col='blue',xlab='Slope',
         ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',ylim=c(0,2))
    dx = 0.01; beta2Arr = seq(1.5,4,dx)
    nuSLR = SLRyonx$df[2]; print(c(nuSLR,'degrees of freedom nuSLR'))
    sigmaEst = SLRyonx$sigma
    p2 = (nuSLR+sum((x-xmean)^2)/sigmaEst^2*
            (beta2Arr-SlopeYonX)^2)^(-(nuSLR+1)/2)
    qnorm2 = sum(p2)*dx
    p2 = p2/qnorm2 # normalize to 1
    lines(beta2Arr,p2,lwd=3,col='magenta',lty=2)
    abline(v=betaTrue,col='black',lty=1)
    abline(v=betaArrMean,col='blue',lty=2)
    # dev.off()
}
# ---------------------------------------------------------------
sflag = 0
if (sflag == 302) {
    print('----------------------------------------------------')
    print('Fig.3B redrawn:')
    # png('Jitjareonchai06Fig3Br241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    # ----- Intercept:
    beta1Arr = seq(0,40,0.01)
    plot(density(beta0Arr[m1:M],from=0,to=40),lwd=3,col='blue',xlab='Intercept',
         ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',ylim=c(0,0.1))
    p1 = (nuSLR+sum((x-xmean)^2)/(sigmaEst^2*sum(x^2)/n)*
            (beta1Arr-IcYonX)^2)^(-(nuSLR+1)/2)
    qnorm = sum(p1)*dx
    p1 = p1/qnorm # normalize to 1
    lines(beta1Arr,p1,lwd=3,col='magenta',lty=2)
    abline(v=beta0True,col='black',lty=1)
    abline(v=beta0ArrMean,col='blue',lty=2)
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
  if (sflag == 401) {
    print('Fig.4A redrawn:')
    # png('Jitjareonchai06Fig4Ar241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(xiiEst[m1:M,1],from=15,to=25),lwd=3,col='blue',xlab='xi1',
         ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 402) {
    print('Fig.4B redrawn:')
    # png('Jitjareonchai06Fig4Br241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(xiiEst[m1:M,2],from=20,to=35),lwd=3,col='blue',xlab='xi2',
         ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 403) {
    print('Fig.4C redrawn:')
    # png('Jitjareonchai06Fig4Cr241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(xiiEst[m1:M,3],from=20,to=30),lwd=3,col='blue',
         xlab='xi3',ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 404) {
    print('Fig.4D redrawn:')
    # png('Jitjareonchai06Fig4Dr241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(xiiEst[m1:M,4],from=25,to=35),lwd=3,col='blue',
         xlab='xi4',ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 405) {
    print('Fig.4E redrawn:')
    # png('Jitjareonchai06Fig4Er241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(xiiEst[m1:M,5],from=9,to=19),lwd=3,col='blue',
         xlab='xi5',ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 406) {
    print('Fig.4F redrawn:')
    # png('Jitjareonchai06Fig4Fr241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(xiiEst[m1:M,6],from=9,to=19),lwd=3,col='blue',
         xlab='xi6',ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    # dev.off()
}
# ---------------------------------------------------------
sflag = 4
if (sflag == 4) {
    print('Fig.4 redrawn:')
    # png('Jitjareonchai06Fig4r241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(3,2))
    plot(density(xiiEst[m1:M,1],from=15,to=25),lwd=3,col='blue',
         xlab='',ylab='',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    abline(v=xiiTrue[1],col='black',lty=2)
    abline(v=xiEst[1],col='magenta',lty=4)
    text(5,0.3,TeX('$\\xi_1$'),col='blue',pos=4)
    plot(density(xiiEst[m1:M,2],from=20,to=35),lwd=3,col='blue',
         xlab='',ylab='',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    abline(v=xiiTrue[2],col='black',lty=2)
    abline(v=xiEst[2],col='magenta',lty=4)
    text(5,0.3,TeX('$\\xi_2$'),col='blue',pos=4)
    plot(density(xiiEst[m1:M,3],from=20,to=30),lwd=3,col='blue',
         xlab='',ylab='',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    abline(v=xiiTrue[3],col='black',lty=2)
    abline(v=xiEst[3],col='magenta',lty=4)
    text(5,0.3,TeX('$\\xi_3$'),col='blue',pos=4)
    plot(density(xiiEst[m1:M,4],from=25,to=35),lwd=3,col='blue',
         xlab='',ylab='',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    abline(v=xiiTrue[4],col='black',lty=2)
    abline(v=xiEst[4],col='magenta',lty=4)
    text(5,0.3,TeX('$\\xi_4$'),col='blue',pos=4)
    plot(density(xiiEst[m1:M,5],from=9,to=19),lwd=3,col='blue',
         xlab='',ylab='',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    abline(v=xiiTrue[5],col='black',lty=2)
    abline(v=xiEst[5],col='magenta',lty=4)
    text(30,0.3,TeX('$\\xi_5$'),col='blue',pos=4)
    plot(density(xiiEst[m1:M,6],from=9,to=19),lwd=3,col='blue',
         xlab='',ylab='',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.4))
    abline(v=xiiTrue[6],col='black',lty=2)
    abline(v=xiEst[6],col='magenta',lty=4)
    text(30,0.3,TeX('$\\xi_6$'),col='blue',pos=4)
    # dev.off()
}
# ---------------------------------------------------------
sflag = 0
if (sflag == 5) { # confidence areas
    print('Fig.5 redrawn:')
    # library(jocre)
    # library(magrittr) # ?
    # library(dplyr) # ?
    # library(ggplot2)
    library(ConfidenceEllipse)
    Mshort = M # use all (M) or fewer data
    xEll = betaArr[m1:Mshort]; yEll = beta0Arr[m1:Mshort]; Ell=data.frame(xEll,yEll)
    names(Ell) # "xEll" "yEll"
    ellipse75=confidence_ellipse(Ell,x=xEll,y=yEll,conf_level=0.75)
    ellipse90=confidence_ellipse(Ell,x=xEll,y=yEll,conf_level=0.9)
    ellipse95=confidence_ellipse(Ell,x=xEll,y=yEll,conf_level=0.95)
    # png('Jitjareonchai06Fig5r241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(ellipse95,type='l',lwd=2,col='black',xlab=NA,ylab=NA,
         las=1,cex=0.6,cex.lab=1.5) #,xlim=c(1.5,4),ylim=c(-5,45))
    lines(ellipse75,lwd=2,col='red',lty=2)
    lines(ellipse90,lwd=2,col='blue',lty=3)
    title(xlab=TeX('Slope $\\beta$')) #,line=2.5)
    title(ylab=TeX('Intercept $\\beta_0$'),line=2.5)
    points(betaTrue,beta0True,col='black',lwd=4,cex=0.6,pch=20)
    # points(betaArrMedian,beta0ArrMedian,col='magenta',lwd=4,cex=0.6,pch=22)
    points(betaArrMean,beta0ArrMean,col='blue',lwd=4,cex=0.6,pch=24)
    text(2.3,33,'95%',col='black',cex.lab=1.5,pos=4)
    text(2.5,30,'90%',col='blue',cex.lab=1.5,pos=4)
    text(2.7,27,'75%',col='red',cex.lab=1.5,pos=4)
    # dev.off()
} 
# ---------------------------------------------------------------
sflag = 0
if (sflag == 601) {
    print('----------------------------------------------------')
    print('Fig.6A redrawn:')
    # png('Jitjareonchai06Fig6Ar241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(varxArr[m1:M],from=0.1,to=10),lwd=3,col='blue',xlab=NA,
         ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,10),ylim=c(0,0.4))
    abline(v=varxTrue,col='black',lty=2)
    abline(v=varxArrMean,col='magenta',lty=4)
    abline(v=varxArrMedian,col='green',lty=3)
    title(xlab=TeX('$\\sigma^2_x$'),cex.lab=1.5) #,line=2.5)
    # dev.off()
}
# ---------------------------------------------------------------
sflag = 0
if (sflag == 602) {
    print('----------------------------------------------------')
    print('Fig.6B redrawn:')
    # png('Jitjareonchai06Fig6Br241211.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(density(varyArr[m1:M],from=1,to=40),lwd=3,col='blue',xlab=NA,
         ylab='Density',las=1,cex=0.6,cex.lab=1.5,main='',
         xlim=c(0,40),ylim=c(0,0.1))
    abline(v=varyTrue,col='black',lty=2)
    abline(v=varyArrMean,col='magenta',lty=4)
    abline(v=varyArrMedian,col='green',lty=3)
    title(xlab=TeX('$\\sigma^2_y$'),cex.lab=1.5) #,line=2.5)
    # dev.off()
}
# ---------------------------------------------------------------------------------------------------------
# The following plots are not from Jitjareonchai et al. (2006)
# ---------------------------------------------------------------------------------------------------------
  xp = c(min(x,0),max(x))
  ypYonX = SlopeYonX*xp+IcYonX
sflag = 0
if (sflag == 10) { # cumulative mean (CM)
    betaCM = numeric(M)
    betaCM[1] = betaArr[1]
    for(j in 2:M) {
      betaCM[j] = (betaCM[j-1]*(j-1)+betaArr[j])/j
    }
    # png('JitjareonchaiCMbeta241213.png',width=16,height=12,units='cm',res=300)
    plot(betaCM,type='p',lwd=1,col='blue',xlab='#',
         ylab='Cumulative mean slope',las=1,cex=0.1,
         ylim=c(min(betaCM,betaTrue),max(betaCM,betaTrue)))
    abline(h=betaTrue,col='black',lty=1)
    abline(h=betaArrMean,col='magenta',lty=4)
    # dev.off()
}
# ---------------------------------------------------------------------------------------------------------
sflag = 0
if (sflag == 11) { # Chi-squared PDF
    z = seq(0.01,2*nu,0.01)
    # z = seq(0.01,2*nu,0.01) # before 12/2024
    chisq = dchisq(z,df=nu)
    zx = Px/varxTrue; zy = Py/varyTrue
    # png('Chisqxy241213.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(1,1))
    plot(z,chisq,type='l',lwd=4,col='blue',xlab='z',
         ylab=NA,las=1,cex=0.6,cex.lab=1.5)
    abline(v=zx,col='black',lty=2)
    abline(v=zy,col='magenta',lty=4)
    title(ylab=TeX('$\\chi^2_\\nu (z)$'),line=2.5)
    # dev.off()
}
# --------------------------------------------------------------
sflag = 0
if (sflag == 14) {  # plot data & lines
    ypYonX = SlopeYonX*xp+IcYonX
    ypYonX1 = SlopeYonX1*xp+IcYonX1
    # ypXonY = SlopeYonX1*xp+IcYonX1
    ypGeo = SlopeGeo*xp+IcGeo
    ypTrue = betaTrue*xp+beta0True
    ypMCMC = betaArrMean*xp+beta0ArrMean
    # png('Jitjareonchai06Lines241212.png',width=16,height=16,units='cm',res=300)
    plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,
         cex.lab=1.5,
         xlim=c(min(c(x,0)),max(x)),
         ylim=c(min(c(y,ypYonX1,0)),max(y)*1.2))
    # ylim=c(min(ypXonY),max(ypXonY))) xlim=c(0,17),
    lines(xp,ypYonX,col='orange',lwd=4,lty=2)
    lines(xp,ypYonX1,col='red',lty=4)
    # lines(xp,ypXonY,col='red',lty=4)
    lines(xp,ypGeo,lwd=2,col='magenta',lty=1)
    lines(xp,ypTrue,lwd=2,col='black')
    lines(xp,ypMCMC,lwd=2,col='green')
    # dev.off()
  } # end of sflag = 14
# ----------------------------------------------------------
sflag = 0
if (sflag == 91) {
  # xp = c(xsc1,xsc2); yp = c(ysc1,ysc2)
  xp = c(min(x,0),max(x))
  ypTrue = betaTrue*xp+beta0True
  ypWald = SlopeWald*xp+IcWald
  # png('Jitjareonchai06Wald241014DS1.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',
       las=1,cex=0.6,cex.lab=1.5,ylim=c(min(ypTrue),max(y)))
  points(xs[n2W:n],ys[n2W:n],col='magenta',lwd=4,cex=0.6)
  lines(xp,ypTrue,col='black',lwd=2)
  lines(xp,ypWald,col='orange',lwd=4)
  points(xsc1,ysc1,col='blue',lwd=4,cex=1.0,pch=4)
  points(xsc2,ysc2,col='magenta',lwd=4,cex=1.0,pch=4)
  # dev.off()
}
# ----------------------------------------------------------
sflag = 0
if (sflag == 92) {
  # xp = c(xsc1,xsc2); yp = c(ysc1,ysc2)
  xp = c(min(x,0),max(x))
  ypTrue = betaTrue*xp+beta0True
  ypBartlett = SlopeBartlett*xp+IcBartlett
  # png('Jitjareonchai06Bartlett241014DS1.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='black',xlab='x',ylab='y',
       las=1,cex=0.6,cex.lab=1.5,ylim=c(min(ypTrue),max(y)))
  points(xs[1:n1B],ys[1:n1B],col='blue',lwd=4,cex=0.6)
  points(xs[n2B:n],ys[n2B:n],col='magenta',lwd=4,cex=0.6)
  lines(xp,ypTrue,col='black',lwd=2)
  lines(xp,ypBartlett,col='orange',lwd=4)
  points(xsc1,ysc1,col='blue',lwd=4,cex=1.0,pch=4)
  points(xsc2,ysc2,col='magenta',lwd=4,cex=1.0,pch=4)
  # dev.off()
}
# ----------------------------------------------------------
sflag = 0
if (sflag == 93) {
  # xp = c(xsc1,xsc2); yp = c(ysc1,ysc2)
  xp = c(min(x,0),max(x))
  ypTrue = betaTrue*xp+beta0True
  ypGibson = SlopeGibson*xp+IcGibson
  # png('Jitjareonchai06Gibson241014DS1.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='black',xlab='x',ylab='y',
       las=1,cex=0.6,cex.lab=1.5,ylim=c(min(ypTrue),max(y)))
  points(xs[1:n1G],ys[1:n1G],col='blue',lwd=4,cex=0.6)
  points(xs[n2G:n],ys[n2G:n],col='magenta',lwd=4,cex=0.6)
  lines(xp,ypTrue,col='black',lwd=2)
  lines(xp,ypGibson,col='orange',lwd=4)
  points(xsc1,ysc1,col='blue',lwd=4,cex=1.0,pch=4)
  points(xsc2,ysc2,col='magenta',lwd=4,cex=1.0,pch=4)
  # dev.off()
}
} # end of MCMCflag
print(' ---------------------------------------------------')
print('MCMC (end)')
print(' ---------------------------------------------------')

