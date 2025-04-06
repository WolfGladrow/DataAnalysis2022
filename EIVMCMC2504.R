print('file: EIVMCMC2504.R')
print(date())
# history: R_Jitjareonchai06MCMC2503.R
# purpose: Errors-In-Variables (EIV)
# analyse example Jitjareonchai et al. (2006) using SLR to MCMC
# created by: Dieter.Wolf-Gladrow@awi.de 
#    12/2024 version 1.0
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
# (SoSx = sum((x-xiiTrueMean)^2))
# (SoSy = sum((y-yiiTrueMean)^2))
# SoSxy = sum((x-xiiTrueMean)*(y-yiiTrueMean))
# print(c(round(SoSx,4),round(SoSy,4),round(SoSxy,4),'SoSx,SoSy,SoSxy'))
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
print('(2) MCMC:')
print(' ---------------------------------------------------')
# MCMCflag = 1
# if (MCMCflag == 1) {
#   print('----------------------------------------------------')
#   print('MCMC pedestrian:') 
#   print('----------------------------------------------------')
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
  # M = 1e4 # test for speed up
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
  set.seed(1953)
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
  print(c(round(lambdaTrue,4),round(LambdaMCMC,2),'lambdaTrue, LambdaMCMC'))
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
  print('(3) Plots:')
  print('----------------------------------------------------')
  library(latex2exp)
  par(mfrow=c(1,1))
  sflag = 2
  if (sflag == 1) { 
    print('----------------------------------------------------')
    print('(3a) plot Chi-squared PDF')
    print('----------------------------------------------------')
    z = seq(0.01,2*nu,0.01)
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
  # ---------------------------------------------------------------------------------------------------------
  # sflag = 0
  if (sflag == 2) {
    print('----------------------------------------------------')
    print('Jitjareonchai06 Fig.2 redrawn:')
    print('----------------------------------------------------')
    # png('Out_Jitjareonchai06Fig2r241211.png',width=16,height=16,units='cm',res=300)
    # png('Jitjareonchai06Fig2r250406.png',width=16,height=16,units='cm',res=300)
    par(mfrow=c(2,1))
    plot(betaArr[1:2000],type='p',lwd=1,col='blue',xlab='Cycle',ylab='Slope',
         las=1,cex=0.6,cex.lab=1.5)
    abline(v=400,lty=2,col='magenta')
    plot(beta0Arr[1:2000],type='p',lwd=1,col='blue',xlab='Cycle',ylab='Intercept',
         las=1,cex=0.6,cex.lab=1.5)
    abline(v=400,lty=2,col='magenta')
    # dev.off()
    par(mfrow=c(1,1))
  }
  # ---------------------------------------------------------------------------------------------------------
  # sflag = 0
  if (sflag == 3) {
    print('----------------------------------------------------')
    print('Jitjareonchai06 Fig.2, however, complete time series:')
    print('----------------------------------------------------')
    # png('Out_Jitjareonchai06Fig2r241213c.png',width=16,height=16,units='cm',res=300)
    # png('Jitjareonchai06Fig2r250406c.png',width=16,height=16,units='cm',res=300)
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
    # dev.off()
    par(mfrow=c(1,1))
}
# ---------------------------------------------------------------------------------------------------------
# sflag = 0
if (sflag == 4) {
    SLRyonx = summary(lm(y~x))
    (SlopeYonX = SLRyonx$coefficients[2])
    print('----------------------------------------------------')
    print('Jitjareonchai06 Fig.3A redrawn')
    print('----------------------------------------------------')
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
 #   dev.off()
}
# ---------------------------------------------------------------
# sflag = 0
if (sflag == 5) {
  SLRyonx = summary(lm(y~x))
  (IcYonX = SLRyonx$coefficients[1])
  print('----------------------------------------------------')
  print('Jitjareonchai06 Fig.3B redrawn')
  print('----------------------------------------------------')
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
  # sflag = 0
  if (sflag == 6) { 
    print('----------------------------------------------------')
    print('Jitjareonchai06 Fig.5 redrawn: confidence areas')
    print('----------------------------------------------------')
    # install.packages('ConfidenceEllipse')
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
    points(betaArrMean,beta0ArrMean,col='blue',lwd=4,cex=0.6,pch=24)
    text(2.3,33,'95%',col='black',cex.lab=1.5,pos=4)
    text(2.5,30,'90%',col='blue',cex.lab=1.5,pos=4)
    text(2.7,27,'75%',col='red',cex.lab=1.5,pos=4)
    # dev.off()
} 
# ---------------------------------------------------------
# sflag = 0
if (sflag == 7) {
  print('----------------------------------------------------')
  print('Jitjareonchai06 Fig.4 redrawn:')
  print('----------------------------------------------------')
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
# sflag = 0
if (sflag == 8) {
  print('----------------------------------------------------')
  print('Jitjareonchai06 Fig.6A redrawn:')
  print('----------------------------------------------------')
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
# sflag = 0
if (sflag == 9) {
  print('----------------------------------------------------')
  print('Jitjareonchai06 Fig.6B redrawn:')
  print('----------------------------------------------------')
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
print(' ---------------------------------------------------')
print('MCMC (end)')
print(' ---------------------------------------------------')
# 
# [1] "file: R_EIV_MCMC_2504.R"
# [1] "Sun Apr  6 16:58:35 2025"
# [1] " ---------------------------------------------------"
# [1] "(1) Jitjareonchai et al. (2006, Table 2): true model parameters & data"
# [1] " ---------------------------------------------------"
# [1] "3"       "true slope, betaTrue = alpha*"
# [1] "10"      "true intercept, beta0True = beta*"
# [1] "4" "9"   "varxTrue,varyTrue"
# [1] "10"      "sample size n"
# [1] "20.4694" "71.4081" "xiiTrueMean,yiiTrueMean"
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
# [1] "0.4444"                                "lambdaTrue"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "396.4405"    "2998.9234"   "1064.6057"   "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (begin):"
# [1] " ---------------------------------------------------"
# [1] "1.9957"  "1.7568"  "meanxDiff,medianxDiff"
# [1] " ---------------------------------------------------"
# [1] "Diagnostic: mean distances of x-values (end)"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(2) MCMC:"
# [1] " ---------------------------------------------------"
# [1] "1. Set t=0 and select a set of starting/initial parameter"
# [1] "   values beta, beta0 varx, vary:"
# [1] "1e+06"   "M number of Monte Carlo runs"
# [1] "3"   "10"        "betaTrue,betaStart"
# [1] "10"  "-35"       "beta0True,beta0Start"
# [1] "4"    "14"       "k,nu"
# [1] "4"     "0.64"    "varxTrue,varx (prior value)"
# [1] "9"     "3.24"    "varyTrue,vary (prior value)"
# [1] "14.7"  "44.3"    "Px, Py (prior value)"
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
# [1] "3"    "2.64" "+-" "0.3"  "betaTrue,betaArrMean"
# [1] "10"  "20.33" "+-" "6.06" "beta0True,beta0ArrMean"
# [1] " ---------------------------------------------------"
# [1] "(B) Cut out first 1.5% of time series (burn-in region):"
# [1] " ---------------------------------------------------"
# [1] "15001"            "m1 (cut out + 1)"
# [1]  "3"  "2.64" "+-" "0.3"   "betaTrue,betaArrMean"
# [1] "10" "20.34" "+-" "6.06"  "beta0True,beta0ArrMean"
# [1]  "3"   "2.63"  "betaTrue,betaArrMedian"
# [1] "10"  "20.54"  "beta0True,beta0ArrMedian"
# [1]  "4"   "3.15" "+-" "1.94" "varxTrue,varxMCMC"
# [1]  "9"  "12.08" "+-" "8.39" "varyTrue,varyMCMC"
# [1]  "2.67"  "9.85" "varxMedian,varyMedian"
# [1] "0.4444"  "0.26"  "lambdaTrue, LambdaMCMC"
# [1] "18.8719" "27.8795" "25.7495" "30.0524" "13.9042" "14.068"  "17.2546"
# [8] "13.5963" "26.5295" "12.5999" "xiEst"  
# [1] "70.2537" "93.896"  "88.302"  "99.5902" "57.2147" "57.647"  "66.0097"
# [8] "56.4086" "90.3415" "53.7964" "yiEst"  
# [1] "18.8784"   "27.83"     "25.7183"   "30.0126"   "13.9407"   "14.0978"  
# [7] "17.2707"   "13.6309"   "26.5026"   "12.6339"   "xiiMedian"
# [1] "70.2685"   "94.0667"   "88.353"    "99.5772"   "57.1506"   "57.6353"  
# [7] "66.0104"   "56.3786"   "90.215"    "53.8276"   "yiiMedian"
# [1] " ---------------------------------------------------"
# [1] "MCMC estimates of (xii,yii) by Jitjareonchai et al. (2006, Table 2):"
# [1] " ---------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "(8) Mean squared error (MSE):"
# [1] " ---------------------------------------------------"
# [1]  "1.1871"   "MSExiJ (Table 2: 1.1871)"
# [1] "10.0112"   "MSEyiJ (Table 2: 10.0112)"
# [1]  "1.2498"   "MSEx   (Table 2: 1.2498)"
# [1] "12.2986"   "MSEy   (Table 2: 12.2986)"
# [1]  "1.0417"   "MSExi" 
# [1]  "9.211"    "MSEyi"
# [1] "----------------------------------------------------"
# [1] "(3) Plots:"
# [1] "----------------------------------------------------"
# [1] " ---------------------------------------------------"
# [1] "MCMC (end)"
# [1] " ---------------------------------------------------"
# 
