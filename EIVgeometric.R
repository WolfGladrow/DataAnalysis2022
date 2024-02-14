print('file: EIVgeometric.R')
print(date())
# purpose: errors in variables (EIV), geometric line 
# created by: Dieter.Wolf-Gladrow@awi.de  2/2024 version 1.0
# ---------------------------------------------------------
# (1) Generate artificial data that look a bit similar to N:P data (Redfield)
# -----------------------------------------------------------
  set.seed(1953)
  SlopeTrue = 15 # true slope (N:P in mol/mol)
  IcTrue = -2.5    # true intercept not equal 0 (negative -> P is > 0 at N=0)
  n = 30           # sample size
  sigx = 0.5
  sigy = 5*sigx # sigy/sigx is not equal to 1 or equal to SlopeTrue
  # (1a) true xi (<-> PO4) values from a normal distribution:
  ximean = 1.9    # mean x concentration
  xisigma = 0.7   # standard deviation
  xi = rnorm(n,ximean,xisigma)
  # (1b) true zeta (<-> NO3) values
  zeta = SlopeTrue*xi+IcTrue
  # (1c) add normal noise (independent from each other) with mean 0:
  errx = rnorm(n,0,sigx)
  erry = rnorm(n,0,sigy)
  x = xi+errx     # 'observed' values
  y = zeta+erry   # 'observed' values
  # -----------------------------------------------------------
  # (2) Simple linear regression (SLR): y on x
  # -----------------------------------------------------------
  SLRyonx = lm(y ~ x)
  SlopeYonX = as.numeric(SLRyonx$coefficients[2])
  IcYonX = as.numeric(SLRyonx$coefficients[1])
  # -----------------------------------------------------------
  # (3)  Simple linear regression (SLR): x on y
  # -----------------------------------------------------------
  SLRyonx = lm(x ~ y)
  b = as.numeric(SLRyonx$coefficients[2])
  a = as.numeric(SLRyonx$coefficients[1])
  # x=b*y+a -> y = (x-a)/b -> slope = 1/b; 
  SlopeXonY = 1/b
  IcXonY = -a/b
  # -----------------------------------------------------------
  # (4) Geometric mean of regression slopes:
  # -----------------------------------------------------------
  SlopeGeo = sqrt(SlopeYonX*SlopeXonY)
  # centroid (Schwerpunkt)
  xc = mean(x); yc = mean(y)
  # line with slope SlopeGeo through centroid:
  # y-yc = SlopeGeo*(x-xc)
  # x = 0 -> y = IcGeo = yc-SlopeGeo*xc
  IcGeo = yc-SlopeGeo*xc
  # -----------------------------------------------------------
  # (5) Plot data & regression lines:
  # -----------------------------------------------------------
  sflag = 1
  if (sflag == 1) {
  # png('EIVgeometric240214.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  xp = c(min(x),max(x))
  yRyonx = SlopeYonX*xp+IcYonX 
  lines(xp,yRyonx,col='black',lwd=2,lty=2)
  yRxony = SlopeXonY*xp+IcXonY 
  lines(xp,yRxony,col='red',lwd=2,lty=4)
  yGeo = SlopeGeo*xp+IcGeo 
  lines(xp,yGeo,col='magenta',lwd=4,lty=1)
  b0r = round(IcGeo,2)
  text(0,47,bquote(~hat(beta)[0]  == .(b0r)),col='magenta',pos=4,cex=1.5)
  br = round(SlopeGeo,2)
  text(1.3,47,bquote(~hat(beta)  == .(br)),col='magenta',pos=4,cex=1.5)
  b0r = round(IcYonX,2)
  text(0,38,bquote(~hat(beta)[0]  == .(b0r)),col='black',pos=4,cex=1.5)
  br = round(SlopeYonX,2)
  text(1.3,38,bquote(~hat(beta)  == .(br)),col='black',pos=4,cex=1.5)
  b0r = round(IcXonY,2)
  text(1.4,5,bquote(~hat(beta)[0]  == .(b0r)),col='red',pos=4,cex=1.5)
  br = round(SlopeXonY,2)
  text(2.6,5,bquote(~hat(beta)  == .(br)),col='red',pos=4,cex=1.5)
  # dev.off()
  }