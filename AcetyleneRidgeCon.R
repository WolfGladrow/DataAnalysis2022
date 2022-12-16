print('file: AcetyleneRidgeCon.R')
print(date())
# extrapolation contour plot: use ridgeDWG170s6.R
# Acetylene data: MATLAB 'load acetylene' or Marquardt & Snee (1975) or ...
Yo = c(49.0,50.2,50.5,48.5,47.5,44.5,28.0,31.5,34.5,35.0,38.0,38.5,15.0,17.0,20.5,29.5)
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
X4o = X1o*X2o; X5o = X1o*X3o; X6o = X2o*X3o
X7o = X1o^2; X8o = X2o^2; X9o = X3o^2
L = length(X1o)  # number of data per predictor
# (1) Estimate slopes & intercept:
k = 0; dflag = 0
Xo = matrix(data=c(X1o,X2o,X3o,X4o,X5o,X6o,X7o,X8o,X9o),nrow=16,ncol=9)
source('ridgeDWG1706s.R')
out = ridgeDWG1706s(Yo,Xo,dflag,k)
n = out[[1]]  # number of predictors
b = out[[2]]  # slopes for original data
b0 = out[[3]] # intercept for original data 
# (2) min & max of X1o, X2o, X3o
X1min = min(X1o); X1max = max(X1o)
X2min = min(X2o); X2max = max(X2o)
X3min = min(X3o); X3max = max(X3o)
# ---------------------------------------------------------
# (3) choose combination of min/max values:
dX1 = (X1max-X1min)/100; dX3 = (X3max-X3min)/100;
U1a = seq(X1min,X1max,dX1); LU1a = length(U1a)
U2 = round(mean(X2o),1)
print(c('U2 = ',U2))
U3a = seq(X3min,X3max,dX3); LU3a = length(U3a)
Yex = matrix(data=NA,nrow=LU1a,ncol=LU3a)
for(i in 1:LU1a) {
  for(j in 1:LU3a) {U1 = U1a[i]; U3 = U3a[j];
  # (4) calculate corresponding values for predictors 4 to 9:
  U4 = U1*U2; U5 = U1*U3; U6 = U2*U3; U7 = U1^2; U8 = U2^2; U9 = U3^2;
  # (5) predict response
  U = c(U1,U2,U3,U4,U5,U6,U7,U8,U9);
  Yex[i,j] = t(U)%*%b+b0 
  }}
print(c('Yex[1,1] =',Yex[1,1]))
library(latex2exp)
# png('Acetylene9MLRextra170616.png',width=16,height=16,units='cm',res=300)
contour(U1a,U3a,Yex,levels=c(-60,-40,-20,0,20,30,40),
        col=c('red','red','red','black','blue','blue','blue'),
        las=0,xlab=TeX('$X_1 (^o C)$'),
        ylab=NA,cex.lab=1.5)
title(ylab=TeX('$X_3 (s)$'),line=2.5,cex.lab=1.5)
# dev.off()