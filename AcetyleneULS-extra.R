print('file: AcetyleneULS-extra.R')
# extrapolation contour plot: Unit Length Scaling (MP82)
# Acetylene data: MATLAB 'load acetylene' or Marquardt & Snee (1975) or ...
Yo = c(49.0,50.2,50.5,48.5,47.5,44.5,28.0,31.5,34.5,35.0,38.0,38.5,15.0,17.0,20.5,29.5)
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
X4o = X1o*X2o; X5o = X1o*X3o; X6o = X2o*X3o
X7o = X1o^2; X8o = X2o^2; X9o = X3o^2
L = length(X1o)
# unit length scaling
n = 9  # number of predictors
Q9 = matrix(data=c(X1o,X2o,X3o,X4o,X5o,X6o,X7o,X8o,X9o),nrow=L,ncol=9)
# Unit length scaling = subtract mean (centering) and divide by square root of sum of squares
XDWG = matrix(data=NA,nrow=L,ncol=n)
meanx = numeric(n); SXj = numeric(n)
for(j in 1:9) {q = Q9[,j]; xj = (q-mean(q))/sqrt(sum((q-mean(q))^2)); 
XDWG[,j] = xj; meanx[j] = mean(q);
SXj[j] = sqrt(sum( (q-mean(q))^2 ))}
y = (Yo-mean(Yo))/sqrt(sum((Yo-mean(Yo))^2));
# Alternative scaling: scale X1,X2,X3 as before -> x1,x2,x3
#   and calculate x4=scale(x1*x2) = Montgomery & Peck (1982) scaling
x1 = XDWG[,1]; x2 = XDWG[,2]; x3 = XDWG[,3];
XMP82 = XDWG
x4u=x1*x2; x4=(x4u-mean(x4u))/sqrt(sum((x4u-mean(x4u))^2)); XMP82[,4]=x4
x5u=x1*x3; x5=(x5u-mean(x5u))/sqrt(sum((x5u-mean(x5u))^2)); XMP82[,5]=x5
x6u=x2*x3; x6=(x6u-mean(x6u))/sqrt(sum((x6u-mean(x6u))^2)); XMP82[,6]=x6
x7u=x1^2;  x7=(x7u-mean(x7u))/sqrt(sum((x7u-mean(x7u))^2)); XMP82[,7]=x7
x8u=x2^2;  x8=(x8u-mean(x8u))/sqrt(sum((x8u-mean(x8u))^2)); XMP82[,8]=x8
x9u=x3^2;  x9=(x9u-mean(x9u))/sqrt(sum((x9u-mean(x9u))^2)); XMP82[,9]=x9
# Scale factors:
Xmean = numeric(9); SX = numeric(9);
Xmean[1] = mean(X1o); Xmean[2] = mean(X2o); Xmean[3] = mean(X3o)
SX[1] = sqrt(sum( (X1o-Xmean[1])^2 ))
SX[2] = sqrt(sum( (X2o-Xmean[2])^2 ))
SX[3] = sqrt(sum( (X3o-Xmean[3])^2 ))
Xmean[4] = mean(x4u); Xmean[5] = mean(x5u); Xmean[6] = mean(x6u)
SX[4] = sqrt(sum( (x4u-Xmean[4])^2 ))
SX[5] = sqrt(sum( (x5u-Xmean[5])^2 ))
SX[6] = sqrt(sum( (x6u-Xmean[6])^2 ))
Xmean[7] = mean(x7u); Xmean[8] = mean(x8u); Xmean[9] = mean(x9u)
SX[7] = sqrt(sum( (x7u-Xmean[7])^2 ))
SX[8] = sqrt(sum( (x8u-Xmean[8])^2 ))
SX[9] = sqrt(sum( (x9u-Xmean[9])^2 ))
Sx1x2 = sqrt(sum((x4u-mean(x4u))^2))
Sx1x3 = sqrt(sum((x5u-mean(x5u))^2))
Sx2x3 = sqrt(sum((x6u-mean(x6u))^2))
Sx1x1 = sqrt(sum((x7u-mean(x7u))^2))
Sx2x2 = sqrt(sum((x8u-mean(x8u))^2))
Sx3x3 = sqrt(sum((x9u-mean(x9u))^2))
Ymean = mean(Yo)
SY = sqrt(sum( (Yo-Ymean)^2 ))
X = XMP82; print('Montgomery and Peck (1982) scaling')
# (1) Estimate slopes & intercept:
k = 0       # k=0 <-> no ridge regression
# (0) Correct input? & size of input
mn = dim(X)
m = mn[1] # m = number of observations (per predictor)
n = mn[2] # n = number of predictors
# if (m != my) error('x and y must have same number of observations')
# (2) Correlation matrix X'X:
XX = t(X)%*%X
# (3) Solve linear system
IM = diag(n)   # identity matrix (n times n)
IXXk = solve(XX+k*IM)
dflag = 1   # display flag
if (dflag == 1) {
  print('(3) Collinearity diagnostics:')
  print('(3a) Inspection of correlation matrix XX:')
  print('Correlation matrix XX')
  print(round(XX,4))
  print('Minimum of XX = ')
  print(min(XX))  # should be >= -1
  print('Maximum of XX = ')
  print(max(XX))  # should be 1
  print('     find maximum of magnitude of off-diagonal elements')
  offdiagmax = -1; ioff = 0; joff = 0 # dummy values
  for(i in 1:n) {
    for(j in 1:n) {if(i != j) {
      if (abs(XX[i,j]) > offdiagmax) {
        offdiagmax = abs(XX[i,j]); ioff = i; joff = j}}}
  }
  print('Correlation with largest magnitude = ')
  print(XX[ioff,joff])
  print(c(ioff,joff))
  print('(3b) Variance inflation factors (VIFs)')
  print('      = diagonal elements of XX^-1 = IXX')
  print(round(diag(IXXk)))
  print(diag(IXXk))
  print('(3c) Condition number of XX')
  print('      = kappa = lambda_max/lambda_min')
  print('      where the lambdas are the eigenvalues of XX')
  out2 = eigen(XX,symmetric=T)
  lambda = out2$values  # eigen values
  kappa = max(lambda)/min(lambda)
  print('eigenvalues of XX:')
  print(lambda)
  print('kappa = ')
  print(kappa)
  print('Inverse of XX+k*IM =')
  print(round(IXXk))  # print(IXXk)
}
bs = numeric(n); b = numeric(n)
bs = IXXk%*%(t(X)%*%y)   # estimated slopes for scaled data
# (4) min & max of X1o, X2o, X3o
X1min = min(X1o); X1max = max(X1o)
X2min = min(X2o); X2max = max(X2o)
X3min = min(X3o); X3max = max(X3o)
# ---------------------------------------------------------
# (5) choose combination of min/max values:
dX1 = (X1max-X1min)/100; dX3 = (X3max-X3min)/100;
U1a = seq(X1min,X1max,dX1); LU1a = length(U1a)
U2 = round(mean(X2o),1)
print(c('U2 = ',U2))
# unit length scaling of U2:
s2 = (U2-Xmean[2])/SX[2]
U3a = seq(X3min,X3max,dX3); LU3a = length(U3a)
Yex = matrix(data=NA,nrow=LU1a,ncol=LU3a)
for(i in 1:LU1a) {
  for(j in 1:LU3a) {U1 = U1a[i]; U3 = U3a[j];
  # unit length scaling of U1, U3:
  s1 = (U1-Xmean[1])/SX[1]; s3 = (U3-Xmean[3])/SX[3]; 
  # (4) calculate corresponding values for predictors 4 to 9:
  U4 = s1*s2; U5 = s1*s3; U6 = s2*s3; U7 = s1^2; U8 = s2^2; U9 = s3^2;
  # unit length scaling of U4, ..., U9:
  s4 = (U4-Xmean[4])/SX[4]; s5 = (U5-Xmean[5])/SX[5]; s6 = (U6-Xmean[6])/SX[6];
  s7 = (U7-Xmean[7])/SX[7]; s8 = (U8-Xmean[8])/SX[8]; s9 = (U9-Xmean[9])/SX[9];
  # (5) predict response
  s = c(s1,s2,s3,s4,s5,s6,s7,s8,s9)
  yex = t(s)%*%bs
  Yex[i,j] = yex*SY+Ymean
  # U = c(U1,U2,U3,U4,U5,U6,U7,U8,U9);
  # Yex[i,j] = t(U)%*%b+b0 
  }}
print(c('Yex[1,1] =',Yex[1,1]))
# png('Acetylene9MLRextraMP82s170620.png',width=16,height=16,units='cm',res=300)
contour(U1a,U3a,Yex,levels=c(-60,-40,-20,0,20,30,40),
        col=c('red','red','red','black','blue','blue','blue'),
        las=0,xlab=TeX('$X_1(^o C)$'),ylab=NA,cex.lab=1.5)
title(ylab=TeX('$X_3(s)$'),line=2.5,cex.lab=1.5)
# dev.off()