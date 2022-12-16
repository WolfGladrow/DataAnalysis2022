print('file: AcetyleneRidgeTrace.R')
print(date())
# acetylene data: X1, X3 anticorrelated
# -------------------------------------------
# (1) = observed data (Yo,X1o,X2o,X3o) and quadratic predictors
Yo = c(49.0,50.2,50.5,48.5,47.5,44.5,28.0,31.5,34.5,35.0,38.0,38.5,15.0,17.0,20.5,29.5)
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
X4o = X1o*X2o; X5o = X1o*X3o; X6o = X2o*X3o
X7o = X1o^2; X8o = X2o^2; X9o = X3o^2
LYo = length(Yo)   # sample size
NoP = 9            # number of predictors
# -------------------------------------------
# (2) Unit length scaling:
Xo = matrix(data=c(X1o,X2o,X3o,X4o,X5o,X6o,X7o,X8o,X9o),nrow=LYo,ncol=NoP)
# Unit length scaling = subtract mean (centering) and divide by square root of sum of squares
X = matrix(data=NA,nrow=LYo,ncol=NoP)
n = NoP
meanX = numeric(n); SX = numeric(n)
for(j in 1:NoP) {q = Xo[,j]; xj = (q-mean(q))/sqrt(sum((q-mean(q))^2)); 
X[,j] = xj; meanX[j] = mean(q); SX[j] = sqrt(sum( (q-mean(q))^2 ))}
meanY = mean(Yo)
SY = sqrt(sum((Yo-meanY)^2))
y = (Yo-meanY)/SY
# -------------------------------------------
# (3a) MLR of original data using lm():
outLMo = lm(Yo ~ Xo)
InterceptEsto = outLMo$coefficients[1]
bEsto = outLMo$coefficients[2:(NoP+1)]
# (3b) MLR of scaled data using lm():
outLMs = lm(y ~ X)
betaEst1 = outLMs$coefficients[2:(NoP+1)]
# (3c) MLR of scaled data: pedestrian way
XX = t(X)%*%X             # correlation matrix
XXI = solve(XX)           # inverse of X'X
betaEst2 = XXI%*%t(X)%*%y  # least squares solution
# -------------------------------------------
# (4) Convert from slopes for scaled data to slopes for original data:
bA = betaEst2/SX
cA = -betaEst2*meanX/SX
bEst2 = betaEst2/SX*SY
InterceptEst2 = sum(cA)*SY+meanY # intercept for original data
# -------------------------------------------
# (5) Multicollinearity diagnostics:
# (5a) Largest absolute correlation:
offdiagmax = -1; ioff = 0; joff = 0 # dummy values
for(i in 1:n) {
  for(j in 1:n) {if(i != j) {
    if (abs(XX[i,j]) > offdiagmax) {
      offdiagmax = abs(XX[i,j]); ioff = i; joff = j}}}
}
# -------------------------------------------
# (5b) Variance inflation factors (VIFs):
VIFs = diag(XXI)
# How many VIFs are > 10? Maximum of the VIFs?
nVIFsL10 = 0
for(j in 1:NoP) if (VIFs[j] > 10) nVIFsL10 = nVIFsL10 + 1
maxVIFs = max(VIFs)
# -------------------------------------------
# (5c) Eigensystem analysis of the correlation matrix XX
#      -> eigenvalues lambda_j
#      -> condition number: kappa = lambda_max/lambda_min
outEV = eigen(XX,symmetric=T)
lambda = outEV$values  # eigen values
kappa = max(lambda)/min(lambda)
# -------------------------------------------
# (5d) Condition number of X'
outSVD = svd(X)
muj = outSVD$d   # singular values
eta = max(muj)/min(muj)
# -------------------------------------------
r13o = cor(X1o,X3o) # correlation
# -------------------------------------------
sflag = 4
if (sflag == 4) {
  print('-------------------------------------------')
  print('Plot: ridge trace')
  source('ridgeDWG1706s.R')
  # ----- k-loop:
  ka = seq(0.001,0.5,0.001); Lka = length(ka)
  beta1a = numeric(Lka); beta2a = numeric(Lka); beta3a = numeric(Lka)
  beta4a = numeric(Lka); beta5a = numeric(Lka); beta6a = numeric(Lka)
  beta7a = numeric(Lka); beta8a = numeric(Lka); beta9a = numeric(Lka)
  rsqa = numeric(Lka)
  dflag = 0
  for (m in 1:Lka) { k = ka[m];
  out = ridgeDWG1706s(Yo,Xo,dflag,k);
  betaEstoRR = out[[2]];   # slopes for original data
  InterceptoRR = out[[3]]; # intercept for original data
  betaEstRR = out[[4]];  # slopes for scaled data
  bs = betaEstRR
  beta1a[m] = bs[1]; beta2a[m] = bs[2]; beta3a[m] = bs[3];
  beta4a[m] = bs[4]; beta5a[m] = bs[5]; beta6a[m] = bs[6];
  beta7a[m] = bs[7]; beta8a[m] = bs[8]; beta9a[m] = bs[9];
  # Prediction: 
  YPred = Xo%*%betaEstoRR+InterceptoRR
  r = cor(Yo,YPred); rsqa[m] = r^2
  }
  sflag1 = 1
  if (sflag1 == 1) {
    library(latex2exp)
    #  png('AcetyleneRidgeTraceDWGsc170625.png',width=16,height=16,units='cm',res=300)
    plot(ka,beta1a,type='l',lwd=1,col='blue',xlab='Biasing parameter k',
         ylab=NA,las=1,cex=0.4,xlim=c(0,0.6),ylim=c(-0.7,0.7),
         cex.lab=1.5)
    xt = 0.54
    title(ylab=TeX('$\\hat{\\beta}_j$'),line=2.5,cex.lab=1.5)
    text(xt,0.2,expression(hat(beta)[1]),col='blue',cex=1.5)
    lines(ka,beta2a,col='red'); 
    text(0.1,0.18,expression(hat(beta)[2]),col='red',cex=1.5)
    lines(ka,beta3a,col='black',lty=1); 
    text(xt,-0.2,expression(hat(beta)[3]),col='black',cex=1.5)
    lines(ka,beta4a,col='blue',lty=2); 
    text(xt,0.06,expression(hat(beta)[4]),col='blue',cex=1.5)
    lines(ka,beta5a,col='blue',lty=4); 
    text(0.05,beta5a[Lka]-0.2,expression(hat(beta)[5]),col='blue',cex=1.5)
    lines(ka,beta6a,col='black'); 
    text(0.02,0.65,expression(hat(beta)[6]),col='black',cex=1.5)
    lines(ka,beta7a,col='magenta',lty=4); 
    text(xt,0.32,expression(hat(beta)[7]),col='magenta',cex=1.5)
    lines(ka,beta8a,col='red',lty=3); 
    text(xt,beta8a[Lka]-0.05,expression(hat(beta)[8]),col='red',cex=1.5)
    lines(ka,beta9a,col='black',lty=2); 
    text(0.05,-0.08,expression(hat(beta)[9]),col='black',cex=1.5)
    # dev.off()
  }   # end of sflag1 == 1
  if (sflag1 == 2) {
    # png('AcetyleneRidgeTraceDWGrsq150625.png',width=16,height=16,units='cm',res=300)
    plot(ka,rsqa,type='l',lwd=5,col='blue',xlab='Biasing parameter k',
         ylab='Coefficient of determination',las=1,cex.lab=1.5)
    # dev.off()
  }  
}
# ------------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Remarks:
# Calculation of the correlation matrix: XX = t(X)%*%X
#   t(X) is the transpose of the matrix X, i.e. all elements x_ij replaced by x_ji
#   The product between two matrices (or matrix and vector) is performed by 
#   applying %*%: C = A%*%C. When A is a k times n matrix and B is A n times m
#   matrix, C is a k times m matrix (when m = 1, B is a vector of length n).
# Inverse matrix:
#   The R routine solve() calculates the inverse of a matrix.
# Extract diagonal values from a matrix:
#   R routine diag(); when A is a n times n matrix, diag(A) generates a vector of length n
# Eigenvalues of a matrix:
#   R routine eigen(); if you know that the matrix A is symmetric you should
#      specify the symmetry parameter: symmetric=TRUE; leads to faster and 
#      more reliable solution
# Singular value decomposition (SVD):
#   R routine svd() with matrix X as argument; X = U * D * V^T
#   The matrices U, D, V can be accessed as follows:
#   out = svd(X)
#   U = out$u; d = out$d; V = out$v
#   Note that d is a vector because the D-matrix is diagonal; it contains the
#        singular values.
#      
# -----------------------------------------------------------------------------
print('Results of analysis:')
print(c(round(offdiagmax,4),'largest offdiagonal absolute correlation'))
print(c(round(maxVIFs),'maximum variance inflation factor'))
print(c(round(kappa),'condition number (maximum ratio of eigenvalues)'))
print(c(round(eta),'maximum ratio of singular values'))
print(c(round(r13o,2),'strong anticorrelation between 1. and 3. predictor'))
# -----------------------------------------------------------------------------
# 'file: AcetyleneRidgeTrace.R'
# 'Wed Dec 14 21:11:27 2022'
# '-------------------------------------------'
# 'Plot: ridge trace'
# 'Results of analysis:'
# '0.9997'   'largest offdiagonal absolute correlation'
# '2856749'  'maximum variance inflation factor'
# '50202670' 'condition number (maximum ratio of eigenvalues)'
# '7085'     'maximum ratio of singular values'
# '-0.96'    'strong anticorrelation between 1. and 3. predictor'
# ------------------------------------------------------------------------------

