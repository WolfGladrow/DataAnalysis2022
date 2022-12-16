print('file: AcetyleneRidgeCon2.R')
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
# (5a) Lagest absolute correlation:
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
lambda = outEV$values           # eigen values
kappa = max(lambda)/min(lambda) # condition number
# -------------------------------------------
# (5d) Condition number of X'
outSVD = svd(X)
muj = outSVD$d          # singular values
eta = max(muj)/min(muj) # maximum ratio of singular values
# -------------------------------------------
r13o = cor(X1o,X3o) # correlation
# -------------------------------------------
k = 0.01
sflag = 6
if (sflag == 6) { 
  print('-------------------------------------------')
  print(c('Plot: extrapolation based on ridge regression with k =',k))
  source('ridgeDWG1706s.R')
  out = ridgeDWG1706s(Yo,Xo,dflag,k);
  betaEstoRR = out[[2]];   # slopes for original data
  InterceptoRR = out[[3]]; # intercept for original data
  # (1) min & max of X1o, X2o, X3o
  X1min = min(X1o); X1max = max(X1o)
  X2min = min(X2o); X2max = max(X2o)
  X3min = min(X3o); X3max = max(X3o)
  # ---------------------------------------------------------
  # (2) choose combination of min/max values:
  dX1 = (X1max-X1min)/100; dX3 = (X3max-X3min)/100;
  U1a = seq(X1min,X1max,dX1); LU1a = length(U1a)
  U2 = round(mean(X2o),1)
  print(c('U2 = ',U2))
  U3a = seq(X3min,X3max,dX3); LU3a = length(U3a)
  Yex = matrix(data=NA,nrow=LU1a,ncol=LU3a)
  # (3) double loop over X1-X3-plane
  for(i in 1:LU1a) {
    for(j in 1:LU3a) {U1 = U1a[i]; U3 = U3a[j];
    # (4) calculate corresponding values for predictors 4 to 9:
    U4 = U1*U2; U5 = U1*U3; U6 = U2*U3; U7 = U1^2; U8 = U2^2; U9 = U3^2;
    # (5) predict response
    U = c(U1,U2,U3,U4,U5,U6,U7,U8,U9);
    Yex[i,j] = t(U)%*%betaEstoRR+InterceptoRR  # prediction
    # YPred = Xo%*%betaEstoRR+InterceptoRR  # prediction
    }}
  print(c('Yex[1,1] =',Yex[1,1]))
  library(latex2exp)
  # png('AcetyleneRR001kextra170625.png',width=16,height=16,units='cm',res=300)
  contour(U1a,U3a,Yex,col='blue',las=0,xlab=TeX('$X_1(^o C)$'),ylab=NA,cex.lab=1.5)
  title(ylab=TeX('$X_3(s)$'),line=2.3,cex.lab=1.5)
  # dev.off()
  print('-------------------------------------------')
}
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
# 'file: AcetyleneRidgeCon2.R'
# 'Wed Dec 14 21:01:38 2022'
# '-------------------------------------------'
# 'Plot: extrapolation based on ridge regression with k =' '0.01'                                                  
# 'U2 = ' '12.4' 
# 'Yex[1,1] ='       '22.6907103993313'
#  '-------------------------------------------'
# 'Results of analysis:'
# '0.9997'     'largest offdiagonal absolute correlation'
# '2856749'    'maximum variance inflation factor'
# '50202670'   'condition number (maximum ratio of eigenvalues)'
# '7085'       'maximum ratio of singular values'
# '-0.96'      'strong anticorrelation between 1. and 3. predictor'
# -----------------------------------------------------------------------------

