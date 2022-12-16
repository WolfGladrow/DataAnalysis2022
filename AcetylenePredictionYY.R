print('file: AcetylenePredictionYY.R')
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
lambda = outEV$values           # eigen values
kappa = max(lambda)/min(lambda) # condition number
# -------------------------------------------
# (5d) Condition number of X'
outSVD = svd(X)
muj = outSVD$d            # singular values
eta = max(muj)/min(muj)   # maximum ratio of singular values
# -------------------------------------------
r13o = cor(X1o,X3o) # correlation
# -------------------------------------------
sflag = 2
if (sflag == 2) {
  YPred = Xo%*%bEsto+InterceptEsto  # prediction
  xp = c(min(Yo),max(Yo))
  # png('AcetyleneYPredMLR170625.png',width=16,height=16,units='cm',res=300)
  plot(xp,xp,type='l',lwd=2,col='green',xlab='Y observed',ylab='Y predicted',las=1,
       cex.lab=1.5)
  points(Yo,YPred,lwd=4,col='blue',cex=0.6)
  # dev.off()
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
# 'file: AcetylenePredictionYY.R'
# 'Wed Dec 14 20:48:58 2022'
# 'Results of analysis:'
# '0.9997'     'largest offdiagonal absolute correlation'
# '2856749'    'maximum variance inflation factor'
# '50202670'   'condition number (maximum ratio of eigenvalues)'
# '7085'       'maximum ratio of singular values'
# '-0.96'      'strong anticorrelation between 1. and 3. predictor'
# -----------------------------------------------------------------------------
