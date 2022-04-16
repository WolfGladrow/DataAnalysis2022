ridgeDWG1706s = function(Yo,Xo,dflag,k) {
# ----------------------------------------------------------
# file: ridgeDWG1706s.R (short version; no diagnostics included)
#
# history: ridgeDWG1706.R, ridgeDWG2.R, ridgeDWG2.R
#          ridgeDWG2.m [n,b,bs,Lbs]
#
# purpose: ridge regression via X'X with correlation form
#          by application of unit length scaling
#
# written by Dieter.Wolf-Gladrow@awi.de 6/2017
#
# references: Hoerl & Kennard (1970)
#             Marquardt & Snee (1975)
#             Montgomery & Perk (1982)
#
# input:
#   Yo       values of response variable   (original, not scaled)
#   Xo       matrix of predictor variables (original, not scaled)
#   dflag    diagnostic flag
#   k        ridge regression factor (0 <= k < 1)
#
# output: return(list(n,betaEsto,Intercepto,betaEst,LbetaEst))
#   n           number of predictors = number of slopes
#   betaEsto    slopes for original data
#   Intercepto  intercept for original data
#   betaEst     slopes for scaled data (1 to n; intercept = 0)
#   LbetaEst    length of betaEst
# 
# ----------------------------------------------------------

# (0) dimensions of input
mn = dim(Xo)
m = mn[1] # m = sample size
n = mn[2] # n = number of predictors

# (1) unit length scaling of Y0 & X0 -> y & X
meanY = mean(Yo); SY = sqrt(sum( (Yo-mean(Yo))^2 )); y = (Yo-meanY)/SY
X = matrix(data=NA,nrow=m,ncol=n)
meanX = numeric(n); SX = numeric(n)
for(j in 1:n) { meanX[j] = mean(Xo[,j]);
SX[j] = sqrt(sum((Xo[,j]-meanX[j])^2));
X[,j] = (Xo[,j]-meanX[j])/SX[j] }
# (2) Correlation matrix X'X:
XX = t(X)%*%X
# (3) Add k times identity matrix, then solve linear system:
IM = diag(n)   # identity matrix (n times n)
IXXk = solve(XX+k*IM)
# (4) Estimates of slopes for scaled data:
betaEst = numeric(n); b = numeric(n)
betaEst = IXXk%*%(t(X)%*%y)   # estimated slopes for scaled data
# (5) Estimates of slopes for original data:
bA = betaEst/SX
cA = -betaEst*meanX/SX
betaEsto = betaEst/SX*SY
Intercepto = sum(cA)*SY+meanY # intercept for original data
# (6) Diagnostics: length of betaEst
LbetaEst = sqrt(sum(betaEst^2))   
# (7) Return results:
return(list(n,betaEsto,Intercepto,betaEst,LbetaEst))
}