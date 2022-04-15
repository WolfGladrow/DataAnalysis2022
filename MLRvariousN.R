print('file: MLRvariousN.R')
# generate data set for multiple linear regression: N varied
#     True model: Yexact = beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * X3 + ... 
#     Predictor matrix X should contain at least 2 highly-correlated columns 
#     plus at least 1 column that is not correlated to the highly-correlated columns
#     -> at least N times 3 matrix  (N = sample size = number of rows)
#     Correlated columns can be generated using function samplecorFct(r,N).
#      
#     Generation of Y: choose intercept beta_0 and slopes beta_j, then calculate
#     Yexact = beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * X3
#     and finally add normal noise
#     Y = Yexact + noise
#     Question: 
#       How well can one estimate intercept and slopes from multiple regression?
# ------------------------------------------------------------------------------
beta0 = 1.5; beta1 = -0.3; beta2 = 0.8; beta3 = 1  # model parameters
Narr = seq(10,500,10); L = length(Narr)  # sample size
b0 = numeric(L); b1 = numeric(L); b2 = numeric(L); b3 = numeric(L);
r = 0.9     # correlation coefficient between X1 and X2
source('samplecorFct.R')
for(n in 1:L) { N = Narr[n];
set.seed(1953) # set seed for random number generators
out1 = samplecorFct(r,N); x1 = out1[1:N]; x2 = out1[(N+1):(2*N)];
x3 = runif(N)
# Correlation matrix:
X = matrix(data=c(x1,x2,x3),nrow=N,ncol=3)
cor(X)
#            [,1]       [,2]       [,3]
# [1,]  1.0000000  0.9102828 -0.3344019
# [2,]  0.9102828  1.0000000 -0.3574583
# [3,] -0.3344019 -0.3574583  1.0000000
yexact = beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
y = yexact+0.8*rnorm(N)
out2 = lm(y ~ x1+x2+x3)
b0[n] = out2$coefficients[1]; b1[n] = out2$coefficients[2];
b2[n] = out2$coefficients[3]; b3[n] = out2$coefficients[4];
}
sflag = 0
if (sflag == 0) {
  png('MultiRegrb0A160803.png',width=16,height=16,units='cm',res=300)
  plot(Narr,b0,type='p',lwd=4,col='blue',xlab='n',ylab=NA,las=1,cex=0.6,
       cex.lab=1.5)
  title(ylab=expression(b[0]),cex.lab=1.5,line=2.5)
  abline(a=beta0,b=0,col='black',lty=2)
  dev.off()
}
if (sflag == 1) {
  # png('MultiRegrb1A160803.png',width=16,height=16,units='cm',res=300)
  plot(Narr,b1,type='p',lwd=4,col='blue',xlab='n',ylab=NA,las=1,cex=0.6,
       cex.lab=1.5)
  title(ylab=expression(b[1]),cex.lab=1.5,line=2.5)
  abline(a=beta1,b=0,col='black',lty=2)
  # dev.off()
}
if (sflag == 2) {
  # png('MultiRegrb2A160803.png',width=16,height=16,units='cm',res=300)
  plot(Narr,b2,type='p',lwd=4,col='blue',xlab='n',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
  title(ylab=expression(b[2]),cex.lab=1.5,line=2.5)
  abline(a=beta2,b=0,col='black',lty=2)
  # dev.off()
}
if (sflag == 3) {
  # png('MultiRegrb3A160803.png',width=16,height=16,units='cm',res=300)
  plot(Narr,b3,type='p',lwd=4,col='blue',xlab='n',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
  title(ylab=expression(b[3]),cex.lab=1.5,line=2.5)
  abline(a=beta3,b=0,col='black',lty=2)
  # dev.off()
}