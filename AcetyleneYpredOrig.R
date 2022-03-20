print('file: AcetyleneYpredOrig.R')
# scaling, MLR, ridge regression of acetylene data (6/2017)')
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
k = 0.01
print(c('Plot: prediction based on ridge regression with k = ',k))
source('ridgeDWG1706s.R')
out = ridgeDWG1706s(Yo,Xo,dflag,k);
betaEstoRR = out[[2]]   # slopes for original data
InterceptoRR = out[[3]] # intercept for original data
YPred = Xo%*%betaEstoRR+InterceptoRR  # prediction
xp = c(min(c(Yo,YPred)),max(c(Yo,YPred)))
# png('AcetyleneYPred001k170625.png',width=16,height=12,units='cm',res=300)
plot(xp,xp,type='l',lwd=2,col='green',xlab='Y observed',ylab='Y predicted',las=1,
     cex.lab=1.5)
points(Yo,YPred,lwd=4,col='blue',cex=0.6)
# dev.off()