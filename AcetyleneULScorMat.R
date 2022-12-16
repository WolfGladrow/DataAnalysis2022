print('file: AcetyleneULScorMat.R')
print(date())
# plot collinearity correlation matrix for unit-length scaled acytelene data
print(' ---------------------------------------------------')
print('(1) Acetylene data: Marquardt & Snee (1975)')
Yo = c(49.0,50.2,50.5,48.5,47.5,44.5,28.0,31.5,34.5,35.0,38.0,38.5,15.0,17.0,20.5,29.5)
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
print(' ---------------------------------------------------')
print('(2) derived predictors:')
X4o = X1o*X2o; X5o = X1o*X3o; X6o = X2o*X3o
X7o = X1o^2; X8o = X2o^2; X9o = X3o^2
L = length(X1o)  # number of data per predictor
print(' ---------------------------------------------------')
Q9 = matrix(data=c(X1o,X2o,X3o,X4o,X5o,X6o,X7o,X8o,X9o),nrow=L,ncol=9)
print('Unit normal scaling = subtract mean (centering) and divide by standard deviation')
X = matrix(data=NA,nrow=L,ncol=9)
for(k in 1:9) {q = Q9[,k]; z = (q-mean(q))/sqrt(sum((q-mean(q))^2)); X[,k] = z}
XX = t(X)%*%X
source('plotCorMatrix.R')
# png('AcetyleneXXheat201010.png',width=16,height=16,units='cm',res=300)
plotCorMatrix(XX)
# dev.off()