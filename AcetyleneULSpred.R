print('file: AcetyleneULSpred.R')
# MLR, 9 predictors, unit length scaled
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
XX = t(X)%*%X
# (1) call lm()
out1 = lm(y ~ X)
# (2) least-squares solution: pedestrian
IXX = solve(XX)
betaEst = IXX%*%(t(X)%*%y)
# (3) calculate predicted response & plot
Ypredict = (X%*%betaEst)*sqrt(sum((Yo-mean(Yo))^2))+mean(Yo)
# png('YoYpredictedML82x170620.png',width=16,height=16,units='cm',res=300)
xp = c(0,60)
plot(xp,xp,type='l',lwd=3,col='green',xlab='Observed Y (%)',
     ylab='Predicted Y (%)',las=1,cex.lab=1.5)
points(Yo,Ypredict,col='blue',lwd=4,cex=0.6)
# dev.off()
# ---------------------------------------------------
# Results for X = XMG82
summary(out1)