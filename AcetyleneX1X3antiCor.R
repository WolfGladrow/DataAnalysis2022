print('file: AcetyleneX1X3antiCor.R')
# acetylene data: X1, X3 anticorrelated
# -------------------------------------------
# (1) = observed data (Yo,X1o,X2o,X3o) and quadratic predictors
Yo = c(49.0,50.2,50.5,48.5,47.5,44.5,28.0,31.5,34.5,35.0,38.0,38.5,15.0,17.0,20.5,29.5)
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
library(latex2exp)
sflag = 1
if (sflag == 1) {
  print('-------------------------------------------')
  print('Plot: X3o versus X1o: anti-correlated')
  # png('AcelyleneX3overX1a170625.png',width=16,height=16,units='cm',res=300)
  plot(X1o,X3o,type='p',lwd=4,col='blue',las=1,cex=0.6,
       xlab=TeX('$X_1(^o C)$'),ylab=NA,cex.lab=1.5)
  title(ylab=TeX('$X_3(s)$'),line=2.5,cex.lab=1.5)
  # dev.off()
}