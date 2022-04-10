print('file: LSconcept.R')
# least squares: concept; artificial data
set.seed(1953) # set seed for random number generators
print(' ---------------------------------------------------')
print('(1) generate artificial data (same model as in What is regression?)')
beta0 = 8; beta1 = -1.2 # true intercept & slope
sigma = 1               # true standard deviation of noise
n = 30  # larger data set -> residuals can be tested for normality
x = round(sort(runif(n,min=1,max=5)),2)
y = round(beta0+beta1*x+sigma*rnorm(n),2)
out1 = lm(y~x)
intercept = out1$coefficients[1]; slope  = out1$coefficients[2]
# ----------------------------------------------------------------
# (2) plot 'squares' for 5 data points
x5 = c(x[2],x[9],x[26],x[18],x[21]); Lx = length(x5)
y5 = c(y[2],y[9],y[26],y[18],y[21])
xp = c(min(x),max(x)); yp = intercept+slope*xp
# png('LSsquares160831.png',width=16,height=16,units='cm',res=300)
plot(xp,yp,type='l',lwd=3,col='blue',xlab='x',ylab='y',las=1,
     cex=0.4,xlim=c(0,8),ylim=c(0,8),cex.lab=1.5)
# plot squares:
for(k in 1:Lx) { xA = x5[k]; yA = y5[k];
yB = intercept+slope*xA; dy=yA-yB; xB = xA+dy;
xsq = c(xA,xB,xB,xA,xA); ysq = c(yA,yA,yB,yB,yA);
polygon(xsq,ysq,col='yellow')}
lines(xp,yp,lwd=3,col='blue')
points(x5,y5,col='red',pch=19,cex=0.8,lwd=4)
# dev.off()