print('file: EIVgeometricTriangles2504.R')
# created by Dieter.Wolf-Gladrow@awi.de 1/2024
print('small data set for script; plot triangles')
# ----------------------------------------------------------------------
dflag = 0
if (dflag == 1) {
  print(' ---------------------------------------------------')
  print('(1) Jitjareonchai et al. (2006) data')
x = c(18.6369,25.8483,25.0829,30.2416,14.7174,
        14.2656,17.2711,14.0370,28.1420,12.2802)
y = c(70.5998,96.9826,89.3344,99.3439,55.9705,
        57.3196,65.9757,55.7193,87.9387,54.2554) # raw data
SlopeTrue = 3    # true slope
IcTrue = 10       # true intercept
varxTrue=4; varyTrue=9    # Table 2
xmin = min(x); xmax = max(x); n = length(x)
}
if (dflag == 0) {
print(' ---------------------------------------------------')
print('(1) Generate artificial data for y(x) = beta0 + beta*x')
n = 8   # sample size (small on purpose)
print(c(n,'n sample size'))
xmax = 5; xmin = -xmax; # range of x-values
SlopeTrue = -0.3    # true slope
IcTrue = 0.4        # true intercept
set.seed(1967)
xTrue = runif(n,xmin,xmax) # from uniform PDF
yTrue = SlopeTrue*xTrue+IcTrue
# Add normal noise to xTrue and yTrue:
sigx = 0.8 # x standard deviation (small compared to x-range on purpose)
sigy = 0.5 # y standard deviation: (sigx/sigy)^2 not equal to 1 and
#                       different from SlopeTrue^2
x = xTrue+rnorm(n,0,sigx) # observed x-values
y = yTrue+rnorm(n,0,sigy) # observed y-values
}
print(' ---------------------------------------------------')
print('(2) Regression y on x (SLR = Simple Linear Regression)')
SLRyonx = lm(y ~ x)
IcYonx    = as.numeric(SLRyonx$coefficients[1])
SlopeYonx = as.numeric(SLRyonx$coefficients[2])
print(c(round(IcYonx,4),'IcYonx'))
print(c(round(SlopeYonx,4),'SlopeYonx'))
print(' ---------------------------------------------------')
print('(3) Regression x on y (SLR = Simple Linear Regression)')
SLRxony = lm(x ~ y)
b = as.numeric(SLRxony$coefficients[2])
a = as.numeric(SLRxony$coefficients[1])
# x=b*y+a -> y = (x-a)/b -> slope = 1/b; 
SlopeXony = 1/b; IcXony = -a/b
print(c(round(IcXony,4),'IcXony'))
print(c(round(SlopeXony,4),'SlopeXony'))
print(' ---------------------------------------------------')
print('(4) geometric line')
SlopeGeo = sqrt(SlopeYonx*SlopeXony)*sign(SlopeYonx)
# line with slope SlopeGeo through centroid:
# y-yc = SlopeGeo*(x-xc)
# x = 0 -> y = IcGeo = yc-SlopeGeo*xc
xc = mean(x); yc = mean(y);  # centroid (Schwerpunkt)
IcGeo = yc-SlopeGeo*xc;
print(c(round(IcGeo,4),'IcGeo'))
print(c(round(SlopeGeo,4),'SlopeGeo'))
print(' ---------------------------------------------------')
print('(5) Plot:')
xp = c(xmin,xmax); yp = xp*SlopeTrue+IcTrue # true line
ypYonX = xp*SlopeYonx+IcYonx                # SLR y on x
ypXonY = xp*SlopeXony+IcXony                # SLR x on y
ypGeo = xp*SlopeGeo+IcGeo                   # geometric line
sflag = 1
if (sflag == 1) {
  # png('OUT_EIVtriangle240104s.png',width=16,height=16,units='cm',res=300)
  # png('EIVtriangle250405s.png',width=16,height=16,units='cm',res=300)
  plot(xp,yp,type='l',lwd=1,col='black',xlab='x',ylab='y',las=1,
       cex=0.6,cex.lab=1.5) #,ylim=c(-1.4,2.2))
  lines(xp,ypYonX,col='green',lwd=1,lty=2)
  lines(xp,ypXonY,col='green',lwd=1,lty=4)
  lines(xp,ypGeo,col='red',lwd=1,lty=4)
  # lines(xp,ypMLEEst,col='magenta',lwd=1,lty=3)
  points(x,y,col='blue',pch=24,lwd=3,cex=0.6)
  points(xc,yc,col='red',pch=20,lwd=5,cex=0.6)
  for(j in 1:n) { # plot triangles
    qx = c(x[j],(y[j]-IcGeo)/SlopeGeo); qy = c(y[j],y[j])
    lines(qx,qy,col='magenta',lwd=1)
    qxx = c(x[j],x[j]); qyy = c(y[j],IcGeo+SlopeGeo*x[j])
    lines(qxx,qyy,col='magenta',lwd=1)
    xpoly = c(x[j],x[j],(y[j]-IcGeo)/SlopeGeo,x[j])
    ypoly = c(y[j],IcGeo+SlopeGeo*x[j],y[j],y[j])
    polygon(xpoly,ypoly,col='yellow')
  }
 # dev.off()
}
# [1] "file: R_EIVgeometricTriangles_2504.R"
# [1] "small data set for script; plot triangles"
# [1] " ---------------------------------------------------"
# [1] "(1) Jitjareonchai et al. (2006) data"
# [1] " ---------------------------------------------------"
# [1] "(2) Regression y on x (SLR = Simple Linear Regression)"
# [1] "19.4953" "IcYonx" 
# [1]  "2.6854" "SlopeYonx"
# [1] " ---------------------------------------------------"
# [1] "(3) Regression x on y (SLR = Simple Linear Regression)"
# [1] "16.858"  "IcXony"
# [1]  "2.8169" "SlopeXony"
# [1] " ---------------------------------------------------"
# [1] "(4) geometric line"
# [1] "18.1924" "IcGeo"  
# [1]  "2.7504" "SlopeGeo"
# [1] " ---------------------------------------------------"
# [1] "(5) Plot:"
# [1] "file: R_EIVgeometricTriangles_2504.R"
# [1] "small data set for script; plot triangles"
# [1] " ---------------------------------------------------"
# [1] "(1) Generate artificial data for y(x) = beta0 + beta*x"
# [1] "8"             "n sample size"
# [1] " ---------------------------------------------------"
# [1] "(2) Regression y on x (SLR = Simple Linear Regression)"
# [1] "0.3747" "IcYonx"
# [1] "-0.2791"   "SlopeYonx"
# [1] " ---------------------------------------------------"
# [1] "(3) Regression x on y (SLR = Simple Linear Regression)"
# [1] "0.3382" "IcXony"
# [1] "-0.4249"   "SlopeXony"
# [1] " ---------------------------------------------------"
# [1] "(4) geometric line"
# [1] "0.3583" "IcGeo" 
# [1] "-0.3444"  "SlopeGeo"
# [1] " ---------------------------------------------------"
# [1] "(5) Plot:"