print('file: OrthogonalRegression2504.R')
# created by Dieter.Wolf-Gladrow@awi.de 4/2024
# ----------------------------------------------------------------------
print(' ---------------------------------------------------')
print('(1) Generate artificial data for y(x) = beta0 + beta*x')
n = 8   # sample size (small on purpose)
print(c(n,'n sample size'))
xmax = 5; xmin = -xmax; # range of x-values
SlopeTrue = -0.3    # true slope
IcTrue = 0.4        # true intercept
set.seed(1967)
# set.seed(1953)
xTrue = runif(n,xmin,xmax) # from uniform PDF
yTrue = SlopeTrue*xTrue+IcTrue
# Add normal noise to xTrue and yTrue:
sigx = 0.8 # x standard deviation (small compared to x-range on purpose)
sigy = 1.5 # y standard deviation: (sigx/sigy)^2 not equal to 1 and
#                       different from SlopeTrue^2
x = xTrue+rnorm(n,0,sigx) # observed x-values
y = yTrue+rnorm(n,0,sigy) # observed y-values
print(' ---------------------------------------------------')
print('Sum of squares:')
print(' ---------------------------------------------------')
xmean = mean(x); ymean = mean(y)
(Sxx = sum((x-xmean)^2))
(Syy = sum((y-ymean)^2)) 
(Sxy = sum((x-xmean)*(y-ymean)))
print(c(round(Sxx,4),round(Syy,4),round(Sxy,4),'Sxx,Syy,Sxy'))
print(' ---------------------------------------------------')
print('(2) regressions')
(beta1I90 = Sxy/Sxx) # y on x
(beta2I90 = Syy/Sxy) # via x on y
(beta3I90 = 1/(beta1I90+beta2I90)*(beta1I90*beta2I90-1+
         sqrt((1+beta1I90^2)*(1+beta2I90^2)))) # bisection
(beta4I90 = ((beta2I90-1/beta1I90)+sign(Sxy)*
         sqrt(4+(beta2I90-1/beta1I90)^2))/2) # orthogonal
(beta5I90 = sign(Sxy)*sqrt(beta1I90*beta2I90)) # geometric
(IC1I90 = ymean-beta1I90*xmean)
(IC2I90 = ymean-beta2I90*xmean)
(IC3I90 = ymean-beta3I90*xmean)
(IC4I90 = ymean-beta4I90*xmean)
(IC5I90 = ymean-beta5I90*xmean)
print(' ---------------------------------------------------')
print('(3) Plot:')
xp = c(xmin,xmax); yp = xp*SlopeTrue+IcTrue # true line
ypOrtho = xp*beta4I90+IC4I90                # orthogonal line
sflag = 1
if (sflag == 1) {
  xc = mean(x); yc = mean(y);  # centroid (Schwerpunkt)
  # png('EIVorthogonal250408.png',width=16,height=16,units='cm',res=300)
  plot(xp,yp,type='l',lwd=1,col='black',xlab='x',ylab='y',las=1,
       cex=0.6,cex.lab=1.5,xlim=c(-5,5),ylim=c(-5,5))
  lines(xp,ypOrtho,col='magenta',lwd=1,lty=3)
  (gammaBeta = atan(beta4I90)) 
  (gammaAlpha = gammaBeta+pi/2) # rotation by 90 deg = pi/2 
  (alpha = tan(gammaAlpha))
for(j in 1:n) { # plot lines and yellow squares
    (alpha0 = y[j]-alpha*x[j])
    (xs = (IC4I90-alpha0)/(alpha-beta4I90))
    (ys = alpha*xs+alpha0)
    qx = c(xs,x[j]); qy = c(alpha*xs+alpha0,y[j])
    lines(qx,qy,col='magenta',lwd=1,lty=2)
    (L = sqrt((xs-x[j])^2+(alpha*xs+alpha0-y[j])^2))
    (dx = L*sqrt(1/(1+beta4I90^2))) 
    (dy = beta4I90*dx)  
    (LL = sqrt(dx^2+dy^2)) 
    (xpoly = c(xs,x[j],x[j]+dx,xs+dx,xs))
    (ypoly = c(ys,y[j],y[j]+dy,ys+dy,ys))
    polygon(xpoly,ypoly,col='yellow')
}
for(j in 1:n) { # plot magenta lines between data & estimated line
    (alpha0 = y[j]-alpha*x[j])  
    (xs = (IC4I90-alpha0)/(alpha-beta4I90)) 
    (ys = alpha*xs+alpha0) 
    qx = c(xs,x[j]); qy = c(alpha*xs+alpha0,y[j])
    lines(qx,qy,col='magenta',lwd=1,lty=1)
}
  points(x,y,col='blue',pch=20,lwd=3,cex=0.6) # plot data
 # dev.off()
}