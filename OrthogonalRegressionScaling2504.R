print('file: OrthogonalRegressionScaling2504.R')
print(date())
# purpose: 
# created by: Dieter.Wolf-Gladrow@awi.de 
#    4/2025 version 1.0
# This software is provided 'as is' without warranty of
# any kind. But it's mine, so you can't sell it.
# 
# ---------------------------------------------------------
print('Is the orthogonal line scale invariant? (4/2025)')
  print(' ---------------------------------------------------')
  print('(1) Generate artificial data for y(x) = beta0 + beta*x')
  n = 20  # sample size 
  print(c(n,'n sample size'))
  xmax = 5; xmin = -xmax; # range of x-values
  SlopeTrue = -0.3    # (m/s) true slope
  IcTrue = 0.4        # (m) true intercept
  set.seed(1953)
  xTrue = runif(n,xmin,xmax) # from uniform PDF
  yTrue = SlopeTrue*xTrue+IcTrue
  sigx = 0.8 # x standard deviation (small compared to x-range on purpose)
  sigy = 1.5 # y standard deviation: (sigx/sigy)^2 not equal to 1 and
  #                       different from SlopeTrue^2
  x = xTrue+rnorm(n,0,sigx) # (s) observed x-values
  y = yTrue+rnorm(n,0,sigy) # (m) observed y-values
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
  (beta4I90 = ((beta2I90-1/beta1I90)+sign(Sxy)*
        sqrt(4+(beta2I90-1/beta1I90)^2))/2) # orthogonal
  (IC4I90 = ymean-beta4I90*xmean)
  print(c(round(beta4I90,4),SlopeTrue,'estimated & true slope (m/s)'))
  print(c(round(IC4I90,4),IcTrue,'estimated & true intercept estimate (m)'))
sflag = 0
if (sflag == 1) {
    xt = c(min(x),max(x)); yt = SlopeTrue*xt+IcTrue
    yo = beta4I90*xt+IC4I90; 
  # png('OrthoScaleA250408.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x(s)',ylab='y(m)',las=1,cex=0.6,cex.lab=1.5)
  lines(xt,yt,col='black')
  lines(xt,yo,col='magenta',lty=3)
  # dev.off()
}
  print(' ---------------------------------------------------')
  print('Different scaling: y in cm instead of m')
  sc = 100 # scale factor
  y = y*sc
  print(' ---------------------------------------------------')
  print('Sum of squares:')
  print(' ---------------------------------------------------')
  xmean = mean(x); ymean = mean(y)
  (Sxx = sum((x-xmean)^2))
  (Syy = sum((y-ymean)^2)) 
  (Sxy = sum((x-xmean)*(y-ymean)))
  print(c(round(Sxx,4),round(Syy,4),round(Sxy,4),'Sxx,Syy,Sxy'))
  print(' ---------------------------------------------------')
  print('(4) regressions of scaled data')
  (beta1I90 = Sxy/Sxx) # y on x
  (beta2I90 = Syy/Sxy) # via x on y
  (beta4I90 = ((beta2I90-1/beta1I90)+sign(Sxy)*
                 sqrt(4+(beta2I90-1/beta1I90)^2))/2) # orthogonal
  (IC4I90 = ymean-beta4I90*xmean)
  # note reverse scaling (1/sc before printing & plotting)
  print(c(round(beta4I90/sc,4),SlopeTrue,'estimated & true slope (m/s)'))
  print(c(round(IC4I90/sc,4),IcTrue,'estimated & true intercept estimate (m)'))
sflag = 2
if (sflag == 2) {
    xt = c(min(x),max(x)); yt = SlopeTrue*xt+IcTrue
    yo = beta4I90*xt+IC4I90; 
    # png('OrthoScaleB250408.png',width=16,height=16,units='cm',res=300)
    plot(x,y/sc,type='p',lwd=4,col='blue',xlab='x(s)',ylab='y(m)',las=1,cex=0.6,cex.lab=1.5)
    lines(xt,yt,col='black')
    lines(xt,yo/sc,col='magenta',lty=3)
    # dev.off()
}
# ----------------------------------------------------------------
# Remarks: The orthogonal regression is not scale invariant. This
#   is well known since a long time and is here illustrated by a
#   simple example where the distance, y, is measured first in meters
#   and later scaled to centimeters: the estimated slopes,
#   -0.3903 m/s and -0.8379 m/s, differ by more than a factor 2.
#   Thus 
# ----------------------------------------------------------------
# [1] "file: R_OrthogonalRegressionScaling2504.R"
# [1] "Tue Apr  8 21:49:04 2025"
# [1] "1 = Is the orthogonal line scale invariant? (4/2025)"
# [1] " ---------------------------------------------------"
# [1] "(1) Generate artificial data for y(x) = beta0 + beta*x"
# [1] "20"            "n sample size"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "183.3697"    "51.0623"     "-60.929"     "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "(2) regressions"
# [1] "-0.3903" "-0.3"   "estimated & true slope (m/s)"
# [1] "0.1745"  [2] "0.4" "estimated & true intercept estimate (m)"
# [1] " ---------------------------------------------------"
# [1] "Different scaling: y in cm instead of m"
# [1] " ---------------------------------------------------"
# [1] "Sum of squares:"
# [1] " ---------------------------------------------------"
# [1] "183.3697"    "510623.4258" "-6092.9"     "Sxx,Syy,Sxy"
# [1] " ---------------------------------------------------"
# [1] "(4) regressions of scaled data"
# [1] "-0.8379"  "-0.3"   "estimated & true slope (m/s)"
# [1] "0.2757"    "0.4"   "estimated & true intercept estimate (m)"
# # -------------------------------------------------------------
