print('File: Lilliefors3Ex.R')
print(date())
# Lilliefors test: 3 examples
y1 = c(-0.68644781, -0.82379154, -0.98416919, -2.02230891, -0.43507791, -0.76655674,  
       1.22178443,  0.09767100, -0.93391714, -1.23458941,  0.09188711,  0.56736177,
       -0.55276453, -0.07969400,  0.11767092,  2.07541230, 1.76443875,  0.60249792, 
       -1.29916116, -0.30322121, -0.77935252, -0.97190317,  0.84580262,  0.28698246,
       1.15160104,  0.35533328,  0.32936546,  1.68584964, 0.18260973,  1.93600509)
y2 = c(-0.43677762, -0.25606172, -0.05539436, -0.35955890, 2.52475885, -0.22159314,
       -0.17360687, -0.24747086, -0.25016363, -0.39612793, -0.22807506, -0.35896958,
       -0.23775192,  0.34039082,  1.31051033, -0.05988203, -0.15271093,  0.22672744,
       -0.15967543, -0.13572103, -0.44215444, -0.40168018,  0.44666595, -0.53978945,
       -0.45903502, -0.47862169, -0.22521386, -0.38695104, -0.44321571, -0.28632236)
y3 = c(0.09188711,  0.56736177, -0.55276453, -0.07969400,  0.11767092,  2.07541230,
       1.76443875,  0.60249792, -1.29916116, -0.30322121, -0.68644781, -0.82379154, 
       -0.98416919, -2.02230891, -0.43507791, -0.76655674, 1.22178443,  0.09767100, 
       -0.93391714, -1.23458941, -0.43677762, -0.25606172, -0.05539436, -0.35955890,
       2.52475885, -0.22159314, -0.17360687, -0.24747086, -0.25016363, -0.39612793)
# install.packages('fBasics')  # install package
library(fBasics)         
sflag = 3
if (sflag == 1) {
  y = (y1 - mean(y1))/sd(y1) # standardize
  outKS = lillieTest(y)
  D = outKS@test$statistic; p = outKS@test$p.value
  xmax = 3; xmin = -xmax
  xH0 = seq(xmin,xmax,0.0001); yH0 = pnorm(xH0,mean(y),sd(y))
  # png('LillieforsExampleA160915.png',width=16,height=16,units='cm',res=300)
  plot(xH0,yH0,type='l',lwd=4,col='black',xlab='x',ylab='CDF',las=1,cex.lab=1.5)
  lines(ecdf(sort(y)),col='blue',lwd=4)
  xt = 1
  text(xt,0.4,paste('D = ',as.character(round(D,3))),col='blue',cex=1.5,pos=4)
  text(xt,0.2,paste('p = ',as.character(round(p,3))),col='blue',cex=1.5,pos=4)
  legend('topleft',legend=c('estimated','normal'),col=c('blue','black'),lty=c(1,1),lwd=c(3,3),cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  y = (y2 - mean(y2))/sd(y2) # standardize
  outKS = lillieTest(y)
  D = outKS@test$statistic; p = outKS@test$p.value
  xmax = 4.5; xmin = -xmax
  xH0 = seq(xmin,xmax,0.0001); yH0 = pnorm(xH0,mean(y),sd(y))
  # png('LillieforsExampleB160915.png',width=16,height=16,units='cm',res=300)
  plot(xH0,yH0,type='l',lwd=4,col='black',xlab='x',ylab='CDF',las=1,cex.lab=1.5)
  lines(ecdf(sort(y)),col='blue',lwd=4)
  xt = 1
  text(xt,0.4,paste('D = ',as.character(round(D,3))),col='blue',cex=1.5,pos=4)
  text(xt,0.2,paste('p = ',as.character(round(p,9))),col='blue',cex=1.5,pos=4)
  legend('topleft',legend=c('estimated','normal'),col=c('blue','black'),
         lty=c(1,1),lwd=c(3,3),cex=1.2)
  # dev.off()
}
if (sflag == 3) {
  y = (y3 - mean(y3))/sd(y3) # standardize
  outKS3 = lillieTest(y)     # D: 0.2061    p-value = 0.002211 -> reject H0
  outKS = ks.test(y,'pnorm') # D: 0.20606   p-value = 0.1355   -> do not reject H0
  D = outKS3@test$statistic; p = outKS3@test$p.value
  xmax = 4.5; xmin = -xmax
  xH0 = seq(xmin,xmax,0.0001); yH0 = pnorm(xH0,mean(y),sd(y))
  # png('LillieforsExampleC170711.png',width=16,height=16,units='cm',res=300)
  plot(xH0,yH0,type='l',lwd=4,col='black',xlab='x',ylab='CDF',las=1,cex.lab=1.5)
  lines(ecdf(sort(y)),col='blue',lwd=4)
  xt = 1 # -xmax*2.5/3
  text(xt,0.4,paste('D = ',as.character(round(D,3))),col='blue',cex=1.5,pos=4)
  text(xt,0.2,paste('p = ',as.character(round(p,4))),col='blue',cex=1.5,pos=4)
  legend('topleft',legend=c('estimated','normal'),col=c('blue','black'),
         lty=c(1,1),lwd=c(3,3),cex=1.2)
  # dev.off()
  print('outKS3 (Lilliefors')
  print(outKS3)
  print('outKS')
  print(outKS)
}
# -----------------------------------------------------------------------------
# Results:
# "File: Lilliefors3Ex.R"
# "Sat Dec 17 19:57:55 2022"
# "outKS3 (Lilliefors"
# Title: Lilliefors (KS) Normality Test
# Test Results:
#  STATISTIC:  D: 0.2061
# P VALUE:  0.002211 
# Description:
#  Sat Dec 17 19:57:55 2022 by user: 
# "outKS"
# One-sample Kolmogorov-Smirnov test
# data:  y
# D = 0.20606, p-value = 0.1355
# alternative hypothesis: two-sided
# -----------------------------------------------------------------------------

