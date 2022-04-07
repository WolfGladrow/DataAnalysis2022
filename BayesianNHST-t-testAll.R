print('file: BayesianNHST-t-testA.R')
# hist: compare t- and Bayesian-t-test: different effect size & n
# effect sizes = 0, 0.2, 1.2; sample size n = 10, 100 
# -> 6 different Monte Carlo simulations
alpha = 0.05   # level of significance
LowB10 = 1/sqrt(10)
HighB10 = sqrt(10)
sflag = 1
if (sflag == 1) {n = 10; delta = 0}      # A
if (sflag == 2) {n = 10; delta = 0.2}    # B
if (sflag == 3) {n = 10; delta = 1.2}    # C
if (sflag == 4) {n = 100; delta = 0}
if (sflag == 5) {n = 100; delta = 0.2}
if (sflag == 6) {n = 100; delta = 1.2}
set.seed(1953)
M = 1e4 # number of Monte Carlo runs
p = numeric(M); B10 = numeric(M)
for (i in 1:M) {
  x = rnorm(n,mean=delta,sd=1)    # random sample from normal PDF
  p[i] = t.test(x,mu=0)$p.value
  tval = t.test(x,mu=0)$statistic # t-value
  out = ttestBF(x,rscale=1); B10[i] = extractBF(out,onlybf = TRUE)
}
print(c(sflag,'sflag'))
print(c(M,'M'))
print(c(n,'n'))
print(c(delta,'delta'))
print(c(round(min(B10),3),'min(B10)'))
print(c(round(max(B10),3),'max(B10)'))
print(c(min(B10),'min(B10)'))
print(c(max(B10),'max(B10)'))
# How many p-values < alpha?
ploweralpha = sum(p < alpha)/M*100    # percent
plargeralpha = sum(p >= alpha)/M*100 
print(c(ploweralpha,'p < alpha (%)'))
print(c(plargeralpha,'p > alpha (%)'))
B10lower = sum(B10 < 0.1)/M*100 # percent
B10higher = sum(B10 > 10)/M*100
B10higher3 = sum(B10 > HighB10)/M*100
B10higher1 = sum(B10 > 1)/M*100
B10lower03 = sum(B10 < LowB10)/M*100
B10lower1 = sum(B10 < 1)/M*100
print(c(B10higher,'B10 > 10 (%)'))
print(c(B10higher3-B10higher,'10 > B10 > 3.16 (%)'))
print(c(B10higher1-B10higher3,'3.16 > B10 > 1 (%)'))
print(c(B10lower1-B10lower03,'0.316 B10 < 1 (%)'))
print(c(B10lower03-B10lower,'0.1 B10 < 0.316 (%)'))
print(c(B10lower,'B10 < 0.1 (%)'))
# Bayesian-t-test-MonteCarlo-n=10-delta=1.2-histogram-date.png
if (sflag == 1) {
# png('BttMCn10d0h220309A.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(2,1))
# A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
par(mai=c(1,0.8,0.3,1))
hist(log10(B10),col='grey',las=0,main='',xlab=NA,ylab=NA,cex.lab=1.5)
title(xlab=TeX('$log_{10}(B_{10})$'),cex.lab=1.5,line=2.5)
title(ylab='Frequency',line=2.3,cex.lab=1.5)
abline(v=log10(1),col='blue'); abline(v=log10(sqrt(10)),col='blue'); abline(v=log10(10),col='blue')
abline(v=log10(1/sqrt(10)),col='blue'); abline(v=log10(0.1),col='blue')
text(1.1,1500,paste('n = ',as.character(n)),col='black',pos=4,cex=1.5)
text(1.1,2200,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
text(1.9,1450,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
if (delta > 0) text(1.9,2200,TeX('$H_0\\, = \\, false$'),col='black',pos=4,cex=1.5)
if (delta == 0) text(1.9,2200,TeX('$H_0\\, = \\, true$'),col='black',pos=4,cex=1.5)
par(mai=c(1,0.8,0,1))
hist(p,breaks=20,col='grey',las=0,main='',ylab=NA,xlim=c(0,1),cex.lab=1.5)
abline(v=0.05,col='red',lwd=3)
title(ylab='Frequency',line=2.3,cex.lab=1.5)
# dev.off()
}
if (sflag == 2) {
  # png('BttMCn10d0d2h220309B.png',width=16,height=16,units='cm',res=300)
  par(mfrow=c(2,1))
  # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
  par(mai=c(1,0.8,0.3,1))
  hist(log10(B10),col='grey',las=0,main='',xlab=NA,ylab=NA,cex.lab=1.5)
  title(xlab=TeX('$log_{10}(B_{10})$'),cex.lab=1.5,line=2.5)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  abline(v=log10(1),col='blue'); abline(v=log10(sqrt(10)),col='blue'); abline(v=log10(10),col='blue')
  abline(v=log10(1/sqrt(10)),col='blue'); abline(v=log10(0.1),col='blue')
  text(1,1500,paste('n = ',as.character(n)),col='black',pos=4,cex=1.5)
  text(1,2200,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
  text(2,1450,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
  if (delta > 0) text(2,2200,TeX('$H_0\\, = \\, false$'),col='black',pos=4,cex=1.5)
  if (delta == 0) text(2,2200,TeX('$H_0\\, = \\, true$'),col='black',pos=4,cex=1.5)
  par(mai=c(1,0.8,0,1))
  hist(p,breaks=20,col='grey',las=0,main='',ylab=NA,xlim=c(0,1),cex.lab=1.5)
  abline(v=0.05,col='red',lwd=3)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  # dev.off()
}
if (sflag == 3) {
# png('BttMCn10d1d2h220309C.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(2,1))
# A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
par(mai=c(1,0.8,0.3,1))
hist(log10(B10),col='grey',las=0,main='',xlab=NA,ylab=NA,cex.lab=1.5)
title(xlab=TeX('$log_{10}(B_{10})$'),cex.lab=1.5,line=2.5)
title(ylab='Frequency',line=2.3,cex.lab=1.5)
abline(v=log10(1),col='blue'); abline(v=log10(sqrt(10)),col='blue'); abline(v=log10(10),col='blue')
abline(v=log10(1/sqrt(10)),col='blue'); abline(v=log10(0.1),col='blue')
text(2.1,1500,paste('n = ',as.character(n)),col='black',pos=4,cex=1.5)
text(2.1,2200,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
text(4,1450,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
if (delta > 0) text(4,2200,TeX('$H_0\\, = \\, false$'),col='black',pos=4,cex=1.5)
if (delta == 0) text(4,2200,TeX('$H_0\\, = \\, true$'),col='black',pos=4,cex=1.5)
par(mai=c(1,0.8,0,1))
hist(p,breaks=20,col='grey',las=0,main='',ylab=NA,xlim=c(0,1),cex.lab=1.5)
abline(v=0.05,col='red',lwd=3)
title(ylab='Frequency',line=2.3,cex.lab=1.5)
# dev.off()
}
if (sflag == 4) {
  # png('BttMCn100d0h220309D.png',width=16,height=16,units='cm',res=300)
  par(mfrow=c(2,1))
  # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
  par(mai=c(1,0.8,0.3,1))
  hist(log10(B10),col='grey',las=0,main='',xlab=NA,ylab=NA,cex.lab=1.5)
  title(xlab=TeX('$log_{10}(B_{10})$'),cex.lab=1.5,line=2.5)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  abline(v=log10(1),col='blue'); abline(v=log10(sqrt(10)),col='blue'); abline(v=log10(10),col='blue')
  abline(v=log10(1/sqrt(10)),col='blue'); abline(v=log10(0.1),col='blue')
  text(0,1500,paste('n = ',as.character(n)),col='black',pos=4,cex=1.5)
  text(0,2200,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
  text(1,1450,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
  # text(1,2200,TeX('$H_0\\, = \\, $false'),col='black',pos=4,cex=1.5)
  if (delta > 0) text(1,2200,TeX('$H_0\\, = \\, false$'),col='black',pos=4,cex=1.5)
  if (delta == 0) text(1,2200,TeX('$H_0\\, = \\, true$'),col='black',pos=4,cex=1.5)
  par(mai=c(1,0.8,0,1))
  hist(p,breaks=20,col='grey',las=0,main='',ylab=NA,xlim=c(0,1),cex.lab=1.5)
  abline(v=0.05,col='red',lwd=3)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  # dev.off()
}
if (sflag == 5) {
  # png('BttMCn100d0d2h220309E.png',width=16,height=16,units='cm',res=300)
  par(mfrow=c(2,1))
  # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
  par(mai=c(1,0.8,0.3,1))
  hist(log10(B10),col='grey',las=0,main='',xlab=NA,ylab=NA,cex.lab=1.5)
  title(xlab=TeX('$log_{10}(B_{10})$'),cex.lab=1.5,line=2.5)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  abline(v=log10(1),col='blue'); abline(v=log10(sqrt(10)),col='blue'); abline(v=log10(10),col='blue')
  abline(v=log10(1/sqrt(10)),col='blue'); abline(v=log10(0.1),col='blue')
  # text(0.8,1500,paste('n = ',as.character(n)),col='black',pos=4,cex=1.5)
  text(0.8,2250,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
  # text(3.5,1500,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
  text(1,1500,paste('n = ',as.character(n),'; M = ',as.character(M)),col='black',pos=4,cex=1.5)
  # text(3,2200,TeX('$H_0\\, = \\, $false'),col='black',pos=4,cex=1.5)
  if (delta > 0) text(3.5,2200,TeX('$H_0\\, = \\, false$'),col='black',pos=4,cex=1.5)
  if (delta == 0) text(3.5,2200,TeX('$H_0\\, = \\, true$'),col='black',pos=4,cex=1.5)
  par(mai=c(1,0.8,0,1))
  hist(p,breaks=20,col='grey',las=0,main='',ylab=NA,xlim=c(0,1),cex.lab=1.5)
  abline(v=0.05,col='red',lwd=3)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  # dev.off()
}
if (sflag == 6) {
  # png('BttMCn100d1d2h220309F.png',width=16,height=16,units='cm',res=300)
  par(mfrow=c(2,1))
  # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
  par(mai=c(1,0.8,0.3,1))
  hist(log10(B10),col='grey',las=0,main='',xlab=NA,ylab=NA,cex.lab=1.5)
  title(xlab=TeX('$log_{10}(B_{10})$'),cex.lab=1.5,line=2.5)
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  abline(v=log10(1),col='blue'); abline(v=log10(sqrt(10)),col='blue'); abline(v=log10(10),col='blue')
  abline(v=log10(1/sqrt(10)),col='blue'); abline(v=log10(0.1),col='blue')
  text(8,800,paste('n = ',as.character(n)),col='black',pos=4,cex=1.5)
  text(8,1200,bquote(~delta == .(delta)),col='black',pos=4,cex=1.5)
  text(23,760,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
  if (delta > 0) text(23,1200,TeX('$H_0\\, = \\, false$'),col='black',pos=4,cex=1.5)
  if (delta == 0) text(23,1200,TeX('$H_0\\, = \\, true$'),col='black',pos=4,cex=1.5)
  par(mai=c(1,0.8,0,1))
  hist(p,breaks=50,col='grey',las=0,main='',ylab=NA,xlim=c(0,max(p)),cex.lab=1.5) # xlim=c(0,max(p))
  title(ylab='Frequency',line=2.3,cex.lab=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
# Calculation with large sample size (n = 100) and many Monte Carlo runs (M = 1e4) will take a
#   few minutes.
# ----------------------------------------------------------------