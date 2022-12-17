print('file: BayesianNHST-t-test.R')
print(date())
# compare NHST and Bayesian t-test: different effect size & n; ttestBF
# effect sizes = 0, 0.2, 1.2; sample size n = 10, 100 
# -> 6 different Monte Carlo simulations
library(BayesFactor)
alpha = 0.05   # level of significance
LowB10 = 1/sqrt(10)
HighB10 = sqrt(10)
sflag = 1
if (sflag == 1) {n = 10; delta = 0}
if (sflag == 2) {n = 10; delta = 0.2}
if (sflag == 3) {n = 10; delta = 1.2}
if (sflag == 4) {n = 100; delta = 0}
if (sflag == 5) {n = 100; delta = 0.2}
if (sflag == 6) {n = 100; delta = 1.2}
set.seed(1953)
M = 1e4     # number of Monte Carlo runs
p = numeric(M); B10 = numeric(M)
for (i in 1:M) {
  x = rnorm(n,mean=delta,sd=1)    # random sample from normal PDF
  p[i] = t.test(x,mu=0)$p.value
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
if (sflag == 1) {
  # png('BayesiantTestMCn10delta0x200408.png',width=16,height=16,units='cm',res=300)
  plot(p,B10,type='p',lwd=1,col='green',xlab='p',ylab=NA,las=0,
       cex=0.4,xlim=c(0,1),ylim=c(0.02,1000),log='y',cex.lab=1.5)
  title(ylab=TeX('$B_{10}$'),cex.lab=1.5,line=2.3)
  text(0.052,450,TeX('$p\\, < \\, \\alpha = 0.05$:'),col='magenta',pos=4,cex=1.5)
  text(0.4,470,paste(as.character(round(ploweralpha,2)),'%'),col='magenta',pos=4,cex=1.5)
  text(0.052,100,TeX('$H_0\\, = \\, $true;'),col='black',pos=4,cex=1.5)
  text(0.33,100,TeX('$\\delta = 0$;'),col='black',pos=4,cex=1.5)
  text(0.5,108,'n = 10;',col='black',pos=4,cex=1.5)
  text(0.7,108,paste('M = ',as.character(M)),col='black',pos=4,cex=1.5)
  abline(v=alpha,col='magenta')
  abline(h=1,col='blue',lty=4)
  abline(h=sqrt(10),col='blue',lty=4)
  abline(h=10,col='blue',lty=4)
  abline(h=1/sqrt(10),col='blue',lty=4)
  abline(h=0.1,col='blue',lty=4)
  text(0.052,20,paste(as.character(round(B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,20,TeX('strong evidence against $H_0$'),col='blue',pos=4)
  text(0.052,5,paste(as.character(round(B10higher3-B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,5,TeX('substantial evidence against $H_0$'),col='blue',pos=4)
  text(0.052,1.5,paste(as.character(round(B10higher1-B10higher3,1)),'%'),col='blue',pos=4)
  text(0.4,1.5,TeX('slight evidence against $H_0$'),col='blue',pos=4)
  text(0.052,0.5,paste(as.character(round(B10lower1-B10lower03,1)),'%'),col='blue',pos=4)
  text(0.4,0.5,TeX('slight evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.18,paste(as.character(round(B10lower03-B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.18,TeX('substantial evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.05,paste(as.character(round(B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.05,TeX('strong evidence for $H_0$'),col='blue',pos=4)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
# Calculation with M = 10000 takes about 1 min
# ----------------------------------------------------------------
if (sflag == 2) {
  png('BayesiantTestMCn10delta0d2x200408.png',width=16,height=16,units='cm',res=300)
  plot(p,B10,type='p',lwd=1,col='green',xlab='p',ylab=NA,las=0,
       cex=0.4,xlim=c(0,1),ylim=c(0.02,1000),log='y',cex.lab=1.5)
  title(ylab=TeX('$B_{10}$'),cex.lab=1.5,line=2.3)
  text(0.052,450,TeX('$p\\, < \\, \\alpha = 0.05$:'),col='magenta',pos=4)
  text(0.4,500,paste(as.character(round(ploweralpha,2)),'%'),col='magenta',pos=4)
  text(0.56,100,'n = 10;',col='black',pos=4)
  text(0.35,100,TeX('$\\delta = 0.2$;'),col='black',pos=4)
  text(0.8,100,paste('M = ',as.character(M)),col='black',pos=4)
  text(0.052,100,TeX('$H_0\\, = \\, $false;'),col='black',pos=4)
  abline(v=alpha,col='magenta')
  abline(h=1,col='blue',lty=4)
  abline(h=sqrt(10),col='blue',lty=4)
  abline(h=10,col='blue',lty=4)
  abline(h=1/sqrt(10),col='blue',lty=4)
  abline(h=0.1,col='blue',lty=4)
  text(0.052,20,paste(as.character(round(B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,20,TeX('strong evidence against $H_0$'),col='blue',pos=4)
  text(0.052,5,paste(as.character(round(B10higher3-B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,5,TeX('substantial evidence against $H_0$'),col='blue',pos=4)
  text(0.052,1.5,paste(as.character(round(B10higher1-B10higher3,1)),'%'),col='blue',pos=4)
  text(0.4,1.5,TeX('slight evidence against $H_0$'),col='blue',pos=4)
  text(0.052,0.5,paste(as.character(round(B10lower1-B10lower03,1)),'%'),col='blue',pos=4)
  text(0.4,0.5,TeX('slight evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.18,paste(as.character(round(B10lower03-B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.18,TeX('substantial evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.05,paste(as.character(round(B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.05,TeX('strong evidence for $H_0$'),col='blue',pos=4)
  dev.off()
}
if (sflag == 3) {
  # png('BayesiantTestMCn10delta1d2x200408.png',width=16,height=12,units='cm',res=300)
  plot(p,B10,type='p',lwd=1,col='green',xlab='p',ylab=NA,las=0,
       cex=0.4,xlim=c(0,1),ylim=c(0.02,max(B10)),log='y',cex.lab=1.5)
  title(ylab=TeX('$B_{10}$'),cex.lab=1.5,line=2.3)
  text(0.052,18000,TeX('$p\\, < \\, \\alpha = 0.05$:'),col='magenta',pos=4)
  text(0.4,20000,paste(as.character(round(ploweralpha,2)),'%'),col='magenta',pos=4)
  text(0.052,400,'n = 10;',col='black',pos=4)
  text(0.35,1200,TeX('$\\delta = 1.2$;'),col='black',pos=4)
  text(0.35,400,paste('M = ',as.character(M)),col='black',pos=4)
  text(0.052,1200,TeX('$H_0\\, = \\, $false;'),col='black',pos=4)
  abline(v=alpha,col='magenta')
  abline(h=1,col='blue',lty=4)
  abline(h=sqrt(10),col='blue',lty=4)
  abline(h=10,col='blue',lty=4)
  abline(h=1/sqrt(10),col='blue',lty=4)
  abline(h=0.1,col='blue',lty=4)
  text(0.052,20,paste(as.character(round(B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,20,TeX('strong evidence against $H_0$'),col='blue',pos=4)
  text(0.052,5,paste(as.character(round(B10higher3-B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,5,TeX('substantial evidence against $H_0$'),col='blue',pos=4)
  text(0.052,1.5,paste(as.character(round(B10higher1-B10higher3,1)),'%'),col='blue',pos=4)
  text(0.4,1.5,TeX('slight evidence against $H_0$'),col='blue',pos=4)
  text(0.052,0.5,paste(as.character(round(B10lower1-B10lower03,1)),'%'),col='blue',pos=4)
  text(0.4,0.5,TeX('slight evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.18,paste(as.character(round(B10lower03-B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.18,TeX('substantial evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.05,paste(as.character(round(B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.05,TeX('strong evidence for $H_0$'),col='blue',pos=4)
  # dev.off()
}
if (sflag == 4) {
  # png('BayesiantTestMCn100delta0x200408.png',width=16,height=16,units='cm',res=300)
  plot(p,B10,type='p',lwd=1,col='green',xlab='p',ylab=NA,las=0,
       cex=0.4,xlim=c(0,1),ylim=c(0.01,200),log='y',cex.lab=1.5)
  title(ylab=TeX('$B_{10}$'),cex.lab=1.5,line=2.3)
  text(0.052,100,TeX('$p\\, < \\, \\alpha = 0.05$:'),col='magenta',pos=4)
  text(0.4,110,paste(as.character(round(ploweralpha,2)),'%'),col='magenta',pos=4)
  text(0.5,55,'n = 100;',col='black',pos=4)
  text(0.35,50,TeX('$\\delta = 0$;'),col='black',pos=4)
  text(0.72,55,paste('M = ',as.character(M)),col='black',pos=4)
  text(0.052,50,TeX('$H_0\\, = \\, $true;'),col='black',pos=4)
  abline(v=alpha,col='magenta')
  abline(h=1,col='blue',lty=4)
  abline(h=sqrt(10),col='blue',lty=4)
  abline(h=10,col='blue',lty=4)
  abline(h=1/sqrt(10),col='blue',lty=4)
  abline(h=0.1,col='blue',lty=4)
  text(0.052,20,paste(as.character(round(B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,20,TeX('strong evidence against $H_0$'),col='blue',pos=4)
  text(0.052,5,paste(as.character(round(B10higher3-B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,5,TeX('substantial evidence against $H_0$'),col='blue',pos=4)
  text(0.052,1.5,paste(as.character(round(B10higher1-B10higher3,1)),'%'),col='blue',pos=4)
  text(0.4,1.5,TeX('slight evidence against $H_0$'),col='blue',pos=4)
  text(0.052,0.5,paste(as.character(round(B10lower1-B10lower03,1)),'%'),col='blue',pos=4)
  text(0.4,0.5,TeX('slight evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.18,paste(as.character(round(B10lower03-B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.18,TeX('substantial evidence for $H_0$'),col='blue',pos=4)
  text(0.052,0.05,paste(as.character(round(B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.05,TeX('strong evidence for $H_0$'),col='blue',pos=4)
  # dev.off()
}
if (sflag == 5) {
  # png('BayesiantTestMCn100delta0d2x200408.png',width=16,height=12,units='cm',res=300)
  plot(p,B10,type='p',lwd=1,col='green',xlab='p',ylab=NA,las=0,
       cex=0.4,xlim=c(0,1),ylim=c(0.05,20000),log='y',cex.lab=1.5)
  title(ylab=TeX('$B_{10}$'),cex.lab=1.5,line=2.3)
  text(0.052,1e4,TeX('$p\\, < \\, \\alpha = 0.05$:'),col='magenta',pos=4)
  text(0.4,1.1e4,paste(as.character(round(ploweralpha,2)),'%'),col='magenta',pos=4)
  text(0.052,3e2,'n = 100;',col='black',pos=4)
  text(0.4,1e3,TeX('$\\delta = 0.2$;'),col='black',pos=4)
  text(0.4,3e2,paste('M = ',as.character(M)),col='black',pos=4)
  text(0.052,1e3,TeX('$H_0\\, = \\, $false;'),col='black',pos=4)
  abline(v=alpha,col='magenta')
  abline(h=1,col='blue',lty=4)
  abline(h=sqrt(10),col='blue',lty=4)
  abline(h=10,col='blue',lty=4)
  abline(h=1/sqrt(10),col='blue',lty=4)
  abline(h=0.1,col='blue',lty=4)
  text(0.052,20,paste(as.character(round(B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,20,TeX('strong evidence against $H_0$'),col='blue',pos=4)
  text(0.052,5,paste(as.character(round(B10higher3-B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,5,TeX('substantial evidence against $H_0$'),col='blue',pos=4)
  text(0.052,1.5,paste(as.character(round(B10higher1-B10higher3,1)),'%'),col='blue',pos=4)
  text(0.4,1.5,TeX('slight evidence against $H_0$'),col='blue',pos=4)
  text(0.052,0.5,paste(as.character(round(B10lower1-B10lower03,1)),'%'),col='blue',pos=4)
  text(0.4,0.5,TeX('slight for $H_0$'),col='blue',pos=4)
  text(0.052,0.18,paste(as.character(round(B10lower03-B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.18,TeX('substantial for $H_0$'),col='blue',pos=4)
  text(0.052,0.05,paste(as.character(round(B10lower,1)),'%'),col='blue',pos=4)
  text(0.4,0.05,TeX('strong for $H_0$'),col='blue',pos=4)
  # dev.off()
}
if (sflag == 6) {
  # png('BayesiantTestMCn100delta1d2x200408.png',width=16,height=16,units='cm',res=300)
  plot(p,B10,type='p',lwd=1,col='green',xlab='p',ylab=NA,las=0,
       cex=0.4,xlim=c(0,1),ylim=c(10,1e29),log='y',cex.lab=1.5)
  title(ylab=TeX('$B_{10}$'),cex.lab=1.5,line=2.3)
  text(0.052,1e26,TeX('$p\\, < \\, \\alpha = 0.05$:'),col='magenta',pos=4)
  text(0.4,2e26,paste(as.character(round(ploweralpha,2)),'%'),col='magenta',pos=4)
  text(0.052,1e17,'n = 100;',col='black',pos=4)
  text(0.4,1e20,TeX('$\\delta = 1.2$;'),col='black',pos=4)
  text(0.4,1e17,paste('M = ',as.character(M)),col='black',pos=4)
  text(0.052,1e20,TeX('$H_0\\, = \\, $false;'),col='black',pos=4)
  abline(v=alpha,col='magenta')
  abline(h=10,col='blue',lty=4)
  text(0.052,100,paste(as.character(round(B10higher,1)),'%'),col='blue',pos=4)
  text(0.4,100,TeX('strong evidence against $H_0$'),col='blue',pos=4)
  # dev.off()
}
print(date())
# -----------------------------------------------------------------------------
# Results:
# "file: BayesianNHST-t-test.R"
# "Sat Dec 17 09:39:54 2022"
# "1"     "sflag"
# "10000" "M"    
# "10" "n" 
# "0"     "delta"
# "0.232"    "min(B10)"
# "419.408"  "max(B10)"
# "0.232326295914857" "min(B10)"         
# "419.408004100236" "max(B10)"        
# "5.14"          "p < alpha (%)"
# "94.86"         "p > alpha (%)"
# "0.63"             "B10 > 10 (%)"
# "1.88"        "10 > B10 > 3.16 (%)"
# "6.21"      "3.16 > B10 > 1 (%)"
# "34.96"      "0.316 B10 < 1 (%)"
# "56.32"        "0.1 B10 < 0.316 (%)"
# "0"                "B10 < 0.1 (%)"
# "Sat Dec 17 09:40:35 2022"
# -----------------------------------------------------------------------------
# Remarks:
# Run time of script: less than 1 minute (MacBook Air)
# -----------------------------------------------------------------------------
