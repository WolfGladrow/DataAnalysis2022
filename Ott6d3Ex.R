print('file: Ott6d3Ex.R')
# rejection region
# Ott and Longnecker (2001) example 6.3 (9/2014)')
# Data (p.272):
y1 = c(18,43,28,50,16,32,13,35,38,33,6,7)   # worms in drug-treated sheep
y2 = c(40,54,26,63,21,37,39,23,48,58,28,39) # worms in    untreated sheep
n1 = length(y1); n2 = length(y2)
# % p.272-274
y1mean = mean(y1); print(c(round(y1mean,4),'y1mean'))   # 26.5833
s1 = sd(y1); print(c(round(s1,4),'s1'))                 # 14.3619
y2mean = mean(y2); print(c(round(y2mean,4),'y2mean'))   # 39.6667
s2 = sd(y2); print(c(round(s2,4),'s2'))                 # 13.8586
# pooled estimate:
sp = sqrt(((n1-1)*var(y1)+(n2-1)*var(y2))/(n1+n2-2)) # 14.1125
print(c(round(sp,4),'sp pooled estimate'))
ts = (y1mean-y2mean)/sp/sqrt(1/n1+1/n2)   # -2.2709 test statistic
print(c(round(ts,4),'ts test statistic'))
alpha = 0.05; print(c(alpha,'alpha chosen level of significance'))
nu = n1+n2-2; print(c(nu,'degrees of freedom')) 
tc = qt(alpha,nu); print(c(round(tc,4),'one-sided critical t-value'))   # -1.7171
p = pt(ts,nu); print(c(round(p,4),'p-value'))
#  --------------- Normality? Apply Lilliefors test
# install.packages('fBasics')  
  library(fBasics)
  LF1 = lillieTest(y1)
# Title: Lilliefors (KS) Normality Test
# Test Results: STATISTIC: D: 0.147  P VALUE:  0.6734 
  pLF = LF1@test$p.value   # another type of access to output
  print(c(round(pLF,4),'p-value from Lilliefors test'))
  library(latex2exp)
  t0 = t.test(y1,y2)$statistic; print(c(round(t0,4),'t0')) # t0 = test statistic = -2.2709
  # disp_p = p # observed level of evidence
  # mdiff = mean(y1)-mean(y2)
  tmax=4; dx=0.001; xa=seq(-tmax,tmax,dx); ya=dt(xa,nu);
  a05=0.05; t05=qt(1-a05,nu)  # one-sided critical value t_{0.05(1)22} = 1.7171 
  # z05=1.96; 
  yt=dt(ts,nu); xpz1=c(ts,ts); ypz1=c(0, yt*4); xpz05=c(1,1)*t05; ypz05=c(0,0.3); 
  u1=-tmax; u2=-t05; v1=dt(u1,nu); v2=dt(u2,nu); du=(u2-u1)/50; 
  un=seq(u1,u2,du); vn=dt(un,nu); uf=c(u2,u1,un); vf=c(0,0,vn); 
  # png('Ott01Ex6d3_rejection_regionL_140907.png',width=16,height=12,units='cm',res=300)
  plot(xa,ya,type='l',lwd=4,col='black',xlab='t',ylab=NA,las=1,cex=0.6,cex.lab=1.5,
       ylim=c(0,0.5))
  title(ylab=TeX('$f(t; \\nu = 22)$'),line=2.3,cex.lab=1.5)
  lines(-xpz05,ypz05,col='blue',lty=4)
  polygon(uf,vf,col='blue')
  xt = -3;
  text(xt,0.46,TeX('$-t_{0.05(1)22} = 1.7174$'),col='blue',cex=1.5,pos=4)
  text(xt,0.36,TeX('$\\alpha = 0.05$'),col='blue',cex=1.5,pos=4)
  text(xt,0.41,TeX('$blue\\, area = 0.05$'),col='blue',cex=1.5,pos=4)
# dev.off()