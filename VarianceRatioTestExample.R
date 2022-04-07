print('file: VarianceRatioTestExample.R')
# illustration of variance ratio test; artificial data
# (1) generate random samples from normal distributions with equal variances:
set.seed(1953) # set seed for random number generators
n1 = 7; n2 = 5  # sample sizes (small & different from each other -> differences recognizable)
x1 = round(rnorm(n1,mean=2,sd=1.5),3)
x2 = round(rnorm(n2,mean=3,sd=1.5),3)
nu1 = n1-1; nu2 = n2-1    # degrees of freedom
# (2) apply variance ratio test (F test):
out1 = var.test(x1,x2); p1 = out1$p.value; print(c(round(p1,4),'p1'))
out2 = var.test(x2,x1); p2 = out2$p.value; print(c(round(p2,4),'p2'))
alpha = 0.05
p = p1
if(p < alpha) print(c('p < alpha = ',alpha,': reject H0'))
if(p >= alpha) print(c('p >= alpha = ',alpha,': do not reject H0'))
# (3) pedestrian way:
F01 = var(x1)/var(x2)
p01 = pf(F01,df1=nu1,df2=nu2)
p21 = 2*min(p01,1-p01); print(c(round(p21,4),'p21'))
F02 = var(x2)/var(x1)
p02 = pf(F02,df1=nu2,df2=nu1) # note interchange of nu1, nu2 in degr. of freedom
p22 = 2*min(p02,1-p02); print(c(round(p21,4),'p21'))
# (4) plot F-distributions & p-values:
xp = seq(0,3,0.001); y1 = df(xp,df1=nu1,df2=nu2); y2 = df(xp,df1=nu2,df2=nu1)
xF01 = c(F01,F01); yF01 = c(0,df(F01,df1=nu1,df2=nu2))
xF02 = c(F02,F02); yF02 = c(0,df(F02,df1=nu1,df2=nu2))
# Polygons:
a1 = seq(0,F01,0.001); b1 = df(a1,df1=nu2,df2=nu1); c1 = c(a1,F01,0,0); d1 = c(b1,0,0,b1[1])
a2 = seq(F02,3,0.001); b2 = df(a2,df1=nu1,df2=nu2); c2 = c(a2,3,F02,F02); d2 = c(b2,0,0,b2[1])
sflag = 1
if (sflag == 1) {
# png('VarRatioTest170707.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(1,2))
plot(xp,y1,type='l',lwd=3,col='blue',xlab='x',ylab='F-distributions',las=1,
     cex=0.4,ylim=c(0,0.7),cex.lab=1.5)
polygon(c1,d1,col='black')
lines(xp,y1,col='blue',lwd=3,lty=1)
lines(xp,y2,col='magenta',lwd=3,lty=1)
lines(xF01,yF01,col='black',lwd=1)
text(F01+0.3,df(F01,df1=nu2,df2=nu1),pos=4,col='black',
     paste('F = ',as.character(round(F01,3))),cex=1.5)
text(0.4,0.68,pos=4,col='black',
     paste('p = ',as.character(round(p01,3))),cex=1.5)
# text(F01+0.3,df(F01,df1=nu2,df2=nu1)-0.1,pos=4,col='black',
# --------------------
plot(xp,y1,type='l',lwd=3,col='blue',xlab='x',ylab='F-distributions',
     las=1,cex=0.4,ylim=c(0,0.7),cex.lab=1.5)
polygon(c2,d2,col='black')
lines(xp,y1,col='blue',lwd=3,lty=1)
lines(xp,y2,col='magenta',lwd=3,lty=1)
lines(xF02,yF02,col='black',lwd=1)
text(0.8,df(F01,df1=nu2,df2=nu1),pos=4,col='black',
     paste('F = ',as.character(round(F02,3))),cex=1.5)
text(0.1,0.68,pos=4,col='black',
     paste('1-p = ',as.character(round(1-p02,3))),cex=1.5)
# dev.off()
}