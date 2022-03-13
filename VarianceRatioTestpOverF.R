print('file: VarianceRatioTestpOverF.R')
# variance ratio test: p .ne. 1 at R = 1 
Fa = seq(0.5,2,0.001); L = length(Fa)
nu1 = 6; nu2 = 4  # fictive degrees of freedom; must be different from each other
pa = numeric(L)
for(k in 1:L) {p = pf(Fa[k],df1=nu1,df2=nu2); pa[k] = 2*min(p,1-p)}
xp = c(1,1); yp = c(0,1)
# png('varratiopmax170708.png',width=16,height=12,units='cm',res=300)
plot(Fa,pa,type='l',lwd=3,col='blue',xlab='Observed variance ratio F',
     ylab='p-value',las=1,cex.lab=1.5)
lines(xp,yp,col='black',lty=3)
# dev.off()