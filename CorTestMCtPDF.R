print('file: CorTestMCtPDF.R')
print(date())
# Significant correlations: t_cor following t-dist.? small n: n=6
set.seed(1953)
M = 1e6   # number of Monte Carlo runs
n = 6      # sample size
tcor = numeric(M)
for (m in 1:M) {x=rnorm(n); y=rnorm(n); r=cor(x,y); tcor[m]=r/sqrt((1-r^2)/(n-2))}
# png('MCsigN6Cor170710.png',width=16,height=16,units='cm',res=300)
plot(density(tcor,from=-3.5,to=3.5),lwd=4,col='black',xlim=c(-3,3),ylim=c(0,0.4),
     xlab=expression(t[cor]),main='',cex.lab=1.5)
xp = seq(-3.5,3.5,0.01)
lines(xp,dt(xp,df=n-2),col='magenta',lwd=2,lty=2)
legend('topleft',legend=c('Monte-Carlo','t(x;4)'),col=c('black','magenta'),lty=c(1,2),
       lwd=c(4,2),cex=1.3)
# dev.off()
print(date())
# ----------------------------------------------------------------
# Remarks:
# Monte-Carlo simulation takes about 20 s.
# ----------------------------------------------------------------