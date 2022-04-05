print('file: Cox46Next4.R')
# Cox(1946) probability for 4 in next trial
p1 = 1/3; p2 = 1/6; N = 10; k = seq(0,10)
p = (p1*dbinom(k,N,p1)+p2*dbinom(k,N,p2))/(dbinom(k,N,p1)+dbinom(k,N,p2))
# png('Cox46FourInNext171024.png',width=16,height=16,units='cm',res=300)
plot(k,p,type='p',lwd=4,col='blue',xlab='Number of successes',
     ylab='Probability',las=1,cex=0.6,ylim=c(0,0.4),cex.lab=1.5)
abline(a=p1,b=0,col='magenta')
abline(a=p2,b=0,col='black')
# dev.off()