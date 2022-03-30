print('file: PDsPDFsBinomPD10n.R')
# binomial distribution
n = 10; k = seq(0,n); p = 0.25; pk = dbinom(x=k,size=n,prob=p)
# png('Binom160817.png',width=16,height=12,units='cm',res=300)
plot(k,pk,type='p',lwd=4,col='blue',xlab='k',ylab='Probability',las=1,cex=0.6,cex.lab=1.5)
text(5,0.25,bquote(~n == .(n)),col='blue',cex=1.5,pos=4)
text(5,0.2,bquote(~p == .(p)),col='blue',cex=1.5,pos=4)
# dev.off()