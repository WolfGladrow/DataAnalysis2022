print('file: Solution-F-PDFs.R')
# EXERCISE & SOLUTION: plot F(x; nu1, nu2) & F(x; nu2, nu1)
nu1 = 3; nu2 = 15   # degrees of freedom (for 19 data points in 4 groups)
x1 = seq(0,10,0.01)
F1 = df(x1,df1=nu1,df2=nu2)
F2 = df(x1,df1=nu2,df2=nu1)
# png('Fnu1nu2X170704.png',width=16,height=12,units='cm',res=300)
plot(x1,F1,type='l',lwd=3,col='blue',xlab='x',ylab=NA,cex.lab=1.5,
     las=1,xaxs='i',yaxs='i',ylim=c(0,0.75),xlim=c(-0.1,10))
title(ylab=expression(paste('F(x; ',nu[1],'=3',',',nu[2],'=15), F(x; ',nu[1],'=15',',',nu[2],'=3)')),
      line=2.3,cex.lab=1.5)
lines(x1,F2,col='black',lwd=3,lty=4)
legend('topright',legend=c('F(x;3,15)','F(x;15,3)'),col=c('blue','black'),
       lty=c(1,4),lwd=c(3,3))
# dev.off()