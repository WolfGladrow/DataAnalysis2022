print('file: PointEstMedianVersusMean.R')
# median not equal mean (3/2022)')
library(latex2exp)
# png('Median220304.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(1,2))
# standard normal PDF: median equal mean
dx=0.001; xN = seq(-4,4,dx); yN = dnorm(xN)
xL = seq(-4,0,dx); yL = dnorm(xL)
xNL = c(xL,0,-4); yNL = c(yL,0,0) # normal, left
xR = seq(0,4,dx); yR = dnorm(xR)
xNR = c(xR,4,0); yNR = c(yR,0,0) # normal, left
plot(xN,yN,type='l',lwd=3,col='black',xlab=NA,ylab=NA,las=1,cex=0.4,xlim=c(-3,3))
polygon(xNL,yNL,col='magenta')
polygon(xNR,yNR,col='blue')
lines(xN,yN,lwd=3,col='black')
text(-3.2,0.35,'median',col='magenta',cex=1.2,pos=4)
text(-3.2,0.3,'= 0',col='magenta',cex=1.2,pos=4)
text(1,0.35,TeX('$\\mu = 0$'),col='black',cex=1.2,pos=4)
abline(v=,col='magenta',lty=2)
# ---------------------------------------------------------
# F-PDF: median not equal mean
df1 = 3; df2 = 15
Fmean = df2/(df2-2) # for df2 > 2
print(c(round(Fmean,4),'Fmean'))
xF = seq(0,7,dx); yF = df(xF,df1,df2)
Fmedian = qf(0.5,df1,df2) # no analytic expression known for median of F-PDF
print(c(round(Fmedian,4),'Fmedian'))
xL = seq(0,Fmedian,dx); yL = df(xL,df1,df2)
xFL = c(xL,Fmedian,-4); yFL = c(yL,0,0) # normal, left
xR = seq(Fmedian,6,dx); yR = df(xR,df1,df2)
xFR = c(xR,6,Fmedian); yFR = c(yR,0,0) # normal, left
plot(xF,yF,type='l',lwd=3,col='black',xlab=NA,ylab=NA,las=1,cex=0.4,xlim=c(0,5))
polygon(xFL,yFL,col='magenta')
polygon(xFR,yFR,col='blue')
lines(xF,yF,lwd=3,col='black')
text(1,0.65,'median = 0.83',col='magenta',cex=1.2,pos=4)
text(1,0.45,TeX('$\\mu = 1.15$'),col='black',cex=1.2,pos=4)
abline(v=Fmedian,col='magenta',lty=2)
abline(v=Fmean,col='black',lty=4)
# dev.off()