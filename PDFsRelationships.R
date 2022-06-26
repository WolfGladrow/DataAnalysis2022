print('file: PDFsRelationships.R')
# Leemis (1986) diagram: PDFs, modified')
# normal, standard normal, uniform, triangular, beta, gamma, chi-squared,
# F, t, exponential, Cauchy
# box:
phi1 = seq(45,135); sr2=sqrt(2)
xphi1 = sr2*sin(pi*phi1/180)
yphi1 = sr2*cos(pi*phi1/180)
xB = c(-1,1,xphi1,1,-1,-xphi1)
yB = c(1,1,yphi1,-1,-1,-yphi1)
# longer box:
xBL = c(-1,1,xphi1,1,-1,-xphi1)*1.5
yBL = c(1,1,yphi1,-1,-1,-yphi1)
sc = 7 # scale
library(latex2exp)
# png('PDFsRelations220625.png',width=16,height=16,units='cm',res=300)
plot(NA,type='p',lwd=4,col='blue',xlab='',ylab='',xlim=c(0,100),ylim=c(0,100),
     col.axis='white',xaxt='n',yaxt='n',bty='n')
# ----------------------------------- Normal
x0=50; y0=90
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'Normal',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,TeX('$\\mu, \\sigma$'),col='black',cex=0.7,pos=1)
arrows(21,y0,39,y0,lty=4,col='magenta',angle=30,length=0.15)
arrows(39,y0,21,y0,lty=4,col='magenta',angle=30,length=0.15)
text(29,y0+8,TeX('$y = e^x$'),col='magenta',cex=0.7,pos=1)
text(29,y0,TeX('$x = log(y)$'),col='magenta',cex=0.7,pos=1)
arrows(79,85,61,y0,lty=2,col='red',angle=10,length=0.1)
text(72,y0+8,TeX('$\\alpha = \\beta \\rightarrow \\infty$'),col='red',cex=0.7,pos=1)
text(69,y0-3,TeX('$\\mu = 0.5, \\sigma \\rightarrow 0$'),col='red',cex=0.7,pos=1)
arrows(49,81,30,68,lty=1,col='black',angle=10,length=0.1)
arrows(30,68,49,81,lty=1,col='black',angle=10,length=0.1)
# ----------------------------------- lognormal
x0 = 10; y0=90
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'lognormal',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$y > 0$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,TeX('$\\alpha, \\beta$'),col='black',cex=0.7,pos=1)
# ----------------------------------- beta
x0 = 90; y0=85
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'beta',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$0 < x 1$'),col='black',cex=0.7,pos=1)
text(x0,y0,TeX('$\\alpha, \\beta$'),col='black',cex=0.7,pos=1)
arrows(90,78,92,37,lty=1,col='black',angle=10,length=0.1)
text(90,75,TeX('$\\alpha = \\beta = 1$'),col='black',cex=0.7,pos=4)
# ----------------------------------- gamma
x0 = 75; y0=68
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'gamma',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$0 < x 1$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,TeX('$\\alpha, \\beta$'),col='black',cex=0.7,pos=1)
arrows(90,78,92,37,lty=1,col='black',angle=10,length=0.1)
arrows(75,76,80,80,lty=4,col='magenta',angle=30,length=0.15)
arrows(75,59,75,38,lty=1,col='black',angle=10,length=0.1)
text(74,49,TeX('$\\beta = 1$'),col='black',cex=0.7,pos=4)
arrows(64,67,58,58,lty=1,col='black',angle=10,length=0.1)
text(48,67,TeX('$\\alpha = n/2$'),col='black',cex=0.7,pos=4)
text(48,62,TeX('$\\beta = 1/2$'),col='black',cex=0.7,pos=4)
# ----------------------------------- standard normal
x0=30; y0=60
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'std. normal',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
arrows(28,51,12,28,lty=4,col='magenta',angle=30,length=0.15)
text(20,35,TeX('$x_1/x_2$'),col='magenta',cex=0.7,pos=1)
arrows(41,60,50,55,lty=1,col='black',angle=10,length=0.1)
# ----------------------------------- Cauchy
x0=10; y0=50
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'Cauchy',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,TeX('$\\theta, \\sigma$'),col='black',cex=0.7,pos=1)
# ----------------------------------- standard Cauchy
x0=10; y0=20
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'std. Cauchy',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
arrows(10,42,10,28,lty=1,col='black',angle=10,length=0.1)
arrows(10,28,10,42,lty=1,col='black',angle=10,length=0.1)
# ----------------------------------- t
x0=30; y0=5
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'t',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,'n',col='black',cex=0.7,pos=1)
arrows(19,y0,10,12,lty=1,col='black',angle=10,length=0.1)
text(12,9,'n=1',col='black',cex=0.7,pos=1)
arrows(30,14,30,52,lty=1,col='black',angle=10,length=0.1)
text(30,49,TeX('$n \\rightarrow \\infty$'),col='black',cex=0.7,pos=4)
arrows(31,14,40,21,lty=4,col='magenta',angle=30,length=0.15)
# ----------------------------------- chi-squared
x0=60; y0=50
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8.5,TeX('$\\chi^2$'),col='blue',cex=0.7,pos=1)
text(x0,y0+3.5,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,'n',col='black',cex=0.7,pos=1)
arrows(67,44,70,38,lty=1,col='black',angle=10,length=0.1)
arrows(50,46,45,38,lty=4,col='magenta',angle=30,length=0.15)
arrows(45,38,50,46,lty=4,col='magenta',angle=30,length=0.15)
# ----------------------------------- F
x0=45; y0=30
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'F',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$x > 0$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,'n',col='black',cex=0.7,pos=1)
# ----------------------------------- exponential
x0=70; y0=30
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'exp.',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$x > 0$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,TeX('$\\alpha$'),col='black',cex=0.7,pos=1)
arrows(70,22,59,16,lty=1,col='black',angle=10,length=0.1)
# ----------------------------------- standard uniform
x0=93; y0=28
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'std. unif.',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$0 < x < 1$'),col='black',cex=0.7,pos=1)
arrows(95,20,95,13,lty=1,col='black',angle=10,length=0.1)
arrows(93,20,72,13,lty=1,col='black',angle=10,length=0.1)
arrows(85,35,80,35,lty=4,col='magenta',angle=30,length=0.15)
# ----------------------------------- uniform
x0=93; y0=5
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'uniform',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$a < x < b$'),col='black',cex=0.7,pos=1)
text(x0,y0,TeX('$a, b$'),col='black',cex=0.7,pos=1)
# ----------------------------------- triangular (tent)
x0=72; y0=5
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'triangular',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$-1 < x < 1$'),col='black',cex=0.7,pos=1)
# ----------------------------------- Weibull
x0=50; y0=10
lines(xB*sc+x0,yB*sc+y0,col='black')
text(x0,y0+8,'Weibull',col='blue',cex=0.7,pos=1)
text(x0,y0+4,TeX('$x > 0$'),col='black',cex=0.7,pos=1)
text(x0,y0+0,TeX('$\\alpha, \\beta$'),col='black',cex=0.7,pos=1)
# dev.off()