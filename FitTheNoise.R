print('file: FitTheNoise.R')
# fitting the noise
print('Generate data:')
set.seed(1953)
x = seq(1,4); n = length(x)
y = -0.3 + 1.3*x+2.1*rnorm(x)
print('Fit polynomial:')
out = lm(y ~ x+I(x^2)+I(x^3))
z = summary(out)
b = z$coefficients[1:n]
xfit = seq(min(x),max(x),0.01); L = length(xfit)
yfit = rep(b[1],L)
for(j in 2:n) yfit = yfit+b[j]*xfit^(j-1)
outSLR = summary(lm(y ~ x))
ic = outSLR$coefficients[1]
slope = outSLR$coefficients[2]
ySLR = ic+slope*xfit
# png('PolyfitPerfect181121.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,
     ylim=c(min(yfit),max(yfit)),cex.lab=1.5)
abline(0,0,col='green',lty=4)
lines(xfit,yfit,col='red')
lines(xfit,ySLR,col='black',lty=2)
# dev.off()