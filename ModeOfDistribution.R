print('file: ModeOfDistribution.R')
print(date())
# purpose: mode of distributions
# created by: Dieter.Wolf-Gladrow@awi.de 
#    10/2024 version 1.0
# ---------------------------------------------------------
print('1 = distributions: uni-, bi-modal (2024)')
print(' ---------------------------------------------------')
print('Discrete, unimodal:')
print(' ---------------------------------------------------')
n = 11; p1=0.2; p2=0.7
k = seq(0,n)
ybinom = dbinom(k,n,p1)
print(' ---------------------------------------------------')
print('Discrete, bimodal:')
print(' ---------------------------------------------------')
ybinomB = dbinom(k,n,p2)
ybinomC = ybinom*2/3 + ybinomB/3
meanB = n*p1
medianB = qbinom(1/2,n,p1)
medianB1 = floor(n*p1)
medianB2 = ceiling(n*p1)
modeB = k[which.max(ybinom)]
modeB1 = floor((n+1)*p1)
modeB2 = ceiling((n+1)*p1)-1
print(' ---------------------------------------------------')
print('Continuous, unimodal: F(x,15,3)')
print(' ---------------------------------------------------')
x = seq(0.01,6,0.01)
nu1 = 15; nu2 = 3
y = df(x,nu1,nu2)
modeF = (nu1-2)/nu1*nu2/(nu2+2)
meanF = nu2/(nu2-2)
medianF = qf(0.5,nu1,nu2)
print(c(round(modeF,4),'modeF'))
print(c(round(meanF,4),'meanF'))
print(c(round(medianF,4),'medianF'))
print(' ---------------------------------------------------')
print('Continuous, bimodal:')
print(' ---------------------------------------------------')
yB = dnorm(x,3,0.7)
yC = y*2/3+yB/3
# png('ModeBimodal24.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(2,2))  # allow for 2 x 2 panels in one plot
plot(k,ybinom,type='p',lwd=4,col='blue',xlab='k',
         ylab='Probability',las=1,cex=0.6,cex.lab=1.5)
abline(v=modeB,col='magenta',lty=2)
abline(v=meanB,col='black',lty=3)
# ---
plot(k,ybinomC,type='p',lwd=4,col='blue',xlab='k',
         ylab='Probability',las=1,cex=0.6,cex.lab=1.5)
# ---
plot(x,y,type='l',lwd=4,col='blue',xlab='x',ylab='Density',las=1,
         cex=0.6,cex.lab=1.5)
    abline(v=modeF,col='magenta',lty=2)
    abline(v=meanF,col='black',lty=3)
    abline(v=medianF,col='green',lty=4)
# ---
plot(x,yC,type='l',lwd=4,col='blue',xlab='x',ylab='Density',las=1,cex=0.6,
         cex.lab=1.5)
# dev.off()
  