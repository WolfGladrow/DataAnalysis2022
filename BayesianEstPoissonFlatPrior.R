print('file: BayesianEstPoissonFlatPrior.R')
# Poisson: Bayesian estimation: flat prior, n = 30 
set.seed(1953); 
lambdaTrue = 2.7 # true rate (mean number of events) = variance
n = 30           # sample size
y = rpois(n,lambdaTrue) # take random sample
tss = sum(y); print(c(tss,'sufficient statistic t'))
samplemean = mean(y); print(c(round(samplemean,3),'samplemean'))
print(' ---------------------------------------------------')
print('Likelihood function:')
dlambda = 0.01; lambdaA = seq(1.5,4.5,dlambda); L = length(lambdaA)
Like = numeric(L)
for(j in 1:L) {
  lambdaj = lambdaA[j]; expj = exp(-lambdaj); Like[j] = 1
  for(i in 1:n) Like[j] = Like[j]*lambdaj^(y[i])/factorial(y[i])*expj
}
(qnorm = sum(Like)*dlambda)
Like = Like/qnorm
print(' ---------------------------------------------------')
print('Analyze likelihood function:')
mean1 = sum(lambdaA*Like)*dlambda; print(c(round(mean1,4),'mean1'))
var1 = sum((lambdaA-mean1)^2*Like)*dlambda; 
print(c(round(var1,4),'var1'))
sd1 = sqrt(var1); print(c(round(sd1,4),'sd1'))
mode1 = lambdaA[which.max(Like)]; print(c(round(mode1,4),'mode1'))
mode2 = mean(y); print(c(round(mode2,4),'mode2 = MLE'))
Lcdf = length(lambdaA)
CDF = numeric(Lcdf)
CDF[1] = Like[1]*dlambda
for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Like[i]*dlambda
(median1 = lambdaA[which.min((CDF-0.5)^2)])
print(c(round(median1,3),'median1'))
print(' ---------------------------------------------------')
print('95% interval')
ahalf = 0.05/2
I95L = lambdaA[which.min((CDF-ahalf)^2)]; print(c(round(I95L,4),'I95L'))
I95U = lambdaA[which.min((CDF-(1-ahalf))^2)]; print(c(round(I95U,4),'I95U'))
xI95 = c(I95L,I95U); yI95 = c(0,0)
print(' ---------------------------------------------------')
print('Plot:')
library(latex2exp)
# png('PoissonLikeFct30n210620.png',width=16,height=12,units='cm',res=300)
plot(lambdaA,Like,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,cex=0.4)
abline(v=lambdaTrue,col='black')
abline(v=mean1,col='blue',lty=2)
abline(v=mode1,col='green',lty=3)
abline(v=median1,col='magenta',lty=4)
lines(xI95,yI95,col='magenta',lwd=3)
xt = 3.5
text(2.3,0.07,'95% interval',col='magenta',pos=4,cex=1.5)
text(1.5,1.2,TeX('$\\lambda = 2.7$'),col='black',pos=4,cex=1.5)
title(xlab=TeX('Rate parameter $\\lambda$'),cex.lab=1.5)
title(ylab=TeX('Liklihood function $p(\\lambda | y)$'),line=2.5,cex.lab=1.5)
text(0,1.2,TeX('$\\lambda = 2.7$'),col='black',pos=4,cex=1.5)
text(xt,1.2,paste('n = ',as.character(n)),col='blue',pos=4,cex=1.5)
text(xt,1.1,paste('mean = ',as.character(round(mean1,3))),col='blue',pos=4,cex=1.5)
text(xt,1.0,paste('sd = ',as.character(round(sd1,3))),col='blue',pos=4,cex=1.5)
text(xt,0.9,paste('mode = ',as.character(round(mode1,3))),col='green',pos=4,cex=1.5)
text(xt,0.8,paste('median = ',as.character(round(median1,3))),col='magenta',pos=4,cex=1.5)
# dev.off()