print('file: PoissonRegr1Exam.R')
# Poisson regression: generate artificial data set 
# (1) exact model: log(mu) = beta0 + beta*x;  -1 <= x <= +1
beta0 = 1.3; beta = 1.2; xmin = -1; xmax = 1
n = 30                 # sample size
dx = (xmax-xmin)/(n-1); x = round(seq(xmin,xmax,dx),3)
set.seed(1953)         # set seed for random number generators
logmu = beta0+beta*x
mu = exp(beta0+beta*x)
y = numeric(n)
for(k in 1:n) y[k] = rpois(1,lambda=mu[k])
# (3) Poisson regression
out1 = glm(y ~ x,family=poisson)  # < -------------- Poisson regression
out2 = summary(out1)
b0 = out2$coefficients[1]  # estimate of beta0
b  = out2$coefficients[2]  # estimate of beta
ub0 = out2$coefficients[3] # uncertainty of beta0 estimate
ub  = out2$coefficients[4] # uncertainty of beta estimate
muest = exp(b0+b*x)
# png('PoissonRegArt160930.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
beta0=1.3; beta=1.2; xp=seq(-1,1,0.01); yp=exp(beta0+xp*beta)
lines(xp,yp,col='black',lwd=2,lty=4)
lines(x,muest,col='blue',lwd=3,lty=1)
points(x,y,col='blue',lwd=3,cex=0.4)
br = round(b,2); ubr = round(ub,2)
text(-1,9,bquote(~hat(beta) == .(br) %+-% .(ubr)),col='blue',pos=4,cex=1.5)
b0r = round(b0,2); ub0r = round(ub0,2)
text(-1,12,bquote(~hat(beta)[0] == .(b0r) %+-% .(ub0r)),col='blue',pos=4,cex=1.5)
text(0.3,11,bquote(~beta[0] == .(beta0)),col='black',pos=4,cex=1.5)
text(0.3,14,bquote(~beta == .(beta)),col='black',pos=4,cex=1.5)
text(0.3,17,TeX('$log(\\mu) = \\beta_0 + \\beta x$'),col='black',cex=1.5)
legend('topleft',legend=c('estimated','true'),col=c('blue','black'),lty=c(1,4),lwd=c(3,2),cex=1.5)
# dev.off()
# -----------------------------------------------------------------------------
# Remarks:
# When applying Poisson regression one assumes that the data stem from Poisson
#   distributions. The link function is always the exponential function; this
#   assures that the mean rates of the various Poisson distributions are non-negative.
# The Poisson regression is performed by calling the R routine glm() for fitting 
#   Generalized Linear Models (GLMs). The call is simply glm(y ~ x,family=poisson),
#   i.e. similar to the call for simple linear regression with x as predictor and
#   y as response, except that glm (instead of lm) is called and the family 
#   parameter is specified as poisson (note: lowercase).
# -----------------------------------------------------------------------------


# 