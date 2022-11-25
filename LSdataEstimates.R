print('file: LSdataEstimates.R')
# generate & plot artificial data & mean values (with respect to y)
beta0 = 8; beta = -1.2 # true intercept & slope
sigma = 1              # true standard deviation of normal noise
# ----------- generate data (begin)
set.seed(1953)                       # set seed for random number generators
x = seq(1,5,0.25); Lx = length(x)    # non-stochastic (uncertainty = 0)
Lr = 10                              # number of data for each x-value ('replicates')
se = sigma/sqrt(Lr); print(c(round(se,4),'standard error'))
y = matrix(data=NA,nrow=Lx,ncol=Lr)  # Lx x Lr = 17 x 10 matrix
for(k in 1:Lx) y[k,] = beta0+beta*x[k]+sigma*rnorm(Lr) # additive normal noise
# ----------- generate data (end)
# ----------- prepare data (matrix -> array) & apply lm()
xarr = numeric(Lr*Lx); yarr = numeric(Lr*Lx)      # matrix -> 1D arrays
for(k in 1:Lr) {xarr[((k-1)*Lx+1):(k*Lx)] = x;    # matrix -> 1D arrays
   yarr[((k-1)*Lx+1):(k*Lx)] = y[,k]}             # matrix -> 1D arrays
out1 = lm(yarr ~ xarr)   # use tilde sign to relate response (yarr) to predictor (xarr)
out2 = summary(out1)     # summary gives more information, especially the uncertainties
b0 = out2$coefficients[1];  print(c(round(b0,4),'intercept b0 (estimate)'))
b = out2$coefficients[2];   print(c(round(b,4),'slope b (estimate)'))
ub0 = out2$coefficients[3]; print(c(round(ub0,4),'intercept uncertainty ub0 (estimate)'))
ub = out2$coefficients[4];  print(c(round(ub,4),'slope uncertainty ub (estimate)'))
b0r = round(b0,2); br = round(b,2); ub0r = round(ub0,2); ubr = round(ub,2); 
# --------------------------- plot:
ymean = numeric(Lx); for(k in 1:Lx) ymean[k] = mean(y[k,])  # mean y-values
# png('SLFestimates220308.png',width=16,height=16,units='cm',res=300)
plot(x,y[,1],type='p',lwd=4,col='black',cex=0.6,ylim=c(0,10),
     xlim=c(0,max(x)),xlab='x',ylab='y',las=1,cex.lab=1.5)
for(k in 2:Lr) points(x,y[,k],lwd=4,col='black',cex=0.6)
xp = c(min(x),max(x))
yp = out1$coefficients[1]+out1$coefficients[2]*xp
lines(xp,yp,col='magenta',lwd=3)
xt = 0.1   # x-position for text
text(xt,2,bquote(~hat(beta)[0] == .(b0r) %+-% .(ub0r)),col='magenta',cex=1.5,pos=4)
text(xt,0.5,bquote(~hat(beta) == .(br) %+-% .(ubr)),col='magenta',cex=1.5,pos=4)
xt = 3
text(xt,9,bquote(~beta[0] == .(beta0)),col='black',cex=1.5,pos=4)
text(xt,7.5,bquote(~beta == .(beta)),col='black',cex=1.5,pos=4)
# dev.off()