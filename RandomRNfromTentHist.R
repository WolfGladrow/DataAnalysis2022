print('file: RandomRNfromTentHist.R')
# histogram of 1000 random numbers from tent PDF
set.seed(1953) # set seed for random number generators
L = 1000; r = runif(L);  # uniformly distributed random numbers
y = numeric(L)        # array of length L
for (k in 1:L) {      # transformation y(x)
  x = r[k];
  if (x < 0.5) {y[k] = sqrt(x/2)}
  else {y[k] = 1-sqrt((1-x)/2)}}
nob = floor(sqrt(L))  # number of bins 
# floor(x) = rounding to largest integer not greater than x
# png('tenthist160722.png',width=16,height=12,units='cm',res=300)
out = hist(y,nob,xlab='y',ylab='Frequency',main='',col='blue',las=1,cex.lab=1.5)
Xh = out$breaks; # bin coordinates (Xh)
dx = Xh[2]-Xh[1]; c = L*dx; # scale factor
xp1 = c(0,1/2); yp1 = c(0,2);   # q(y) = 4*y   => 2 for y=1/2
xp2 = c(1/2,1); yp2 = c(2,0);   # q(y) = 4-4*y => 2 for y=1/2
lines(xp1,yp1*c,type='l',col='red',lwd=2)
lines(xp2,yp2*c,type='l',col='red',lwd=2)
# dev.off()