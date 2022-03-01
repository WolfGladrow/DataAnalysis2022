print('file: Random50RNfromTentPDF.R')
# 50 random numbers from tent PDF
set.seed(1953) # set seed for random number generators
L = 50; r = runif(L); # uniformly distributed random numbers
y = numeric(L)        # array of length L
for (k in 1:L) {            # transformation y(x)
  x = r[k];
  if (x < 0.5) {y[k] = sqrt(x/2)}
  else {y[k] = 1-sqrt((1-x)/2)}}
xp = seq(1,L)         # sequence 1,2,...,L
# png('tentRN160722.png',width=16,height=12,units='cm',res=300)
plot(xp,y,type='p',lwd=3,col='blue',xlab='#',ylab='Random numbers from tent PDF',
     las=1,cex=0.4,cex.lab=1.5)
# dev.off()