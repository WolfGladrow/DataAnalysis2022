# print('file: DIboxtransp.R')
# print(date())
# print('2 = transparent box plot (8/2028)')
x = c(2.480, 0.668, 0.542, 1.770, 2.678, 2.931, 0.295, 4.656, 1.825)
# png('Data1Box220828.png',width=16,height=16,units='cm',res=300)
boxplot(x,col='yellow',las=1,xlab='x',cex.lab=1.5) 
L = length(x)
set.seed(1953) # set seed for random number generators
jitter = runif(L,1-0.05,1+0.05)
points(jitter,x,col='blue',lwd=3,cex=0.4)
# dev.off()