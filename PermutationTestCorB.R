print('file: PermutationTestCorB.R')
# permutation test for correlation: example 
x = c(4.8,2.8,5.4,8.2,3.9,2.6,4.6,5.1,3.9,10.0,6.5,3.8,
      9.4,4.7,6.7,2.8,6.4,4.4,3.1,5.6,4.3,1.9,2.4,4.3,
      2.0,2.5,2.1,3.4,6.0,1.9)
y = c(72,75,59,64,61,94,53,61,68,69,57,84,53,83,100,
      84,96,74,79,73,59,54,95,64,97,78,85,92,51,99)
r = cor(x,y); print(c(round(r,4),'r'))
source('CorTestPerm.R')
p = CorTestPerm(x,y)
print(c(round(p,4),'p-value (correlation test)'))
# ---------- Monte Carlo simulation -> histogram of r-values
M = 1e4
n = length(x); 
permcor <- rep(0,M) # generate array of zeros
permcor[1] <- cor(x,y)
set.seed(1953)
for(iperm in 2:M) {
  yperm <- y[sample(n)] # permutation = sample without replacement
  permcor[iperm] <- cor(x,yperm)}
pPermut = sum(abs(permcor)>=abs(permcor[1]))/M # relative frequency
sflag = 2
if (sflag == 2) {
  # png('CorTestPermHist200524.png',width=16,height=12,units='cm',res=300)
  hist(permcor,breaks=seq(-0.7,0.7,0.05),main='',
       xlab='Correlation',ylab='Frequency',col='blue',las=1,cex.lab=1.5)
  xp = c(r,r); yp = c(0,500)
  lines(-xp,yp,col='black',lty=3)
  lines(xp,yp,col='black')
  text(-0.6,970,paste('p = ',as.character(round(pPermut,3))),col='black',pos=4,cex=1.5)
  # dev.off()
}