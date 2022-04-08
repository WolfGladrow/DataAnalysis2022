print('file: PermutationTestCor.R')
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
# png('Greenacre13DepthPollution200523f.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
text(7,90,paste('r = ',as.character(round(r,4))),col='black',pos=4,cex=1.5)
text(7,80,paste('p = ',as.character(round(p,4))),col='black',pos=4,cex=1.5)
# dev.off()