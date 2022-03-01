print('file: CLTdensity1e5MC.R')
# Central Limit Theorem (CLT): sum of random numbers -> normal distribution
M = 1e5  
N = 10000   # number of random numbers to sum up
S = numeric(M) # array for sums of random numbers
set.seed(1953) # set seed for random number generators
for(k in 1:M) S[k] = sum(runif(N))
Smean = mean(S); Ssd = sd(S)
SmeanTheory = N/2; SsdTheory = sqrt(N/12)
# png('SumRandomM100000DensityBook160104.png',width=16,height=12,units='cm',res=300)
plot(density(S,from=4900,to=5100),col='blue',lwd=3,main='',
       xlab='Sum of 10000 random numbers',xlim=c(4925,5075),las=0,cex.lab=1.5)
  text(4927,0.01,pos=4,'M = 100000',col='blue',cex=1.5)
  x = seq(4900,5100,1); y = dnorm(x,SmeanTheory,SsdTheory)
  lines(x,y,col='red',lwd=2,lty=2)
  legend('bottom',legend=c('MC density estimate','normal'),col=c('blue','red'),
         lty=c(1,2),lwd=c(3,2))
# dev.off()
# --------------------------------------------------------------
# Results:
print(c('M = ',M))                                 
print(c('Smean = ',round(Smean,3)))                
print(c('should be close to ',SmeanTheory))        
print(c('Ssd = ',round(Ssd,3)))                    
print(c('should be close to ',round(SsdTheory,3))) 
