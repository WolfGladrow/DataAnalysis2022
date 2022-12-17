print('file: CLThistMC.R')
print(date())
# Central Limit Theorem (CLT): sum of random numbers -> normal distribution
M = 1000    # number of Monte Carlo runs
N = 10000   # number of random numbers to sum up
S = numeric(M) # array for sums of random numbers
set.seed(1953) # set seed for random number generators
for(k in 1:M) S[k] = sum(runif(N))
Smean = mean(S); Ssd = sd(S)
SmeanTheory = N/2; SsdTheory = sqrt(N/12)
# png('SumRandomBook160104.png',width=16,height=16,units='cm',res=300)
hist(S,30,col='blue',xlab='Sum of 10000 random numbers',main='',las=1,cex.lab=1.5)
abline(v=5000,col='black',lty=4)
# dev.off()
# --------------------------------------------------------------
# Results:
print(c('M = ',M))                               
print(c('Smean = ',round(Smean,3)))       
print(c('should be close to ',SmeanTheory))   
print(c('Ssd = ',round(Ssd,3)))        
print(c('should be close to ',round(SsdTheory,3))) 
# --------------------------------------------------------------
# "file: CLThistMC.R"
# "Sat Dec 17 10:16:46 2022"
# "M = " "1000"
# "Smean = " "5001.714"
# "should be close to " "5000"               
# "Ssd = " "29.185"
# "should be close to " "28.868"             
# "Sat Dec 17 10:16:47 2022"
# --------------------------------------------------------------


