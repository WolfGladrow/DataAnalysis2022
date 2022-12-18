print('file: PointEstBinomSuccessProb.R')
print(date())
# Estimate probability of success in single trial (binomial distribution)
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
j = 5    # sample size
n = 10     # total number of trials (binomial PD)
# tnot = 10  # total number of trials
p = 0.6    # probability for success in a single trial (binomial PD)
expectedmean = n*p; print(c(round(expectedmean,4),'expectedmean')) 
expectedvar  = n*p*(1-p); print(c(round(expectedvar,4),'expectedvar')) 
expectedSD = sqrt(expectedvar); print(c(round(expectedSD,4),'expectedSD')) 
bMean = numeric(M)
for(m in 1:M) bMean[m] = mean(rbinom(j,size=n,prob=p))
pEsts = bMean/n
pEst = mean(pEsts); print(c(round(pEst,4),'pEst'))     #  (expected: close to p=0.6)
meanEst = mean(bMean); print(c(round(meanEst,4),'meanEst'))  #  (expected: close to 6)
# Uncertainty of estimate (should depend on sample size j):
varpEsts = var(pEsts); print(c(round(varpEsts,4),'varpEsts'))
varpExp = n*p*(1-p)/n/n/j; print(c(round(varpExp,3),'varpExp'))
sdpEsts = sd(pEsts); print(c(round(sdpEsts,4),'sdpEsts'))
# png('MCpBinom160227.png',width=16,height=16,units='cm',res=300)
hist(pEsts,30,col='blue',xlab=TeX('$\\hat{p}\\, (success\\,  in\\,  single\\,  trial)$'),
     main='',las=1,cex.lab=1.5)
pEstr = round(pEst,4); varpEstsr = round(varpEsts,4); sdpEstsr = round(sdpEsts,4); 
xt = 0.8
text(xt,100,bquote(~mean(hat(p)) == .(pEstr)),col='blue',cex=1.5)
text(xt,80,bquote(~var(hat(p)) == .(varpEstsr)),col='blue',cex=1.5)
text(xt,60,bquote(~sd(hat(p)) == .(sdpEstsr)),col='blue',cex=1.5)
abline(v=p,col='black',lty=1)
abline(v=pEst,col='blue',lty=4)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: PointEstBinomSuccessProb.R"
# "Sun Dec 18 10:54:52 2022"
# "6"       "expectedmean"
# "2.4"     "expectedvar"
# "1.5492"  "expectedSD"
# "0.6001"  "pEst"  
# "6.0006"  "meanEst"
# "0.005"   "varpEsts"
# "0.005"   "varpExp"
# "0.0707"  "sdpEsts"
# -----------------------------------------------------------------------------
