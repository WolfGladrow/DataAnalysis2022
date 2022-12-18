print('file: RejectTrueH0.R')
print(date())
# H0 true: p lower alpha = 0.05 versus B10 > 3.16'
narr = c(10,50,100,500,1000,5000,1e4); L = length(narr)
HighB10 = sqrt(10)
alpha = 0.05
set.seed(1953)
M = 1e5   # number of Monte Carlo runs: 10 min for M = 1e5
print(c(M,'M number of Monte Carlo runs'))
ploweralpha = numeric(L); B10higher3 = numeric(L)
p = numeric(M); B10 = numeric(M)
for(j in 1:L) {
  n = narr[j]
  nu = n-1 # degrees of freedom
  Integrand = function(g) {
    # Rouder et al. (2009, Eq.1)
    return((1+n*g)^(-1/2)*(1+tval^2/((1+n*g)*nu))^(-(nu+1)/2)*(2*pi)^(-1/2)*
             g^(-3/2)*exp(-1/(2*g)))
  }
  for (i in 1:M) {
    x = rnorm(n)    # random sample from standard normal PDF
    p[i] = t.test(x,mu=0)$p.value
    tval = t.test(x,mu=0)$statistic # t-value
    q = integrate(Integrand,lower=0,upper=Inf)$value
    B10[i] = q/(1+tval^2/nu)^(-(nu+1)/2)
  }
  ploweralpha[j] = sum(p < alpha)/M*100    # percent
  B10higher3[j] = sum(B10 > HighB10)/M*100
}
# png('BayesiantTestsign191201st.png',width=16,height=16,units='cm',res=300)
  plot(narr,ploweralpha,type='p',lwd=4,col='black',xlab='n',
       ylab='Percentage',las=1,cex=0.6,log='x',ylim=c(0,max(ploweralpha)),cex.lab=1.5)
  points(narr,B10higher3,col='red',lwd=4,cex=0.6,pch=24)
  abline(h=alpha*100,col='green',lty=4)
# dev.off()
  print(date())
# ----------------------------------------------------------------
# Remarks:
# Calculation took 6 minutes. Recommendation: write results to a file.
# ----------------------------------------------------------------
# narr            10    50   100   500  1000  5000 10000
# ploweralpha  4.892 4.931 5.022 5.011 5.042 5.023 5.001
# B10higher3   2.205 0.951 0.628 0.264 0.185 0.077 0.045