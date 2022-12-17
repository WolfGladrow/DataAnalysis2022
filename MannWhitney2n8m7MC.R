print('file: MannWhitney2n8m7MC.R') 
print(date())
# PD & CDF Mann & Whitney (1947) 
n = 8; m = 7; Ua = seq(0,n*m)
L = n*m+1; pcum = numeric(L)
# -------------------------------------------------
print('Monte Carlo estimate:')
source('WMWpMC.R')
pMC = WMWpMC(n,m)
pcumMC = numeric(L)  # cumulative probabilities
pcumMC[1] = pMC[1]
for(k in 2:L) pcumMC[k] = pcumMC[k-1]+pMC[k]
# -------------------------------------------------
# Mann & Whitney (1947, Table I):
pcum1 = c(0,0,0.001,0.001,0.002,0.003,0.005,0.007,0.01,
          0.014,0.02,0.027,0.036,0.047,0.06,0.076,0.095,
          0.116,0.140,0.168,0.198,0.232,0.268,0.306,0.347,
          0.389,0.433,0.478,0.522) 
pcum[1:length(pcum1)] = pcum1
p = numeric(L); p[1] = pcum[1]
Lhalf = 1+round(n*m/2);
for(k in 2:Lhalf) p[k] = pcum[k] - pcum[k-1]
# symmetrical extension: 'p[Lhalf+k] = p[Lhalf-k]'
for(k in 1:(Lhalf-1)) p[Lhalf+k] = p[Lhalf-k]
for(k in 1:(Lhalf-1)) pcum[Lhalf+k] = pcum[Lhalf+k-1]+p[Lhalf+k]
print(c('sum(p) = ',sum(p)))
print(c('pcum[L] = ',pcum[L]))
# -------------------------------------------------
print('test symmetry:')
Lh = (L-1)/2; p1 = pMC[1:Lh]; p2 = numeric(Lh)
for(k in 1:Lh) p2[k] = pMC[L+1-k]
sflag = 2
if (sflag == 2) {
  # png('Wilcoxon8n7mpMC180624.png',width=16,height=16,units='cm',res=300)
  plot(pMC,type='p',lwd=4,col='blue',xlab='U',
       ylab='Monte Carlo estimate of p',las=1,cex=0.6,cex.lab=1.5)
  # dev.off()
}
print(date())
# -----------------------------------------------------------------------------
# Remarks:
# Run time 24 s
# -----------------------------------------------------------------------------