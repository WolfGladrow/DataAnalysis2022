print('file: WilcoxonPairedCritValMC2.R')
# Wilcoxon paired-sample critical values: Monte Carlo
set.seed(1953) # set seed for random number generators
M = 1e5 # Tplusa = numeric(M); Tminusa = numeric(M)
nmax = 100; nseq = seq(1,nmax)
cr1a = numeric(nmax); cr2a = numeric(nmax)
for(i in 7:nmax) { 
  n=nseq[i]; L = n*(n+1)/2+1; fr = numeric(L); CDF = numeric(L)
  for(j in 1:M) {
    x = rnorm(n); y = rnorm(n); # random samples from normal PDF
    d = x-y; # difference
    r = rank(abs(d));   # ranks
    sr = sign(d)*r;     # signed ranks
    Tplus = 0; # Tminus = 0;
    for(k in 1:n) if (sr[k] > 0 ) Tplus = Tplus + sr[k] # else
    # Tminus = Tminus + abs(sr[k]);
    # Tplusa[j] = Tplus; Tminusa[j] = Tminus;
    fr[Tplus+1] = fr[Tplus+1] + 1   
  }
  rf = fr/M       # relative frequencies = estimate of probabilities
  CDF[1] = rf[1]  # CDF
  for (k in 2:L) CDF[k] = CDF[k-1] + rf[k]
  cr1 = 0; cr2 = 0
  alpha = 0.05 # level of significance
  alphaHalf = alpha/2
  for(k in 1:L) { 
    if(CDF[k] <= alpha) cr1 = k-1;
    if(CDF[k] <= alphaHalf) cr2 = k-1}
  cr1a[i] = cr1; cr2a[i] = cr2
}
for(i in 1:6) {cr1a[i] = NA; cr2a[i] = NA}
# write.table(cr1a,file='WilcoxonCR1a180612.txt')
# write.table(cr2a,file='WilcoxonCR2a180612.txt')
sflag = 2
if (sflag == 2) {
  # png('WilcoxonCV2sided180606.png',width=16,height=12,units='cm',res=300)
  plot(cr2a,type='p',lwd=4,col='blue',xlab='n',ylab='Critical values (2-sided)',
       las=1,cex=0.6,cex.lab=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
#   calculations take several minutes -> save output: write.table()
# ----------------------------------------------------------------