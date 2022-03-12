WMWpMC = function(n,m) {
  # -------------------------------------------------
  # Monte Carlo estimate of probabilities of test statistic U
  # for Wilcoxon-Mann-Whitney two-sided test
  # created by: Dieter.Wolf-Gladrow@awi.de 6/2018
  # file: WMWpMC.R; how to source: source('WMWpMC.R'); how to use: p=WMWpMC(n,m)
  # ----------------------------------------------------
  # if ((n < 9) || (m < 9)) {                     # 3L
  jmin = sum(seq(1:m)); jmax = sum(seq(n+1,n+m));
  # -> U = m*n+m*(m+1)/2-T can vary between
  Umin = 0; Umax = n*m
  fr = numeric(Umax+1);   # frequencies
  set.seed(1953) # set seed for random number generators
  M = 1e6;       # number of Monte Carlo runs 
  # M = 10^6 -> very good results for n=8, m=7
  Ua = numeric(M);
  for(j in 1:M) {
    x = rnorm(n); y = rnorm(m); # random samples from normal distribution
    z = c(x,y); rz = rank(z); 
    sry = sum(rz[(n+1):(n+m)]); # = test statistic T
    U = m*n+m*(m+1)/2-sry;
    Ua[j] = U;
    fr[U+1] = fr[U+1] + 1
  }
  Uamin = min(Ua);
  Uamax = max(Ua);
  pMC = fr/M;      # relative frequencies
  # probability for left & right tail:
  # pLeft = sum(pMC[1:floor(Usample+1)]);  # pLeft
  # pRight = sum(pMC[ceiling(Usample+1):(Umax+1)]);
  return(pMC)
}