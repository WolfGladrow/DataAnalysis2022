WilcoxonPaired = function(d0) {
  # file: WilcoxonPaired.R
  # 2-sided Wilcoxon paired-sample test (Zar, 2010, p.183ff)
  # input: differences d0
  # remark: clean version; additional output if wflag > 0
  # created by Dieter.Wolf-Gladrow@awi.de 6/2018
  alpha = 0.05   # chosen level of significance
  wflag = 0  # write flag
  q = -1  # return variable (dummy)
  if (wflag > -1) print('---------- function WilcoxonPaired ----------')
  # -----------------------------------------------------------
  # (1) remove zeros
  n0 = length(d0); d = c()   # dynamic array
  for(k in 1:n0) if (d0[k] != 0) d = c(d,d0[k])
  n = length(d); r = rank(abs(d)); sr = sign(d)*r; 
  if (wflag > -1) print(c('n = ',n))
  # (2) n is too small ----------------------------------------
  if (n < 7) print(c('n < 7 is too small -> STOP, n = ',n))
  # (3) small n: use critical values --------------------------
  if (n < 101) {
    # (3a) calculate test statistics: -------------------------
    Tplus = 0; Tminus = 0; 
    for(k in 1:n) if (sr[k] > 0 ) Tplus = Tplus + sr[k] else
      Tminus = Tminus + abs(sr[k])
    TminusObs = Tminus; TplusObs = Tplus
    if (wflag > 0) print(c('TplusObs, TminusObs = ',TplusObs,TminusObs))
    if (wflag > 0) print(c('sum(seq(1:n)) = ',sum(seq(1:n))))    # test
    if (wflag > 0) print(c('Tplus + Tminus = ',Tplus + Tminus))  # test
    # (3b) critical T values (Zar, 2010, p.758) for n = 1,2,3,4,...,100
    #      2-sided, alpha = 0.05
    Tc2 = c(NA,NA,NA,NA,NA,NA,2,3,5,8,10,13,17,21,25,29,34,40,46,52,58,65,
            73,81,89,98,106,116,126,136,148,159,170,182,194,208,221,235,249,263,
            278,295,310,326,342,360,379,396,414,434,454,473,495,515,535,558,580,
            604,624,647,671,696,724,749,773,801,825,850,877,910,934,966,994,1024,
            1052,1085,1114,1146,1179,1210,1243,1278,1314,1347,1378,1414,1448,1490, 
            1523,1562,1597,1634,1675,1710,1754,1789,1833,1871,1911,1956)
    Talpha2n = Tc2[n] # for alpha = 0.05, 2-sided
    if (wflag > -1) print(c('TminusObs = ',TminusObs))
    if (wflag > -1) print(c('TplusObs = ',TplusObs))
    if (wflag > -1) print(c('T_0.05(2)n = ',Talpha2n,' (critical T value)'))
    if ((TminusObs <= Talpha2n) || (TplusObs <= Talpha2n))
      print('H0 rejected -> difference not equal 0') else
        print('H0 accepted -> difference is 0')
    q = 1
    # (3c) ---------------------------------------------------------
    if (wflag > -1) print('Estimate p-value by Monte Carlo simulation:')
    set.seed(1953) # set seed for random number generators
    M = 1e5
    Tminusa = numeric(M)
    L = n*(n+1)/2+1  # range of T values from 0 to n*(n+1)/2
    fr = numeric(L)
    CDF = numeric(L)
    for(j in 1:M) {
      x = rnorm(n); y = rnorm(n); # random samples from normal PDF
      d = x-y; # difference
      r = rank(abs(d));   # ranks 
      sr = sign(d)*r;     # signed ranks
      Tplus = 0; Tminus = 0;
      for(k in 1:n) if (sr[k] > 0 ) Tplus = Tplus + sr[k] else
        Tminus = Tminus + abs(sr[k]);
      Tminusa[j] = Tminus;
      fr[Tminus+1] = fr[Tminus+1] + 1
    }
    # (3d) relative frequencies -------------------------------------------
    rf = fr/M    # relative frequencies = estimate of probabilities for T_-
    # (4b) cumulative density functions (CDFs)
    CDF[1] = rf[1]
    for (k in 2:L) CDF[k] = CDF[k-1] + rf[k]
    if (TminusObs < TplusObs) pvalue2 = CDF[TminusObs+1] + 1-CDF[TplusObs] else
      pvalue2 = 1 - CDF[TminusObs+1] + CDF[TplusObs]
    # ----------------------------------------------------------------------
    print(c('p-value = ',round(pvalue2,4)))
    # ----------------------------------------------------------------------
    # (3e) ---------------------------------------------------------
    if (wflag > 0) print('normal approximation:')
    if (wflag > 0) print('  for n < 100 this is an approximation only')
    # Zar (2010, p.186)
    muT = n*(n+1)/4                   # mean value (theoretical value)
    if (wflag > 0) print(c('muT = ',round(muT,3)))
    sigmaT = sqrt(n*(n+1)*(2*n+1)/24) # standard deviation (theoretical value)
    z1 = (TminusObs-muT)/sigmaT       # |z1| should be equal to |z2|
    z2 = (TplusObs-muT)/sigmaT
    p = 2*pnorm(-(abs(z1)))           # 2-sided p-value
    # if (wflag > -1) print(c('normal app. z1, z2, p = ',round(c(z1,z2,p),4)))
    if (wflag > -1) print(c('z1, z2 = ',round(c(z1,z2),4),' (normal app.)'))
    if (wflag > -1) print(c('p-value = ',round(c(p),4))) #,' (normal app.)'))
    if (p < alpha)  print('H0 rejected -> difference not equal 0') else
      print('H0 not rejected')
    #    print('H0 accepted -> difference is 0')
  }  # end of n < 101
  # (4) normal approximation for n > 100
  if (n > 100) {
    if (wflag > 0) print('normal approximation:')
    # Zar (2010, p.186)
    muT = n*(n+1)/4                   # mean value (theoretical value)
    if (wflag > 0) print(c('muT = ',round(muT,3)))
    sigmaT = sqrt(n*(n+1)*(2*n+1)/24) # standard deviation (theoretical value)
    z1 = (TminusObs-muT)/sigmaT       # |z1| should be equal to |z2|
    z2 = (TplusObs-muT)/sigmaT
    p = 2*pnorm(-(abs(z1)))           # 2-sided p-value
    if (wflag > -1) print(c('z1, z2 = ',round(c(z1,z2),4),' (normal app.)'))
    if (wflag > -1) print(c('p-value = ',round(c(p),4))) #,' (normal app.)'))
    # if (p < alpha)  print('H0 rejected -> difference not equal 0') else
    if (p < alpha)  print('H0 rejected (p < alpha=0.05)') else
      print('H0 not rejected')
  } # end of n > 100
  return(q)
}