print('file: MLRn10AICc.R')
print(date())
# created by: Dieter.Wolf-Gladrow@awi.de 11/2022 version 1.0
# ---------------------------------------------------------
print('-----------------------------------------------------')
print('(1) generate artificial data:')
print('-----------------------------------------------------')
  # True model: Yexact=beta_0+beta_1*X1+beta_2*X2+beta_3*X3 
  # The predictor matrix X should contain at least 2 
  # highly-correlated columns plus at least 1 column that is 
  # not correlated to the highly-correlated columns
  # -> at n times 3 matrix  (n = sample size = number of rows)
  # Correlated columns can be generated using function 
  # samplecorFct(r,n).
  # (MAY BE NOT RELEVANT: however, samplecorFct(r,n) does not 
  # provide true values)
  # Generation of Y: choose intercept beta_0 and slopes beta_j, then calculate
  # Yexact = beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * X3
  # and finally add normal noise Y = Yexact + noise
  # Question: How well can one estimate intercept and slopes 
  #    from multiple regression?
  # Model parameters:
  beta0 = 1.5; beta1 = -0.3; beta2 = 0.8; beta3 = 1; sigma = 0.8
  n = 10  # sample size
  r = 0.9 # correlation coefficient between X1 and X2
  source('samplecorFct.R')
  set.seed(1953) # set seed for random number generators
  out1 = samplecorFct(r,n); x1 = out1[1:n]; x2 = out1[(n+1):(2*n)];
  x3 = runif(n) # random sample from uniform PDF
  # Correlation matrix:
  X = matrix(data=c(x1,x2,x3),nrow=n,ncol=3)
  print('Correlation matrix for predictors:')
  print(round(cor(X),4))
  yexact = beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
  y = yexact+rnorm(n,0,sigma) # add normal noise with mu=0
  print('-----------------------------------------------------')
  print('(2) Models M0, ..., M3')
  print('   Model M0: all 3 predictors (global model)')
  print('-----------------------------------------------------')
  M0 = lm(y ~ x1+x2+x3); M0s = summary(M0)
  print(c(round(as.numeric(M0$coefficients[1]),2),'intercept model M0'))
  print(c(round(as.numeric(M0$coefficients[2]),2),'1. slope model M0'))
  print(c(round(as.numeric(M0$coefficients[3]),2),'2. slope model M0'))
  print(c(round(as.numeric(M0$coefficients[4]),2),'3. slope model M0'))
  print(c(round(as.numeric(M0s$coefficients[5]),2),'uncertainty of intercept model M0'))
  print(c(round(as.numeric(M0s$coefficients[6]),2),'uncertainty of 1. slope model M0'))
  print(c(round(as.numeric(M0s$coefficients[7]),2),'uncertainty of 2. slope model M0'))
  print(c(round(as.numeric(M0s$coefficients[8]),2),'uncertainty of 3. slope model M0'))
  AICM0 = AIC(M0); print(c(round(AICM0,1),'AICM0'))
  k0 = 1+3+1 # number of model parameters: IC, 3 slopes, var. of noise
  AICcM0 = AICM0 + 2*k0*(k0+1)/(n-k0-1); print(c(round(AICcM0,1),'AICcM0'))
  print('-----------------------------------------------------')
  print('   Model M1: drop 1. predictor:')
  print('-----------------------------------------------------')
  M1 = lm(y ~ x2+x3); M1s = summary(M1)
  print(c(round(as.numeric(M1$coefficients[1]),2),'intercept model M1'))
  print(c(round(as.numeric(M1$coefficients[2]),2),'x2 slope model M1'))
  print(c(round(as.numeric(M1$coefficients[3]),2),'x3 slope model M1'))
  print(c(round(as.numeric(M1s$coefficients[4]),2),'uncertainty of intercept model M1'))
  print(c(round(as.numeric(M1s$coefficients[5]),2),'uncertainty of x2 slope model M1'))
  print(c(round(as.numeric(M1s$coefficients[6]),2),'uncertainty of x3 slope model M1'))
  AICM1 = AIC(M1); print(c(round(AICM1,1),'AICM1'))
  k = 1+2+1 # number of model parameters: IC, 2 slopes, var. of noise (for M2,M3,M4)
  AICcM1 = AICM1 + 2*k*(k+1)/(n-k-1); print(c(round(AICcM1,1),'AICcM1'))
  print('-----------------------------------------------------')
  print('   Model M2: drop 2. predictor:')
  print('-----------------------------------------------------')
  M2 = lm(y ~ x1+x3); M2s = summary(M2)
  print(c(round(as.numeric(M2$coefficients[1]),2),'intercept model M2'))
  print(c(round(as.numeric(M2$coefficients[2]),2),'x1 slope model M2'))
  print(c(round(as.numeric(M2$coefficients[3]),2),'x3 slope model M2'))
  print(c(round(as.numeric(M2s$coefficients[4]),2),'uncertainty of intercept model M2'))
  print(c(round(as.numeric(M2s$coefficients[5]),2),'uncertainty of x1 slope model M2'))
  print(c(round(as.numeric(M2s$coefficients[6]),2),'uncertainty of x3 slope model M2'))
  AICM2 = AIC(M2); print(c(round(AICM2,1),'AICM2'))
  AICcM2 = AICM2 + 2*k*(k+1)/(n-k-1); print(c(round(AICcM2,1),'AICcM2'))
  print('-----------------------------------------------------')
  print('   Model M3: drop 3. predictor:')
  print('-----------------------------------------------------')
  M3 = lm(y ~ x1+x2); M3s = summary(M3)
  print(c(round(as.numeric(M3$coefficients[1]),2),'intercept model M3'))
  print(c(round(as.numeric(M3$coefficients[2]),2),'x1 slope model M3'))
  print(c(round(as.numeric(M3$coefficients[3]),2),'x2 slope model M3'))
  print(c(round(as.numeric(M3s$coefficients[4]),2),'uncertainty of intercept model M3'))
  print(c(round(as.numeric(M3s$coefficients[5]),2),'uncertainty of x1 slope model M3'))
  print(c(round(as.numeric(M3s$coefficients[6]),2),'uncertainty of x2 slope model M3'))
  AICM3 = AIC(M3); print(c(round(AICM3,1),'AICM3'))
  AICcM3 = AICM3 + 2*k*(k+1)/(n-k-1); print(c(round(AICcM3,1),'AICcM3'))
  print('-----------------------------------------------------')
  print('(3) Find minimum AICc')
  print('-----------------------------------------------------')
  AICcs = c(AICcM0,AICcM1,AICcM2,AICcM3)
  j = which.min(AICcs)
  print(c('Model M',j-1,'has minimum AICc'))
  AICcmin = AICcs[j]
  print('-----------------------------------------------------')
  print('(4) Calculate Deltas:')
  print('-----------------------------------------------------')
  Delta = AICcs-AICcmin
  print(c(round(Delta,2),'Delta'))
  print('-----------------------------------------------------')
  print('(5) Calculate relative likelihoods:')
  print('-----------------------------------------------------')
  RL = exp(-Delta/2)
  print(c(round(RL,5),'relative likelihoods'))
  print('-----------------------------------------------------')
  print('(6) Calculate probabilities wi')
  print('-----------------------------------------------------')
  sumRL = sum(RL)
  w = RL/sumRL
  print(c(round(w,5),'probabilities wi'))
  print(c(round(w*100,2),'probabilities wi (%)'))
  print('-----------------------------------------------------')
  print('(7) Relative likelihood ratios (example): ')
  print('-----------------------------------------------------')
  print(c(round(RL[j]/RL[2],1),'RL(M3)/RL(M1)'))
  
  
  # ---------------------------------------------------------------------
  # ---------------------------------------------------------------------
  # ---------------------------------------------------------------------
  # [1] "file: MLRn10AICc.R"
  # [1] "Sun Nov 27 17:53:13 2022"
  # [1] "-----------------------------------------------------"
  # [1] "(1) generate artificial data:"
  # [1] "-----------------------------------------------------"
  # [1] "Correlation matrix for predictors:"
  # [,1]    [,2]    [,3]
  # [1,]  1.0000  0.9103 -0.3344
  # [2,]  0.9103  1.0000 -0.3575
  # [3,] -0.3344 -0.3575  1.0000
  # [1] "-----------------------------------------------------"
  # [1] "(2) Models M0, ..., M3"
  # [1] "   Model M0: all 3 predictors (global model)"
  # [1] "-----------------------------------------------------"
  # [1] "2.11"               "intercept model M0"
  # [1] "-1.12"             "1. slope model M0"
  # [1] "1.07"              "2. slope model M0"
  # [1] "-0.35"             "3. slope model M0"
  # [1] "0.54"                              "uncertainty of intercept model M0"
  # [1] "0.52"                             "uncertainty of 1. slope model M0"
  # [1] "0.22"                             "uncertainty of 2. slope model M0"
  # [1] "0.97"                             "uncertainty of 3. slope model M0"
  # [1] "24.6"  "AICM0"
  # [1] "39.6"   "AICcM0"
  # [1] "-----------------------------------------------------"
  # [1] "   Model M1: drop 1. predictor:"
  # [1] "-----------------------------------------------------"
  # [1] "2.09"               "intercept model M1"
  # [1] "0.64"              "x2 slope model M1"
  # [1] "-0.3"              "x3 slope model M1"
  # [1] "0.66"                              "uncertainty of intercept model M1"
  # [1] "0.12"                             "uncertainty of x2 slope model M1"
  # [1] "1.19"                             "uncertainty of x3 slope model M1"
  # [1] "28.3"  "AICM1"
  # [1] "36.3"   "AICcM1"
  # [1] "-----------------------------------------------------"
  # [1] "   Model M2: drop 2. predictor:"
  # [1] "-----------------------------------------------------"
  # [1] "2.44"               "intercept model M2"
  # [1] "1.16"              "x1 slope model M2"
  # [1] "-0.99"             "x3 slope model M2"
  # [1] "1.1"                               "uncertainty of intercept model M2"
  # [1] "0.47"                             "uncertainty of x1 slope model M2"
  # [1] "1.97"                             "uncertainty of x3 slope model M2"
  # [1] "38.5"  "AICM2"
  # [1] "46.5"   "AICcM2"
  # [1] "-----------------------------------------------------"
  # [1] "   Model M3: drop 3. predictor:"
  # [1] "-----------------------------------------------------"
  # [1] "1.93"               "intercept model M3"
  # [1] "-1.12"             "x1 slope model M3"
  # [1] "1.08"              "x2 slope model M3"
  # [1] "0.19"                              "uncertainty of intercept model M3"
  # [1] "0.49"                             "uncertainty of x1 slope model M3"
  # [1] "0.2"                              "uncertainty of x2 slope model M3"
  # [1] "22.8"  "AICM3"
  # [1] "30.8"   "AICcM3"
  # [1] "-----------------------------------------------------"
  # [1] "(3) Find minimum AICc"
  # [1] "-----------------------------------------------------"
  # [1] "Model M"          "3"                "has minimum AICc"
  # [1] "-----------------------------------------------------"
  # [1] "(4) Calculate Deltas:"
  # [1] "-----------------------------------------------------"
  # [1] "8.79"  "5.48"  "15.74" "0"     "Delta"
  # [1] "-----------------------------------------------------"
  # [1] "(5) Calculate relative likelihoods:"
  # [1] "-----------------------------------------------------"
  # [1] "0.01236"              "0.06453"              "0.00038"              "1"                    "relative likelihoods"
  # [1] "-----------------------------------------------------"
  # [1] "(6) Calculate probabilities wi"
  # [1] "-----------------------------------------------------"
  # [1] "0.01147"          "0.0599"           "0.00036"          "0.92827"          "probabilities wi"
  # [1] "1.15"                 "5.99"                 "0.04"                 "92.83"                "probabilities wi (%)"
  # [1] "-----------------------------------------------------"
  # [1] "(7) Relative likelihood ratios (example): "
  # [1] "-----------------------------------------------------"
  # [1] "15.5"          "RL(M3)/RL(M1)"