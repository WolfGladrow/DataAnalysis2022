print('file: LeveneTestZar10d1Ex.R')
print(date())
# Levene test: homogeneity of variances? Zar (2010) Example 10.1 with 4 samples
# apply routine leveneTest() from package car 
# -------------------------------------------------------------------
# Null hypothesis H_0: random samples from (non-Gaussian) populations with
#      equal variances (sig_1^2 = sig_2^2)
# Alternative hypothesis H_A: random samples from (non-Gaussian) populations with
#   different variances (sig_1^2 not equal sig_2^2)
# Note: mean values of populations do not play a role in
#    hypotheses (in general: mu_1 not equal n_2).
# -------------------------------------------------------------------
# (0) data:
# x1 = c(41,35,33,36,40,46,31,37,34,30,38) # number of moths in trap #1
# x2 = c(52,57,62,55,64,57,56,55,60,59)    # number of moths in trap #2
x1 = c(60.8,67.0,65.0,68.6,61.7) 
x2 = c(68.7,67.7,75.0,73.3,71.8)
x3 = c(69.6,77.1,75.2,71.5)
x4 = c(61.9,64.2,63.1,66.7,60.3)
# -------------------------------------------------------------------
print('---------------------------------------------------------------')
print('(1) routine leveneTest() from package car')
print('---------------------------------------------------------------')
library(car)
y = c(x1,x2,x3,x4)
group = as.factor(c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4, length(x4))))
out = leveneTest(y,group)
Fvalue = out$`F value`[1]
p = out$`Pr(>F)`[1]
print(c(round(p,4),'p-value'))
alpha = 0.05   # (-) chosen level of significance
print(c(alpha,'alpha'))
if (p > alpha) print('H_0 not rejected because p > alpha')
if (p <= alpha) print('H_0 rejected because p <= alpha')
# ----------------------------------------------------------------
# Results:
# "file: LeveneTestZar10d1Ex.R"
# "Sat Dec 17 19:46:57 2022"
# "---------------------------------------------------------------"
# "(1) routine leveneTest() from package car"
# "---------------------------------------------------------------"
# "0.7205"  "p-value"
# "0.05"    "alpha"
# "H_0 not rejected because p > alpha"
# ----------------------------------------------------------------
