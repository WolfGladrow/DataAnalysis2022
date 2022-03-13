print('file: LeveneTest3.R')
# Levene test: homogeneity of variances? Zar (2010) Example 8.9 p.155-156
# 3 approaches: 
# (1) routine leveneTest() from package car 
# (2) Pedestrian method (following Zar) using sample means   (Zar, 2010, p.155-156)
# (3) Pedestrian method (following Zar) using sample medians (Zar, 2010, p.157)
# -------------------------------------------------------------------
# Null hypothesis H_0: random samples from (non-Gaussian) populations with
#      equal variances (sig_1^2 = sig_2^2)
# Alternative hypothesis H_A: random samples from (non-Gaussian) populations with
#   different variances (sig_1^2 not equal sig_2^2)
# Note: mean values of populations do not play a role in
#    hypotheses (in general: mu_1 not equal n_2).
# -------------------------------------------------------------------
# (0) data:
x1 = c(41,35,33,36,40,46,31,37,34,30,38) # number of moths in trap #1
x2 = c(52,57,62,55,64,57,56,55,60,59)    # number of moths in trap #2
# -------------------------------------------------------------------
sflag = 1
if (sflag == 1) {
print('---------------------------------------------------------------')
print('(1) routine leveneTest() from package car')
print('---------------------------------------------------------------')
library(car)
y <- c(x1,x2)
group <- as.factor(c(rep(1, length(x1)), rep(2, length(x2))))
out = leveneTest(y,group)
Fvalue = out$`F value`[1]
p = out$`Pr(>F)`[1]
print(c(round(p,4),'p-value'))
alpha = 0.05   # (-) chosen level of significance
print(c(alpha,'alpha'))
if (p > alpha) print('H_0 not rejected because p > alpha')
if (p <= alpha) print('H_0 rejected because p <= alpha')
# Levene's Test for Homogeneity of Variance (center = median)
#       Df  F value Pr(>F)
# group  1  0.5438  0.4699
#       19  
}
# -------------------------------------------------------------------
sflag = 2   
if (sflag == 2) { 
print('---------------------------------------------------------------')
print('(2) Pedestrian method (following Zar) using sample means')
print('---------------------------------------------------------------')
n1=length(x1); n2=length(x2); nu1=n1-1; nu2=n2-1; x1mean=mean(x1); x2mean=mean(x2); 
sumx1=sum(x1); print(c(sumx1,'sumx1')) # 401
sumx2=sum(x2); print(c(sumx2,'sumx2')) # 577
x1prime=abs(x1-x1mean); 
x2prime=abs(x2-x2mean);
sumx1prime=sum(x1prime); print(c(round(sumx1prime,2),'sumx1prime'))   # 39.45
sumx2prime=sum(x2prime); print(c(round(sumx2prime,2),'sumx2prime'))   # 28.4
devx1p=sumx1prime/n1; print(c(round(devx1p,2),'devx1p'))   # 3.59
devx2p=sumx2prime/n2; print(c(round(devx2p,2),'devx2p'))   # 2.84
x1pmean=mean(x1prime); 
x2pmean=mean(x2prime);
SS1p = sum((x1prime-x1pmean)^2); print(c(round(SS1p,2),'SS1p'))  # 77.21 (Zar: 77.25)
# (moths^2) sum of squares  77.2126
# Zar (2010) SS1p = 77.25 slight difference due to rounding of X1pmean and X1prime (?)
SS2p = sum((x2prime-x2pmean)^2); print(c(round(SS2p,2),'SS2p'))  # 35.44     
# (moths^2) sum of squares  35.4440}
spsqp = (SS1p+SS2p)/(nu1+nu2); print(c(round(spsqp,2),'spsqp')) # 5.93       
# (moths^2) estimated variance of populations}
sx1x2sqp = sqrt(spsqp/n1+spsqp/n2); print(c(round(sx1x2sqp,2),'sx1x2sqp')) # 1.06
# (moths^2) estimated variance of difference}
tprime = (x1pmean-x2pmean)/sx1x2sqp; print(c(round(tprime,2),'tprime')) # 0.70 (Zar: 0.71)
# (-) test statistic t'
alpha = 0.05   # (-) chosen level of significance
nu = nu1+nu2   # (-) total number of degrees of freedom
tc = qt(1-alpha/2,nu); print(c(round(tc,4),'tc')) # 2.093
# (-) critical value t_c = t_alpha(2),nu (2-tailed) 2.0930
p = 2*pt(-abs(tprime),nu); print(c(round(p,4),'p')) # 0.4913
tabs = abs(tprime);  # mytfct = @(x)tpdf(x,nu);
# p1 = integrate(dt,df=nu,lower=tabs,upper=max(5*tabs,20)) 
# p = p1$value*2; print(c(round(p,2),'p'))
# (-) observed level of significance, 2-tailed   p = 0.4913 (Zar, 2010, gives p = 0.48)}
if (tabs < tc) print('H_0 not rejected because |t| < t_c')
if (tabs >= tc) print('H_0 rejected because |t| >= t_c')
print(c(alpha,'alpha'))
if (p > alpha) print('H_0 not rejected because p > alpha')
if (p <= alpha) print('H_0 rejected because p <= alpha')
# print(c(round(,),''))
}
# -------------------------------------------------------------------
sflag = 3  
if (sflag == 3) { 
print('---------------------------------------------------------------')
print('(3) Pedestrian method (following Zar) using sample medians')
print('---------------------------------------------------------------')
n1=length(x1); n2=length(x2); nu1=n1-1; nu2=n2-1; 
sumx1=sum(x1); print(c(sumx1,'sumx1')) # 401
sumx2=sum(x2); print(c(sumx2,'sumx2')) # 577
x1prime=abs(x1-median(x1)); 
x2prime=abs(x2-median(x2));
sumx1prime=sum(x1prime); print(c(round(sumx1prime,2),'sumx1prime'))   # 39
sumx2prime=sum(x2prime); print(c(round(sumx2prime,2),'sumx2prime'))   # 27
devx1p=sumx1prime/n1; print(c(round(devx1p,2),'devx1p'))   # 3.55
devx2p=sumx2prime/n2; print(c(round(devx2p,2),'devx2p'))   # 2.7
x1pmean=mean(x1prime); 
x2pmean=mean(x2prime);
SS1p = sum((x1prime-x1pmean)^2); print(c(round(SS1p,2),'SS1p'))  # 82.73
# (moths^2) sum of squares  
SS2p = sum((x2prime-x2pmean)^2); print(c(round(SS2p,2),'SS2p'))  # 48.1     
# (moths^2) sum of squares  
spsqp = (SS1p+SS2p)/(nu1+nu2); print(c(round(spsqp,2),'spsqp')) # 6.89       
# (moths^2) estimated variance of populations}
sx1x2sqp = sqrt(spsqp/n1+spsqp/n2); print(c(round(sx1x2sqp,2),'sx1x2sqp')) # 1.15
# (moths^2) estimated variance of difference}
tprime = (x1pmean-x2pmean)/sx1x2sqp; print(c(round(tprime,2),'tprime')) # 0.74 
# (-) test statistic t'
alpha = 0.05   # (-) chosen level of significance
nu = nu1+nu2   # (-) total number of degrees of freedom
tc = qt(1-alpha/2,nu); print(c(round(tc,4),'tc')) # 2.093
# (-) critical value t_c = t_alpha(2),nu (2-tailed) 2.0930
p = 2*pt(-abs(tprime),nu); print(c(round(p,4),'p')) # 0.4699 identical to levene.Test from car!!!!
tabs = abs(tprime);  # mytfct = @(x)tpdf(x,nu);
if (tabs < tc) print('H_0 not rejected because |t| < t_c')
if (tabs >= tc) print('H_0 rejected because |t| >= t_c')
print(c(alpha,'alpha'))
if (p > alpha) print('H_0 not rejected because p > alpha')
if (p <= alpha) print('H_0 rejected because p <= alpha')
}
# ----------------------------------------------------------------
# Remarks:
# The two pedestrian methods based on sample means (2) or sample medians (3) yielded
#   slightly different p-values.
# The routine leveneTest() and the pedestrian method based an sample medians yielded
#   identical results for the p-value (at least up to 4 digits) -> most probably
#   leveneTest() is based on sample medians.
# leveneTest() works fine, however, the input to leveneTest() is a bit akward 
#   (not just leveneTest(x1,x2)) and not well explained. One has to first concatenate
#   the two samples (c(x1,x2)) and then to generate a vector that gives different
#   factors to elements of x1 and x2: 
#   as.factor(c(rep(1, length(x1)), rep(2, length(x2)))) ->
#    1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
#    Levels: 1 2
# Although the test statistic of the Levene test follows a t-distribution, it is
#    called F in the output of leveneTest().
# In order to obtain the p-value from the output of leveneTest() one has to use 
#    
# ----------------------------------------------------------------