print('file: MannWhitneyGenFct-3pU.R')
print(date())
# Mann-Whitney test: generating function, n=3=m
n = 3; m = 3
# (1) generating function (NOT USED HERE)
# fx = function(x) { 
#  s1 = 1; s2 = 1; for(i in (m+1):(m+n)) s1=s1*(1-x^i); for(i in 1:n) s2=s2*(1-x^i);
#  return(s1/s2/(factorial(n+m)/factorial(m)/factorial(n)))}
xa = seq(-0.5,0.5,1e-3)
# (2) Generating function as expression:
fex = expression(factorial(3)*factorial(3)/factorial(3+3)*
                    (1-x^4)*(1-x^5)*(1-x^6)/(1-x)/(1-x^2)/(1-x^3))
# (3) Taylor expansion -> probabilities:
x = 0
p = numeric(6)
p[1] = eval(fex,x)  # p(3,3,U=0)
d1 = D(fex,'x'); p[2] = eval(d1,x)
d2 = D(d1,'x'); p[3] = eval(d2,x)/factorial(2)
d3 = D(d2,'x'); p[4] = eval(d3,x)/factorial(3)
d4 = D(d3,'x'); p[5] = eval(d4,x)/factorial(4)
d5 = D(d4,'x'); p[6] = eval(d5,x)/factorial(5)
# d6 = D(d5,'x'); p[7] = eval(d6,x)/factorial(6) takes too long
# d7 = D(d6,'x'); p[8] = eval(d7,x)/factorial(7) takes too long
# d8 = D(d7,'x'); p[9] = eval(d8,x)/factorial(8) takes too long
Ua = seq(0,length(p)-1)
print('(5) Monte Carlo simulations')
set.seed(1953) # set seed for random number generators
M = 1e4  # number of Monte Carlo runs
fr = numeric(10)  # frequencies
for(j in 1:M) {
  x = rnorm(n); y = rnorm(m); # random samples from normal distribution
  xy = c(x,y); rxy = rank(xy); sry = sum(rxy[(n+1):(n+m)]); # srx = sum(rxy[1:n]);
  U = m*n+m*(m+1)/2-sry;
  fr[1+U] = fr[1+U] + 1
}
pMC = fr/M      # relative frequencies
UMC = seq(0,9)
sflag = 3
if (sflag == 3) {
  # png('Mann47Whitneyp33GFMC180528.png',width=16,height=16,units='cm',res=300)
  plot(UMC,pMC,type='p',lwd=4,col='black',xlab='U',ylab='p(U)',las=1,cex=0.6,
       ylim=c(0,0.2),pch=24,cex.lab=1.5)
  points(Ua,p,col='magenta',lwd=4,cex=0.6)
  # dev.off()
}
print(date())
# -----------------------------------------------------------------------------
# Results:
print(c(round(p,3),'p'))
print(c(round(pMC,3),'pMC'))
# "0.05"  "0.05"  "0.1"   "0.15"  "0.15"  "0.15" "p"   
# "0.045" "0.051" "0.097" "0.149" "0.156" "0.151" "0.151" "0.103" "0.051" "0.046" "pMC" 
# -----------------------------------------------------------------------------
# Remarks:
# Calculation of derivatives is not efficient -> long run time
# Calculation of derivatives should by replaced by symbolic manipilation / analytic
#   differentiation.
# Results from differentiation of generating function and from Monte Carlo
#   simulation compare quite well to each other.
# -----------------------------------------------------------------------------




