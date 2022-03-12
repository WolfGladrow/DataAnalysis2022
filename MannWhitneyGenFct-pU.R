print('file: MannWhitneyGenFct-pU.R')
# Mann-Whitney test: generating function, n=3=m
n = 3; m = 3
fx = function(x) { # (1) generating function
  s1 = 1; s2 = 1; for(i in (m+1):(m+n)) s1=s1*(1-x^i); for(i in 1:n) s2=s2*(1-x^i);
  return(s1/s2/(factorial(n+m)/factorial(m)/factorial(n)))}
xa = seq(-0.5,0.5,1e-3)
# (2) Generating function as expression:
fex = expression(factorial(3)*factorial(3)/factorial(3+3)*
                   (1-x^4)*(1-x^5)*(1-x^6)/(1-x)/(1-x^2)/(1-x^3))
# (3) Taylor expansion -> probabilities:
x = 0
p = numeric(8)
p[1] = eval(fex,x)  # p(3,3,U=0)
d1 = D(fex,'x')
d2 = D(d1,'x'); d3 = D(d2,'x'); d4 = D(d3,'x'); d5 = D(d4,'x'); d6 = D(d5,'x'); d7 = D(d6,'x');
# d8 = D(d7,'x') takes too long
p[2] = eval(d1,x) 
p[3] = eval(d2,x)/factorial(2)
p[4] = eval(d3,x)/factorial(3)
p[5] = eval(d4,x)/factorial(4)
p[6] = eval(d5,x)/factorial(5)
p[7] = eval(d6,x)/factorial(6)
p[8] = eval(d7,x)/factorial(7)
# p[9] = eval(d8,x)/factorial(8)
Ua = seq(0,length(p)-1)
# (4) Cumulative density function (CDF)
Ucdf = c(0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8)
pc = numeric(length(p))
pc[1] = p[1]; 
for(j in 2:length(p)) pc[j] = pc[j-1]+p[j];
pCDF = c(0,pc[1],pc[1],pc[2],pc[2],pc[3],pc[3],pc[4],pc[4],pc[5],pc[5],pc[6],pc[6],
         pc[7],pc[7],pc[8],pc[8])
sflag = 2
if (sflag == 2) {
  # png('Mann47Whitneyp33GF180528.png',width=16,height=12,units='cm',res=300)
  plot(Ucdf,pCDF,type='l',lwd=4,col='blue',xlab='U',ylab='p(U)',las=1,cex=0.6,
       ylim=c(0,1),cex.lab=1.5)
  points(Ua,p,col='red',lwd=4,cex=0.6)
  # dev.off()
}
