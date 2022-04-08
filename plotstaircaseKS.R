plotstaircaseKS = function(x,xmin,xmax,xH0,yH0) {
# file: plotstaircaseKS.R
# Dieter.Wolf-Gladrow@awi.de 2/2016
# purpose: estimate CDF from data & plot as staircase & plot CDF for H0
# ---------------------------------------------------------------------
# input:
# x      data (assumed to be sorted from small to large; otherwise: x = sort(x))
# xmin   minimum x-value for staircase plot
# xmax   maximum x-value for staircase plot
# xH0    x-values for CDF based on H0
# yH0    y-values for CDF based on H0
# ---------------------------------------------------------------------
# x = sort(x)
if (min(x) < xmin) xmin = min(x); if (max(x) > xmax) xmax = max(x)
N = length(x)              # number of data
yCDFest = seq(1,N)/N       # y-values for CDF estimated from data
xp = numeric(2*N+2); yp = numeric(2*N+2) # arrays for staircase plot
m = 1  # counter
xp[m] = xmin; yp[m] = 0
m = m+1; k = 1; xp[m] = x[k]; yp[m] = 0
for(k in 1:(N-1)) {m=m+1; xp[m] = x[k]; yp[m] = yCDFest[k];
  m=m+1; xp[m] = x[k+1]; yp[m] = yCDFest[k]}
xp[2*N] = x[N]; yp[2*N] = yCDFest[N-1]
xp[2*N+1] = x[N]; yp[2*N+1] = yCDFest[N]
xp[2*N+2] = xmax; yp[2*N+2] = yCDFest[N]
plot(xp,yp,type='l',col='blue',lwd=3,xlab='x',las=1,ylab='',cex.lab=1.5)
title(ylab=expression(paste(paste(CDF[H0](x),', ',CDF[est](x)))),line=2.5,cex.lab=1.5)
lines(xH0,yH0,col='black',lwd=3)
# test statistic D: pedestrian
L = length(yp); Darr = numeric(L)
for(k in 1:L) {q = which.min( abs(xH0-xp[k])^2); 
                              Darr[k] = abs(yH0[q]-yp[k])}
Dp = max(Darr)
return(Dp)
}