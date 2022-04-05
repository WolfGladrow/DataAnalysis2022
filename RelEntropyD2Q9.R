print('file: RelEntropyD2Q9.R')
# D2Q9 equilibrium distributions
# assume rho = rho_0 = 1 and m/(kB*T) = 1
rho0 = 1; rho = rho0
f = seq(0,0.3,0.01) # scale factor for velocity (jx,jy)
jx = f; jy = 0.7*f # not parallel to one of the lattice velocities
W0 = 4/9; W1 = 1/9; W2 = 1/36
F0 = W0/rho0*(rho0-(jx*jx+jy*jy)/2)
F1 = W1/rho0*(rho+jx+(jx*jx-(jx*jx+jy*jy))/2)
F2 = W1/rho0*(rho+jy+(jy*jy-(jx*jx+jy*jy))/2)
F3 = W1/rho0*(rho-jx+(jx*jx-(jx*jx+jy*jy))/2)
F4 = W1/rho0*(rho-jy+(jy*jy-(jx*jx+jy*jy))/2)
F5 = W2/rho0*(rho+jx+jy+((jx+jy)^2-(jx*jx+jy*jy))/2)
F6 = W2/rho0*(rho-jx+jy+((-jx+jy)^2-(jx*jx+jy*jy))/2)
F7 = W2/rho0*(rho-jx-jy+((-jx-jy)^2-(jx*jx+jy*jy))/2)
F8 = W2/rho0*(rho+jx-jy+((jx-jy)^2-(jx*jx+jy*jy))/2)
library(latex2exp)
# png('D2Q9Fj171214.png',width=16,height=16,units='cm',res=300)
plot(jx,F0,type='l',lwd=3,col='magenta',xlab=TeX('$j_x$'),
     ylab=NA,las=1,ylim=c(0,0.5),cex.lab=1.5)
title(ylab=TeX('$F_j$'),line=2.3,cex.lab=1.5)
lines(jx,F1,col='blue',lwd=3,lty=1)
lines(jx,F2,col='blue',lwd=3,lty=2)
lines(jx,F3,col='blue',lwd=3,lty=3)
lines(jx,F4,col='blue',lwd=3,lty=4)
lines(jx,F5,col='black',lwd=3,lty=1)
lines(jx,F6,col='black',lwd=3,lty=2)
lines(jx,F7,col='black',lwd=3,lty=3)
lines(jx,F8,col='black',lwd=3,lty=4)
# dev.off()