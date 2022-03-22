print('file: PoissonRegrRIKZplus.R')
# Poisson regression: calculate species richness from RIKZ data (Zuur et al., 2007)
RIKZ = read.table('RIKZ.txt',header = T)
RIKZ$Richness = rowSums(RIKZ[,2:76] > 0)
# ---------------------------------------------------------------------------------
prmodel1 = glm(Richness ~ NAP,data = RIKZ,family=poisson)
summary(prmodel1)
b0 = prmodel1$coefficients[1]
b  = prmodel1$coefficients[2]
q = summary(prmodel1)
ub0 = q$coefficients[3]   # uncertainty (1 sigma)
ub  = q$coefficients[4]
# Poisson regression model: log(mu) = beta0 + beta*NAP
sNAP = sort(RIKZ$NAP)  # sort NAP values for plot of regression line
Ypredict = exp(b0 + b*sNAP)
# png('RichPoissonRegrPlus160919.png',width=16,height=16,units='cm',res=300)
plot(RIKZ$NAP,RIKZ$Richness,type='p',lwd=4,col='black',
     xlab='NAP (m)',ylab='Species richness',las=1,cex=0.6,cex.lab=1.5)
lines(sNAP,Ypredict,col='blue',lwd=3)
b0r = round(b0,2); ub0r = round(ub0,2); br = round(b,2); ubr = round(ub,2)
text(0.7,20,bquote(~hat(beta)[0] == .(b0r) %+-% .(ub0r)),col='blue',pos=4,cex=1.5)
text(0.7,17,bquote(~hat(beta) == .(br) %+-% .(ubr)),col='blue',pos=4,cex=1.5)
yp = seq(0,22); 
NAP1 = -0.5; lambda1 = exp(b0+NAP1*b); p1 = dpois(yp,lambda1)
xp01 = c(NAP1,NAP1); yp01 = c(0,22)
lines(xp01,yp01,col='black',lty=2)
points(p1+NAP1,yp,col='magenta',cex=0.4,lwd=2)
yp2 = seq(0,10);
NAP2 = 1.5; lambda2 = exp(b0+NAP2*b); p2 = dpois(yp2,lambda2)
xp02 = c(NAP2,NAP2); yp02 = c(0,10)
lines(xp02,yp02,col='black',lty=2)
points(p2+NAP2,yp2,col='magenta',cex=0.4,lwd=2)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# The calculation of the species richness (the number of different species) in 1 line
#   is quite compact and a bit of explanation is in order. RIKZ is a 45 times 89 data
#   matrix. In columns 2 to 76 it contains the number of observed individuals of 75 different
#   species in 45 samples. RIKZ[,2:76] > 0 is TRUE when at least on individual
#   of a species is observed; try, for example, RIKZ[1,2:76] > 0 for the first row.
#   rowSums() sums the number of TRUE cases for all rows; try, for example, 
#   sum(RIKZ[1,2:76] > 0) for the first row.
#   rowSums(RIKZ[,2:76] > 0) gives a column of 45 richness values. This column is added
#   to the RIKZ matrix by RIKZ$Richness and thus RIKZ is now a 45 times 90 matrix.
# View(RIKZ) displays the RIKZ matrix (note: View starts with capital V)
# ----------------------------------------------------------------
# RIKZ[1,2:76] > 0
# sum(RIKZ[1,2:76] > 0)
# View(RIKZ)