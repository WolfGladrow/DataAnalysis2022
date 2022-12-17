print('file: LookBubblePlots.R')
# Data source: Borcard et al. (2011)
x = c(88,94,102,100,106,112,114,110,136,168,186,205,222,228,252,266,245,225,
      206,189,187,192,192,179,145,91,65,49,27,8)
y = c(7,14,18,28,39,51,61,76,100,112,130,145,167,182,190,209,203,200,
      194,193,201,212,228,233,217,187,174,164,151,133)
NO3g = c(0.20,0.20,0.22,0.21,0.52,0.15,0.15,0.41,0.82,0.75,1.60,0.50,0.52,1.23, 
         1.00,2.00,2.50,2.20,2.20,3.00,2.20,1.62,3.50,2.50,6.20,3.00,3.00,4.00,1.62,1.60)
# convert from mg/L to mumol/L: HNO3 = 1 + 14 + 3*16 = 63 g/mol
NO3 = NO3g*1000/63 # (micro-mol/L) = (mmol/m^3)
library(latex2exp)
# png('BubblePlotRadiusAndArea220220.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(1,2))
plot(x,y,main='',pch=21,col='white',bg='brown',
     cex=5*NO3/max(NO3),xlab='x (km)',ylab='y (km)',las=1,
     xlim=c(0,300),ylim=c(0,300),cex.lab=1.5)
lines(x,y,col='blue')
text(0,280,TeX('$r = NO_3\\, (\\mu mol/L)$'),col='brown',pos=4,cex=1.3)
plot(x,y,main='',pch=21,col='white',bg='brown',
     cex=5*sqrt(NO3/max(NO3)),xlab='x (km)',ylab='y (km)',las=1,
     xlim=c(0,300),ylim=c(0,300),cex.lab=1.5)
lines(x,y,col='blue')
text(0,280,TeX('$r = \\sqrt{NO_3\\, (\\mu mol/L)}$'),col='brown',pos=4,cex=1.3)
# dev.off()
# -----------------------------------------------------------------------------
# Remarks:
# Several panels in a single plot: setting the parameter mfrow to c(nr,nc) as, 
#   for example, by the command par(mfrow=c(1,2)) -> 1 row with two panels
#   (two columns) 
#   par(mfrow=c(2,1)) would produce two panels ontop of each other
#   par(mfrow=c(2,2)) would produce two times two panels
# In the left panel (1. plot) the symbol radius varies linearly with the 
#   nitrate concentration: cex=5*NO3/max(NO3).
# In the right panel (2. plot) the symbol area varies linearly with the 
#   nitrate concentration: cex=5*sqrt(NO3/max(NO3)).
# -----------------------------------------------------------------------------