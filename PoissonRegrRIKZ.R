print('file: PoissonRegrRIKZ.R')
# Poisson regression: calculate species richness from RIKZ data (Zuur et al., 2007)
RIKZ = read.table('RIKZ.txt',header = T)
RIKZ$Richness = rowSums(RIKZ[,2:76] > 0)
# png('Richness160919.png',width=16,height=12,units='cm',res=300)
plot(RIKZ$NAP,RIKZ$Richness,type='p',lwd=4,col='blue',
     xlab='NAP (m)',ylab='Species richness',las=1,cex=0.6,cex.lab=1.5)
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