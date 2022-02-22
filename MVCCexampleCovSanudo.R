print('file: MVCCexampleCovSanudo')
# Sanudo-Wilhelmy et al. (2006)
x = c(0.28,0.37,0.45,0.84,0.76,0.46,0.58,1,0.66,1.08,0.95)  # [PO4] (mumol/L)
y = c(66.8,75.3,70.6,15.2,27.6,27.3,37,45.1,32.2,57.8,24.5) # B12   (pmol/L)
print(c(round(cov(x,y),2),'covariance(x,y)'))
print(c(round(cor(x,y),2),'correlation(x,y)'))
r = cor(x,y); print(c(round(r,4),'r Pearson correlation coefficient'))   # -0.4543169
library(latex2exp)
# png('Sanudo06a220222b.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='p',col='blue',lwd=3,cex=0.4,ylim=c(0,80),las=1,
     xlab=TeX('$\\[ PO_4 \\]\\, (\\mu mol\\, L^{-1})$'),ylab=NA)
title(ylab=TeX('$Vitamin\\, B_{12}\\, (pmol\\, L^{-1})$'),line=2)
text(1,70,paste('r = ',as.character(round(r,2))),col='red')
# dev.off()