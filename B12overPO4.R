print('file: A1-B12overPO4.R')
# B12 over PO4: subscripts, Greek letters
x = c(0.28,0.37,0.45,0.84,0.76,0.46,0.58,1,0.66,1.08,0.95)
y = c(66.8,75.3,70.6,15.2,27.6,27.3,37,45.1,32.2,57.8,24.5)
r = cor(x,y)
# install.packages('latex2exp')
library(latex2exp)
# png('Sanudo06x171020a.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',col='blue',lwd=3,cex=0.4,ylim=c(0,80),las=1,
     xlab=TeX('$\\[PO_4\\]\\,  (\\mu mol\\, L^{-1})$'),ylab=NA,cex.lab=1.5)
title(ylab=TeX('$Vitamin \\, B_{12}\\, (pmol\\, L^{-1})$'),line=2.3,cex.lab=1.5)
text(0.9,70,paste('r = ',as.character(round(r,2))),col='blue',cex=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# cor()   calculates Pearson's correlation coefficient; 
#           here: r < 0, i.e. anticorrelated 
# library(latex2exp)    loading the package 'latex2exp' which allows to write text 
#                         -- especially mathematical expressions -- in LaTeX format;
#                         R provides expression() for writing such text, however,
#                         expression() is not very user-friendly
# install.packages('latex2exp')   the package 'latex2exp' has to be downloaded from
#                         the internet and installed on your computer only once (unless
#                         an update is required); if this is necessary on your computer,
#                         activate the command by removing the hash '#' 
# Within TeX('$$'):
# \\mu    generates the Greek letter mu
# \\,     generates a small gap
# \\[     generates a left square bracket
#
# Set ylab=NA in plot()    NA = not available -> set ylab via title(ylab=...)
# title(ylab=...)   generates the y-label outside plot(); this allows to vary the position
#                      of the y-label relative to the y-axis by setting 'line' (here: line=2.3)
# ----------------------------------------------------------------
