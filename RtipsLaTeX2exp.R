print('file: RtipsLaTeX2exp.R')
# Greek letter, subscripts, ... : LaTeX in R
xa = seq(-3,3,0.01); pa = dnorm(xa)
# install.packages(latex2exp) # do only once on your computer
library(latex2exp)
# png('Latex2expExample220323.png',width=16,height=16,units='cm',res=300)
plot(xa,pa,type='l',lwd=3,col='blue',xlab='x',ylab=NA,las=1,cex.lab=1.5)
title(ylab=TeX('$p_{\\mu_1,\\mu_2}(x)$'),line=2.3,cex.lab=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# R uses expression() to add Greek letters etc. to plots. The application of expression() is
#   often tedious or awful and time-consuming. The package 'latex2exp' allows to use the
#   well-known LaTeX commands (with small modifications, especially by doubling backslashes)
#   to formulate desired terms and to 'translate' them into 'R-expressions'. 
# ----------------------------------------------------------------