print('file: RtipsLaTeX2exp.R')
# Greek letter, subscripts, ... : LaTeX in R
gamma1 = 0.38; q = pi; qr = round(pi,3)
xa = seq(-3,3,0.01); L = length(xa)
pa = dnorm(xa)
# install.packages(latex2exp) # do only once on your computer
library(latex2exp)
# png('Latex2expExample220323.png',width=16,height=16,units='cm',res=300)
plot(xa,pa,type='l',lwd=3,col='blue',xlab='x',ylab=NA,las=1,cex.lab=1.5)
title(ylab=TeX('$p_{\\mu_1,\\mu_2}(x)$'),line=2.3,cex.lab=1.5)
text(-3,0.36,bquote(~gamma[1] == .(gamma1) %+-% .(qr)),col='blue',cex=1.5,pos=4)
text(-3,0.33,paste('q =',as.character(round(q,3))),col='black',cex=1.5,pos=4)
text(-3,0.30,expression(mu[2]),col='magenta',cex=1.5,pos=4)
text(-3,0.27,TeX('$p_{\\mu_1,\\mu_2}(x)$'),col='red',cex=1.5,pos=4)
text(0,0.06,TeX('$f(x; \\mu, \\sigma) = \\frac{1}{\\sqrt{2\\pi \\sigma^2}}\\, exp(-\\frac{(x-\\mu)^2}{2 \\sigma^2})$'),
     col='blue',cex=1.5,pos=1)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# R uses expression() to add Greek letters etc. to plots. The application of expression() is
#   often tedious or awful and time-consuming. The package 'latex2exp' allows to use the
#   well-known LaTeX commands (with small modifications, especially by doubling backslashes)
#   to formulate desired terms and to 'translate' them into 'R-expressions'.
#   bquote() allows to combine Greek letters and values.
# ----------------------------------------------------------------