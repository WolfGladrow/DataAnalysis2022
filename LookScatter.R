print('file: LookScatter.R')
x1 = c(-0.68644, -0.82379, -0.98416, -2.02230, -0.43507, -0.76655,
       1.22178, 0.09767, -0.93391, -1.23458, 0.09188, 0.56736,
       -0.55276, -0.07969, 0.11767, 2.07541, 1.76443, 0.60249,
       -1.29916, -0.30322, -0.77935, -0.97190, 0.84580, 0.28698,
       1.15160, 0.35533, 0.32936, 1.68584, 0.18260, 1.93600)
# png('x1PlotScatter220218s.png',width=16,height=12,units='cm',res=300)
plot(x1,type='p',lwd=4,cex=0.6,col='blue',xaxt='n',yaxt='n',xlab='',ylab='') 
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5,las=1)
mtext('Data #',side=1,line=2.4,cex=2)
library(latex2exp)
mtext(TeX('$x_1$'),side=2,line=2.2,cex=2)
# dev.off()
# ------------------------------------------------------------------------------------
# REMARKS: 
# xaxt='n' NO NUMBER ON X-AXIS; NUMBERS WILL BE ADDED USING 'axis()'
# xlab=''  NO LABELS ON X-AXIS; LABELS WILL BE ADDED USING 'mtext()'
# axis(1,cex.axis=1.5) ADD NUMBERS TO THE 1. AXIS (X-AXIS), CHARACTER SIZE 1.5 TIMES DEFAULT
# axis(2,cex.axis=1.5,las=1) ADD NUMBERS TO THE 2. AXIS (Y-AXIS); 'las=1' FOR HORIZONTAL NUMBERS
# mtext('Data #',side=1,line=2.4,cex=2)   ADD LABEL 'Data #' TO 1. AXIS, CHARACTER SIZE 2 TIMES DEFAULT
# mtext(expression(x[1]),side=2,line=2.2,cex=2)   ADD LABEL x with supscript 1 TO 2. AXIS (Y-AXIS)
