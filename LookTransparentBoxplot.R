print('file: LookTransparentBoxplot.R')
# data:
x1 = c(-0.68644, -0.82379, -0.98416, -2.02230, -0.43507, -0.76655,
       1.22178, 0.09767, -0.93391, -1.23458, 0.09188, 0.56736,
       -0.55276, -0.07969, 0.11767, 2.07541, 1.76443, 0.60249,
       -1.29916, -0.30322, -0.77935, -0.97190, 0.84580, 0.28698,
       1.15160, 0.35533, 0.32936, 1.68584, 0.18260, 1.93600)
library(latex2exp)
# png('TransparentBoxplot220220.png',width=16,height=16,units='cm',res=300)
x1[1] = 5 # create an 'outlier'
boxplot(x1,col='yellow',las=1,xlab=TeX('$x_1$'),cex.lab=1.5) 
L = length(x1)
set.seed(1953) # set seed for random number generators
jitter = runif(L,1-0.05,1+0.05)
points(jitter,x1,col='blue',lwd=3,cex=0.4)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# In boxplot(), choose a light color (here: yellow) making the box 'transparent'.
# Add some noise ('jitter', random numbers from a uniform distribution) in the horizontal
#   in order to avoid overlap between equal or similar data points.
# set.seed()   set seed for random number generators -> reproducible (idential) sequence
#                 of pseudo-random numbers; useful when teaching and students obtain identical
#                 results on their own computers
# runif(L,1-0.05,1+0.05)   generate L random numbers from the uniform distribution with
#                             limits 1-0.05 and 1+0.05
# ----------------------------------------------------------------