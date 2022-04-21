print('file: A1-EdelweissTransparentBoxplot.R')
# transparent boxplot: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissTransparentBoxplot220306.png',width=16,height=16,units='cm',res=300)
boxplot(x,col='yellow',las=1,xlab='x')
L = length(x)
set.seed(1953) # set seed for random number generators
jitter = runif(L,1-0.05,1+0.05)
points(jitter,x,col='blue',lwd=4,cex=0.6)
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