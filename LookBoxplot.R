print('file: LookBoxplot.R')
# data:
x1 = c(-0.68644, -0.82379, -0.98416, -2.02230, -0.43507, -0.76655,
       1.22178, 0.09767, -0.93391, -1.23458, 0.09188, 0.56736,
       -0.55276, -0.07969, 0.11767, 2.07541, 1.76443, 0.60249,
       -1.29916, -0.30322, -0.77935, -0.97190, 0.84580, 0.28698,
       1.15160, 0.35533, 0.32936, 1.68584, 0.18260, 1.93600)
# install.package('latex2exp')
library(latex2exp)
# png('x1PlotBox220220.png',width=16,height=12,units='cm',res=300)
boxplot(x1,col='blue',las=1,xlab=TeX('$x_1$'))
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# Boxplots were introduced by John Tukey in 1977.
# boxplot()   box-and-whisker-plot
# The thick and long horizontal line inside the box indicates the median (a robust
#   estimate of the central tendency)
# The (blue) box contains 50% of the data; lower boundary of the box = 25% quartile,  
#   upper boundary of the box = 75% quantile 
# The interquartile range (IQR) is a measure of the spread of data. It is defined as 
#   the difference between the 3. and 1. quartile: IQR = Q3 - Q1.
#   This is the height of the (blue) box.
# The upper whisker ends at 1.5 IQR above Q3.
# The lower whisker ends at 1.5 IQR below Q1
# If there exist data beyond the whiskers they are plotted (circles). These data might 
#   be outliers and thus should be inspected in detail.
# Remark: box plots are non-parametric, i.e. no assumption is made about the statistical 
#   distribution from which the data come from.
# ----------------------------------------------------------------