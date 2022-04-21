print('file: A1-EdelweissBoxplot.R')
# Box plot: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissBoxplot171020.png',width=16,height=16,units='cm',res=300)
boxplot(x,col='blue',xlab='',ylab='x (m)',las=1,cex.lab=1.5)
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