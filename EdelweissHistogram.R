print('file: A1-EdelweissHistogram.R')
# Histogram: Edelweiss data (10/2017)')
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissHistogram171020b.png',width=16,height=16,units='cm',res=300)
hist(x,col='blue',xlab='x (m)',main='',las=1,cex.lab=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# hist()   histogram; sort data into bins; number and range of bins can be changed by
#             using the parameter 'breaks' (try yourself!)
# main=''  suppresses a title
# ----------------------------------------------------------------