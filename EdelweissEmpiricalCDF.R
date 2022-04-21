print('file: A1-EdelweissEmpiricalCDF.R')
# Empirical cumulative distribution function: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissECDF171020.png',width=16,height=16,units='cm',res=300)
plot(ecdf(x),col='blue',xlab='x (m)',las=1,pch=20,cex.lab=1.5,main='',
     ylab='Empirical CDF')
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# ecdf()   Compute an empirical cumulative distribution function ... (will be discussed later)
# ----------------------------------------------------------------