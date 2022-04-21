print('file: EdelweissLine.R')
# Line plot: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissLine171020.png',width=16,height=16,units='cm',res=300)
plot(x,type='l',lwd=4,col='blue',xlab='Data #',ylab='x (m)',las=1,cex.lab=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# Line instead of points: change type parameter in plot() from 'p' (points) to 'l' (line)
# ----------------------------------------------------------------