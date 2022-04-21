print('file: A1-EdelweissLineScatter.R')
# Line & scatter plot: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissLineScatter171020.png',width=16,height=16,units='cm',res=300)
plot(x,type='l',lwd=1,col='black',xlab='Data #',ylab='x (m)',las=1,cex.lab=1.5)
points(x,col='blue',lwd=4,pch=24,cex=0.6)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# points()   plots points; it has to be called after plot()
# Alternative (if you want the line in the foreground):
# plot(x,type='p',lwd=3,col='blue',xlab='Data #',ylab='x (m)',las=1,cex=0.6,pch=24,cex.lab=1.5)
# lines(x,col='black',lwd=1)
# ----------------------------------------------------------------