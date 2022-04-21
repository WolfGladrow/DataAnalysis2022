print('file: EdelweissScatter2.R')
# Scatter plot with different symbols: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissScatterTri171020.png',width=16,height=16,units='cm',res=300)
plot(x,type='p',lwd=4,col='blue',xlab='Data #',ylab='x (m)',las=1,cex=0.6,pch=24,cex.lab=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# pch   parameter for choosing symbols; allowed values: 0 to 25
# pch = 20    bullet
# pch = 21    filled circle
# pch = 22    filled square
# pch = 23    filled diamond
# pch = 24    filled triangle point-up
# pch = 25    filled triangle point down
# ?par        ask for information about (setting) graphical parameters
# ----------------------------------------------------------------