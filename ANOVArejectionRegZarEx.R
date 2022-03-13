print('file: ANOVArejectionRegZarEx.R')
# plot rejection region for F distribution: red
nu1 = 3; nu2 = 15   # degrees of freedom (for 19 data points in 4 groups)
alpha = 0.05 
Fc = qf(p=1-alpha,df1=nu1,df2=nu2)    # critical F value
print(c('alpha = ',alpha))
print(c('nu1 = ',nu1))
print(c('nu2 = ',nu2))
print(c('Fc = ',round(Fc,3)))
# define polygon & plot
x1max = 6
x1 = seq(Fc,x1max,0.01)
F1 = df(x1,df1=nu1,df2=nu2)
xp = c(Fc,x1,x1max,0)
yp = c(0,F1,0,0)
x0 = seq(0.00001,x1max,0.001); F0 = df(x0,df1=nu1,df2=nu2)
xv = c(Fc,Fc); yv = c(0,0.3)  # vertical line
sflag = 1
if (sflag == 1) {
  # png('Frejectionregion160727red.png',width=16,height=12,units='cm',res=300)
  plot(x0,F0,type='l',lwd=3,col='black',xlab='x',ylab=NA, 
       las=1,xaxs='i',yaxs='i',ylim=c(0,0.75),xlim=c(-0.1,6),cex.lab=1.5)
  title(ylab=expression(paste('F(x, ',nu[1],' = 3',',',nu[2],' = 15)')),line=2.3,cex.lab=1.5)
  polygon(xp,yp,col='red')
  text(4.6,0.3,'rejection region',col='red',cex=1.5)
  text(4.6,0.2,expression(paste('for ',alpha,' = 0.05')),col='red',cex=1.5)
  Fc3 = round(Fc,3)
  text(Fc,0.4,col='red',bquote(~F[c] == .(Fc3)),pos=4,cex=1.5)
  lines(xv,yv,col='red',lty=2)
  #  dev.off()
}
# ----------------------------------------------------------------
# Results:
# "alpha = " "0.05"    
# "nu1 = " "3"     
# "nu2 = " "15"    