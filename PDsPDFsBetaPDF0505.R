print('file: PDsPDFsBetaPDF0505.R')
# beta PDF
alpha = 0.5; beta = 0.5
xarr = seq(0.001,0.999,0.001)
betaPDF = dbeta(x=xarr,shape1=alpha,shape2=beta)
# png('beta0505PDF191017.png',width=16,height=16,units='cm',res=300)
plot(xarr,betaPDF,type='l',lwd=3,col='blue',xlab='x',
     ylab='Density',las=1,ylim=c(0,max(betaPDF)),cex.lab=1.5)
text(0.5,8,bquote(~alpha == .(alpha)),col='blue',cex=1.5)
text(0.5,6,bquote(~beta == .(beta)),col='blue',cex=1.5)
# dev.off()