print('file: TriangAsymPDF.R')
# nonsymmetric triangular PDF
nonsymtri = function(x,thetaMode) {
  # nonsymmetric triangular PDF
  # 0 < x, thetaMode < 1
  # Dieter.Wolf-Gladrow@awi.de 6/2021
  q = 2*x/thetaMode
  L = length(x)
  for(i in 1:L) if(x[i] > thetaMode) q[i] = 2*(1-x[i])/(1-thetaMode)
  return(q)
}
thetaMode = 0.1 # true mode
dx = 0.001; x = seq(dx,1-dx,dx); y = nonsymtri(x,thetaMode)
# png('TriPDFnonsym210614.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',lwd=4,col='blue',xlab='x',ylab='PDF',las=1,cex.lab=1.5)
abline(v=thetaMode,col='black',lty=2)
text(0.15,0.1,paste('mode = ',as.character(thetaMode,3)),col='black',pos=4,cex=1.5)
# dev.off()