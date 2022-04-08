print('file: EdelweissKS.R')
# Edelweiss (observed near S-charl by Paul: KS test
x = c(1853, 1872, 1899, 1949, 1976, 1981, 2001, 2027, 2033, 2044, 2111, 2166, 2245)
L = length(x); ka = seq(1,L)
H0min = 1810; H0max = 2270 # H0: sample from uniform PDF with min = 1810, max = 2270
CDFunif = (x-H0min)/(H0max-H0min)    # uniform CDF at observed heights x_i, i=1,2,...,L
f = numeric(L)+1
z = 1/(H0max-H0min)
xp = c(1750,H0min,H0min,H0max,H0max,2330); 
yp = c(0,0,z,z,0,0)
out = ks.test(x,'punif',min=1810,max=2270)
D = out$statistic; print(c(round(D,4),'D test statistic'))
p = out$p.value; print(c(round(p,4),'p-value'))
Dr = c(round(D,2)); pr = c(round(p,2)) # rounded values for plot
sflag = 7
if (sflag == 1) {
  # png('EdelweissData160915.png',width=16,height=16,units='cm',res=300)
  plot(x,f,type='p',col='blue',ylab='Frequency',xlab='Height (m)',
       las=1,lwd=4,cex=0.6,cex.lab=1.5,ylim=c(0,2))
  # dev.off()
}  
if (sflag == 2) {
  # png('EdelweissCDFest160915.png',width=16,height=16,units='cm',res=300)
  xH0 = xp
  yH0 = punif(xp,min=H0min,max=H0max)
  plot(xH0,yH0,type='l',lwd=4,col='black',xlab='x',ylab='CDF',las=1,cex.lab=1.5)
  lines(ecdf(sort(x)),col='blue',lwd=4)
  text(2100,0.3,bquote(~D == .(Dr)),col='blue',cex=1.5,pos=4)
  text(2100,0.1,bquote(~p == .(pr)),col='blue',cex=1.5,pos=4)
  # dev.off()
}
if (sflag == 7) {
  # Plot PDF of KS-distribution
  dx = 0.001; dx2i = 1/(2*dx)
  xPDF = seq(dx,0.999,dx); LPDF = length(xPDF); yPDF = numeric(LPDF)
  for (k in 2:(LPDF-1)) yPDF[k] = (pkolm(xPDF[k+1],L)-pkolm(xPDF[k-1],L))*dx2i
  # png('EdelweissPDF160915.png',width=16,height=16,units='cm',res=300)
  plot(xPDF,yPDF,type='l',col='black',lwd=3,xlab='D',ylab='KS-PDF (D)',las=1,cex.lab=1.5)
  # add colored polygon with observed level of evidence in red:
  xx = seq(D,1,dx); Lxx = length(xx); yy = numeric(Lxx);
  for(k in 2:(Lxx-1)) yy[k] = (pkolm(xx[k+1],L)-pkolm(xx[k-1],L))*dx2i
  yy[1] = yy[2]; xx1 = c(D,xx,1); yy1 = c(0,yy,0)
  polygon(xx1, yy1, col='red')
  text(0.42,4,paste('p = ',as.character(round(p,2))),col='red',cex=1.5)
  # dev.off()
}
