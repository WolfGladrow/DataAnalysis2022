print('file: SolutionG11NegBinoPL.R')
# EXERCISE & SOLUTION : Gerrodette (2011) with neg. bionom. PD
print(' ---------------------------------------------------')
print('Observed abundances and their uncertainties:')
mu97 = 409;    print(c(mu97,'mu97'))
sigma97 = 250; print(c(sigma97,'sigma97'))
mu08 = 179;    print(c(mu08,'mu08'))
sigma08 = 74;  print(c(sigma08,'sigma08'))
print(' ---------------------------------------------------')
print('Calculate parameters s and p for negative binomial PD:')
s97 = mu97^2/(sigma97^2-mu97); p97 = s97/(mu97+s97)
s08 = mu08^2/(sigma08^2-mu08); p08 = s08/(mu08+s08)
print(c(round(s97,4),'s97')); print(c(round(p97,5),'p97'))
print(c(round(s08,4),'s08')); print(c(round(p08,4),'p08'))
print(' ---------------------------------------------------')
print('Calculate likelihood based on negative binomial PD:')
diff = seq(-1500,500,5)
N97  = seq(20,1600,5)
loglikeNB = array(0,c(length(N97),length(diff))) # matrix
for (i in 1:length(N97))  {  for (j in 1:length(diff))  {
  if (N97[i]+diff[j] > 0) # negative binomial defined for positive values only
    loglikeNB[i,j] = dnbinom(N97[i],s97,p97,log=T) + dnbinom(N97[i]+diff[j],s08,p08,log=T)
  else
    loglikeNB[i,j] = -100 # -> exp(-100) is tiny
}  }
likeNB = exp(loglikeNB)
# print(' ---------------------------------------------------')
print('Calculate and plot profile likelihood (negative binomial):')
profdiffNB = apply(likeNB,2,max)            # profile likelihood of difference
modeLikeNB=diff[which.max(profdiffNB)]                 # mode
meanLikeNB=sum(diff*profdiffNB)/sum(profdiffNB)        # mean
rLikeNB = max(profdiffNB)/profdiffNB[which(diff==0)]  # likelihood ratio
print(c(diff[which.max(profdiffNB)],'mode'))
print(c(sum(diff*profdiffNB)/sum(profdiffNB),'mean'))
print(c(max(profdiffNB)/profdiffNB[which(diff==0)],'ratio'))
PLikeNBm = max(profdiffNB)
PLikeNB0 = profdiffNB[which(diff==0)]
sflag = 1
if (sflag == 1) {
  # png('Gerrodette11ExA190724.png',width=16,height=12,units='cm',res=300)
  plot(diff,profdiffNB,type='l',lwd=3,col='blue',xlab='d',ylab='Profile likelihood',
       las=0,cex.lab=1.5,ylim=c(0,1.2e-5),xaxs='i',yaxs='i')
  abline(v=modeLikeNB,col='red',lty=2)
  abline(v=meanLikeNB,col='black',lty=3)
  text(-1200,max(profdiffNB)*0.9,paste('mode = ',as.character(modeLikeNB)),col='red',
       pos=4,cex=1.5)
  text(-1200,max(profdiffNB)*0.7,paste('mean = ',as.character(round(meanLikeNB,1))),
       col='black',pos=4,cex=1.5)
  text(-1200,max(profdiffNB)*0.5,paste('ratio = ',as.character(round(rLikeNB,2))),
       col='magenta',pos=4,cex=1.5)
  text(50,max(profdiffNB)*1,paste(as.character(round(max(profdiffNB),7))),
       col='magenta',pos=4,cex=1.5)
  text(100,PLikeNB0,paste(as.character(round(PLikeNB0,7))),col='magenta',pos=4,cex=1.5)
  xp1 = c(0,0,150); yp1 = c(0,PLikeNB0,PLikeNB0)
  xp2 = c(modeLikeNB,100); yp2 = c(PLikeNBm,PLikeNBm)
  lines(xp1,yp1,col='magenta')
  lines(xp2,yp2,col='magenta')
  # dev.off()
}