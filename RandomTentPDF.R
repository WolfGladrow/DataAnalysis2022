# print('file: RandomTentPDF.R')
# print('plot tent PDF')
dy = 0.01; y1 = seq(0,0.5,dy); y2 = seq(0.5,1,dy)
q1 = 4*y1; q2 = 4*(1-y2)
# png('tentPDF160722.png',width=16,height=16,units='cm',res=300)
plot(y1,q1,type='l',lwd=3,col='black',xlab='y',ylab='Density',las=1,xaxs='i',yaxs='i',
     xlim=c(0,1),ylim=c(0,2.02),cex.lab=1.5)
lines(y2,q2,lwd=3,col='magenta')
# dev.off()