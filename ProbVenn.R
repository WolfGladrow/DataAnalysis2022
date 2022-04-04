print('file: ProbVenn.R')
# generalized sum rule: Venn diagram
p=seq(0,7,0.01); x=cos(p); y=sin(p)+0.2*sin(2*p+0.8)
# A:
xb=-0.3+0.3*cos(p)
yb=-0.3+0.3*sin(p)+0.15*sin(2*p+4)+0.02*sin(3*p+1)
# subset C with overlap to B
xc=-0.3+0.2*cos(p)
yc=-0.4+0.2*sin(p)+0.1*sin(2*p+1)
# png('Venn170716.png',width=16,height=16,units='cm',res=300)
plot(xc,yc,type='l',lwd=4,col='magenta',xlab='',ylab='',las=1,cex=0.4,
     xlim=c(-0.6,0),ylim=c(-0.7,0.15),bty='n',col.axis='white',xaxt='n',yaxt='n')
lines(xb,yb,col='blue',lwd=4)
text(-0.3,-0.6,'B',col='magenta',cex=1.5); 
text(-0.4,-0.1,'A',col='blue',cex=1.5)
text(-0.35,-0.4,'A \u2229 B',col='black',cex=1.5)
# dev.off()