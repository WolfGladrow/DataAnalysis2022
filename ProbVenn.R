print('file: ProbVenn.R')
print('1 = Venn and generalized sum rule (2/2022, 7/2016; MATLAB 9/04)')
p=seq(0,7,0.01); x=cos(p); y=sin(p)+0.2*sin(2*p+0.8)
# A:
xb=-0.3+0.3*cos(p)
yb=-0.3+0.3*sin(p)+0.15*sin(2*p+4)+0.02*sin(3*p+1)
# subset C with overlap to B
xc=-0.3+0.2*cos(p)
yc=-0.4+0.2*sin(p)+0.1*sin(2*p+1)
# png('Venn170716.png',width=16,height=12,units='cm',res=300)
plot(xc,yc,type='l',lwd=3,col='red',xlab='',ylab='',las=1,cex=0.4,
     xlim=c(-0.6,0),ylim=c(-0.7,0.15),bty='n',col.axis='white',xaxt='n',yaxt='n')
lines(xb,yb,col='blue',lwd=3)
text(-0.3,-0.6,'B',col='red'); text(-0.4,-0.1,'A',col='blue')
text(-0.35,-0.4,'A \u2229 B',col='black')
# dev.off()