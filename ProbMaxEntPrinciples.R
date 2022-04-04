print('file: ProbMaxEntPrinciples.R')
# range of applicability of MaxEnt principles
p=seq(0,7,0.01)
x=cos(p); y=sin(p)+0.2*sin(2*p+0.8)
xa=-0.14+0.27*cos(p); ya=-0.5+0.27*sin(p)
f=1.8; 
xb=f*0.3*cos(p); yb=0.1+f*(0.3*sin(p)+0.15*sin(2*p+4)+0.02*sin(3*p+1))
xc=-0.3+0.2*cos(p); yc=-0.4+0.2*sin(p)+0.1*sin(2*p+1);
# png('MEP171128.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',lwd=3,col='blue',xlab='',ylab='',las=1,cex=0.4,
     bty='n',col.axis='white',xaxt='n',yaxt='n')
lines(xa,ya,col='black',lwd=3)
lines(xb,-yb,col='red',lwd=3)
xt = -0.2;
text(xt+0.3,0.75,pos=4,'Principle of',col='blue')
text(xt-0.1,0.6,pos=4,'Maximum Relative Entropy',col='blue')
text(xt+0.1,0.45,pos=4,'(MaxRelEnt)',col='blue')
xt = -0.51
text(xt,0.18,pos=4,'Principle of',col='red');
text(xt,0.03,pos=4,'Maximum Entropy',col='red');
text(xt,-0.12,pos=4,'(MaxEnt)',col='red');
xt = -0.365;
text(xt,-0.42,pos=4,'Principle of',col='black');
text(xt,-0.55,pos=4,'Indifference',col='black');
# dev.off()