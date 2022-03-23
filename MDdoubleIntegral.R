print('file: MDdoubleIntegral.R')
# double integral over f(x,y; alpha, beta)
# f(x,y; \alpha, \beta) = \frac{24}{4\alpha + 3\beta}(\alpha x^2 y + \beta x y^3)
alpha=3;beta=2
f1=function(z,alpha,beta){x=z[1]; y=z[2];
return(24/(4*alpha+3*beta)*(alpha*x^2*y+beta*x*y^3))}
# install.packages('cubature')
library(cubature)
NormIntegral=adaptIntegrate(f1,lowerLimit=c(0,0),upperLimit=c(1,1),alpha,beta)$integral
print(c(NormIntegral,'normalization'))
# mean_x:
f2=function(z,alpha,beta){x=z[1]; y=z[2];
return(x*24/(4*alpha+3*beta)*(alpha*x^2*y+beta*x*y^3))}
muxnum=adaptIntegrate(f2,lowerLimit=c(0,0),upperLimit=c(1,1),alpha,beta)$integral
print(c(round(muxnum,4),'mean_x, numerical'))                       
muxana = (3*alpha+2*beta)/(4*alpha+3*beta); print(c(round(muxana,4),'mean_x, analytical'))
# mean_y:
f3=function(z,alpha,beta){x=z[1]; y=z[2];
return(y*24/(4*alpha+3*beta)*(alpha*x^2*y+beta*x*y^3))}
muynum=adaptIntegrate(f3,lowerLimit=c(0,0),upperLimit=c(1,1),alpha,beta)$integral
print(c(round(muxnum,4),'mean_y, numerical'))               
muyana = 4/15*(10*alpha+9*beta)/(4*alpha+3*beta); print(c(round(muxana,4),'mean_y, analytical'))
# plot f(x,y;alpha,beta):
xa = seq(0,1,0.02); ya = xa; L = length(xa)
f = matrix(data=NA,nrow=L,ncol=L)
for(m in 1:L) {x = xa[m];
for(n in 1:L) {y = ya[n]; f[m,n] = alpha*x^2*y+beta*x*y^3}}
f = f*24/(4*alpha+3*beta)
max(f)
# install.packages('plot3D')
library(plot3D)
# png('jointPDF3D170220.png',width=16,height=16,units='cm',res=300)
persp3D(z=f,main='',clab='PDF',zlab='PDF',breaks = seq(0,7.5,0.5),
        ticktype='detailed',nticks=2,cex.lab=1.5) 
# dev.off()