print('file: PDsPDFsJointPolyPDF.R')
# joint polynomial PDF
# f(x,y; \alpha, \beta) = \frac{24}{4\alpha + 3\beta}(\alpha x^2 y + \beta x y^3)
# 0 <= x,y, <= 1
alpha = 2; beta = 3
xa = seq(0,1,0.02); ya = xa; L = length(xa)
f = matrix(data=NA,nrow=L,ncol=L)
for(m in 1:L) {x = xa[m];
   for(n in 1:L) {y = ya[n]; f[m,n] = alpha*x^2*y+beta*x*y^3}}
f = f*24/(4*alpha+3*beta)
print(c(round(max(f),2),'max(f)'))
# install.packages('plot3D')
library(plot3D)
# png('jointPDF3D170219a.png',width=16,height=16,units='cm',res=300)
persp3D(z=f,main='',clab='PDF',zlab='PDF',breaks = seq(0,7.5,0.5),
        ticktype='detailed',nticks=2,cex.lab=1.5)  # ,clab = 'PDF'
# dev.off()