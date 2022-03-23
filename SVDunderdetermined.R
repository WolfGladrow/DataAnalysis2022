print('file: SVDunderdetermined.R')
# under-determined linear system: simple example; pedestrian way as alternative to SVD 
r = 2; c = 3   # number of rows & columns
q = c(1,2,2,5,3,8)
A = matrix(data=q,nrow=r,ncol=c)
rhs = c(5,17)
# analytical solutions:
z = seq(2,6,0.0001)
xa = z-9
ya = 7 - 2*z
# least squares solution
sqa = xa^2+ya^2+z^2
k = which.min(sqa)
zk = z[k]
xak = zk-9
yak = 7 - 2*zk
# test solution:
xLS = c(xak,yak,zk)
rhsLS = A%*%xLS
# SVD
svdA = svd(A)  # -> U*D*V^*
U = svdA$u
D = diag(svdA$d) # construct diagonal matrix from singular values
V = svdA$v
x1 = t(U)%*%rhs       # start solving the linear system
DI = diag(1/svdA$d) # construct diagonal matrix from singular values
x2 = DI%*%x1        # continue solving 
xs = V%*%x2         # finalize solving
# png('LeastSquaresUnder180108.png',width=16,height=16,units='cm',res=300)
plot(z,sqa,type='l',lwd=3,col='blue',xlab='z',ylab=NA,las=1,cex.lab=1.5)
title(ylab=expression(paste(x^2,+y^2,+z^2)),line=2.2,cex.lab=1.5)
# dev.off()