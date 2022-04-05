print('file: LagrangeLDieBrute.R')
# Lagrange multiplier for loaded die: 
# (1) Lagrange multipliers for unbiased die:
mu = 3.8    # mean value
L1un = 1 - log(6); L2un = 0
print(c('L1un = ',round(L1un,4)))
print(c('L2un = ',L2un))
print('--------------------------------------------------')
print('(2) Choose coarse grid around (L1un,L2un):')
Lambda1a = seq(-1.2,-0.7,0.0001); L1 = length(Lambda1a) # around -0.7918
Lambda2a = seq(-0.1,0.2,0.0001);  L2 = length(Lambda2a) # around 0
print('--------------------------------------------------')
print('(3) Calculate sum of squares & find minimum:')
ja = seq(1,6)
M = matrix(data=NA,nrow=L1,ncol=L2)
for(i in 1:L1){ Lambda1 = Lambda1a[i];
for(j in 1:L2){ Lambda2 = Lambda2a[j];
M[i,j] = (-1+sum(exp(-1+Lambda1+ja*Lambda2)))^2+
  (-mu+sum(ja*exp(-1+Lambda1+ja*Lambda2)))^2}}
out1 = which(M == min(M), arr.ind = TRUE) # indices of minimum of M
n = out1[1]; m = out1[2]
Mmin1 = min(M); Mmin2 = M[n,m] 
print(c('1. index = n = ',n))
print(c('2. index = m = ',m))
print(c('Mmin1 = ',Mmin1))
print(c('Mmin2 = ',Mmin2))
print('--------------------------------------------------')
print('(4) Lagrange parameters (at minimum): 1. approximation')
L1First = Lambda1a[n]
L2First = Lambda2a[m]
print(c('L1First = ',Lambda1a[n]))
print(c('L2First =  ',Lambda2a[m]))
print('--------------------------------------------------')
print('(5) Remove coarse grid:')
rm(Lambda1a); rm(Lambda2a); rm(M);
print('--------------------------------------------------')
print('(6) Choose fine grid around (L1First,L2First):')
dL = 0.001
Lambda1a = seq(L1First-dL,L1First+dL,0.00001); L1 = length(Lambda1a)
Lambda2a = seq(L2First-dL,L2First+dL,0.00001); L2 = length(Lambda2a)
print('--------------------------------------------------')
print('(7) Calculate sum of squares & find minimum:')
M = matrix(data=NA,nrow=L1,ncol=L2)
for(i in 1:L1){ Lambda1 = Lambda1a[i];
for(j in 1:L2){ Lambda2 = Lambda2a[j];
M[i,j] = (-1+sum(exp(-1+Lambda1+ja*Lambda2)))^2+
  (-mu+sum(ja*exp(-1+Lambda1+ja*Lambda2)))^2}}
out1 = which(M == min(M), arr.ind = TRUE)
n = out1[1]; m = out1[2]
Mmin1 = min(M); Mmin2 = M[n,m] 
print(c('1. index = n = ',n))
print(c('2. index = m = ',m))
print(c('Mmin1 = ',Mmin1))
print(c('Mmin2 = ',Mmin2))
print('--------------------------------------------------')
print('(8) Lagrange parameters (at minimum): 2. approximation')
Lambda1 = Lambda1a[n]
Lambda2 = Lambda2a[m]
print(c('Lambda1 = ',Lambda1a[n]))
print(c('Lambda2 =  ',Lambda2a[m]))
pj = exp(-1+Lambda1+ja*Lambda2)
# png('MaxEntLoadedDie170718.png',width=16,height=12,units='cm',res=300)
plot(ja,pj,type='p',lwd=4,col='blue',ylim=c(0,0.25),xlab='j',ylab='',las=1,cex=0.6,cex.lab=1.5)
title(ylab=expression(paste('MaxEnt probabilities ',p[j])),line=2.7,cex.lab=1.5)
pju = rep(1/6,6)
points(ja,pju,col='black',lwd=4,cex=0.6,pch=24)
xp = c(1,6); yp = c(pj[1],pj[6]); lines(xp,yp,col='green') # Straight line? No!
# dev.off()
print(c('pj = ',round(pj,4)))
print('test of constraints:')
print(c('Constraint 1: sum(pj)    = ',round(sum(pj),4)))
print(c('Constraint 2: sum(ja*pj) = ',round(sum(ja*pj),4)))
# [1] '7 = Lagrange multiplier for loaded die (12/2017; 7/2017)'
# [1] '(1) Lagrange multipliers for unbiased die:'
# [1] 'L1un = ' '-0.7918'
# [1] 'L2un = ' '0'      
# [1] '--------------------------------------------------'
# [1] '(2) Choose coarse grid around (L1un,L2un):'
# [1] '--------------------------------------------------'
# [1] '(3) Calculate sum of squares & find minimum:'
# [1] '1. index = n = ' '301'            
# [1] '2. index = m = ' '2037'           
# [1] 'Mmin1 = '             '2.72457100351743e-09'
# [1] 'Mmin2 = '             '2.72457100351743e-09'
# [1] '--------------------------------------------------'
# [1] '(4) Lagrange parameters (at minimum): 1. approximation'
# [1] 'L1First = ' '-1.17'     
# [1] 'L2First =  ' '0.1036'     
# [1] '--------------------------------------------------'
# [1] '(5) Remove coarse grid:'
# [1] '--------------------------------------------------'
# [1] '(6) Choose fine grid around (L1First,L2First):'
# [1] '--------------------------------------------------'
# [1] '(7) Calculate sum of squares & find minimum:'
# [1] '1. index = n = ' '132'            
# [1] '2. index = m = ' '94'             
# [1] 'Mmin1 = '             '1.83982951524073e-11'
# [1] 'Mmin2 = '             '1.83982951524073e-11'
# [1] '--------------------------------------------------'
# [1] '(8) Lagrange parameters (at minimum): 2. approximation'
# [1] 'Lambda1 = ' '-1.16969'  
# [1] 'Lambda2 =  ' '0.10353'    
# [1] 'pj = '  '0.1267' '0.1405' '0.1558' '0.1728' '0.1917' '0.2126'
# [1] 'test of constraints:'
# [1] 'Constraint 1: sum(pj)    = ' '1'                          
# [1] 'Constraint 2: sum(ja*pj) = ' '3.8' 