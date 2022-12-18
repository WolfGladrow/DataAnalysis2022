print('file: MaxEnt3kinEnergy.R')
print(date())
# particles with 3 kinetic energies
Emean = 4
myFct1 = function(z){1+z^3+z^8-(1+4*z^3+9*z^8)/Emean}
zlower=0.8; zupper=1
out1 = uniroot(myFct1,lower=zlower,upper=zupper,extendInt='downX')
z1 = out1$root
j = seq(1,3)
lambda1 = log(z1)
lambda0 = 1+log(1/sum(exp(lambda1*j^2)))
pj = exp(-1+lambda0+lambda1*j^2)
# test constraints:
C1 = sum(pj)
C2 = sum(j^2*pj)
# ---------------------------------------------------
# Results: 
print(c('z = ',round(z1,4)))
print(c('Lambda0 = ',round(lambda0,4)))
print(c('Lambda1 = ',round(lambda1,4)))
print(c('pj = ',round(pj,4)))
print(c('C1: normalization = ',round(C1,4)))
print(c('C2: mean energy   = ',round(C2,4)))
# "file: MaxEnt3kinEnergy.R"
# "Sun Dec 18 08:57:00 2022"
# "z = "   "0.9381"
# "Lambda0 = " "0.1778"    
# "Lambda1 = " "-0.0639"   
# "pj = "  "0.4123" "0.3404" "0.2473"
# "C1: normalization = " "1"                   
# "C2: mean energy   = " "3.9999" 
# ---------------------------------------------------