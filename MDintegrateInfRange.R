print('file: MDintegrateInfRange.R')
# numerical integration over an infinite range
integrand2 = function(x) exp(-x)
I2 = integrate(integrand2,0,Inf)$value; print(c(I2,'I2, numerical'))
I2ana = -exp(-Inf)+exp(0);              print(c(I2ana,'I2analytical'))
