CorTestPerm = function(x,y){
  # file: CorTestPerm.R
  # purpose: permutation test on correlation coefficients
  # created by: Dieter.Wolf-Gladrow@awi.de 
  #    based on Greenacre & Primicerio (2013, Chap.6)
  #    5/2020 version 1.0
  # This software is provided 'as is' without warranty of
  # any kind. But it's mine, so you can't sell it.
  M = 1e4
  n = length(x); 
  permcor <- rep(0,M) # generate array of zeros
  permcor[1] <- cor(x,y)
  set.seed(1953)
  for(iperm in 2:M) {
    yperm <- y[sample(n)] # permutation = sample without replacement
    permcor[iperm] <- cor(x,yperm)
  }
  pPermut = sum(abs(permcor)>=abs(permcor[1]))/M # relative frequency
  return(pPermut)}