sampleUcorFct = function(r,n){
  # function generating random samples (x,y) from standard
  #  uniform PDF of size n with prescribed correlation r
  # created by Dieter.Wolf-Gladrow@awi.de (12/2016) based on Howell (2013): 
  # http://www.uvm.edu/~dhowell/StatPages/More_Stuff/CorrGen.html
  x = runif(n); y = runif(n)   # data from standard uniform PDF
  x = (x-mean(x))/sd(x); y = (y-mean(y))/sd(y)  # standardize
  a = r/sqrt(1-r^2); y = a*x+y; 
  # scale x,y -> 0 <= x,y <= 1
  x = (x-min(x))/(max(x)-min(x));
  y = (y-min(y))/(max(y)-min(y))
  return(c(x,y))}