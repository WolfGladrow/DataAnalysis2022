tstatisticFct = function(x,y) { # file: tstatisticFct.R
  m=length(x) # Albert (2009) p.10 slightly 
  n=length(y) # modified ('var' instead of 'sd^2')
  sp=sqrt(((m-1)*var(x)+(n-1)*var(y))/(m+n-2))
  t.stat=(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
  return(t.stat)
}