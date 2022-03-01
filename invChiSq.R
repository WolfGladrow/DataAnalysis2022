invChiSq = function(x,nu) {
  icsq = 2^(-nu/2)/gamma(nu/2)*x^(-nu/2-1)*exp(-1/(2*x)); 
  return(icsq)}
# file: invChiSq.R
# purpose: inverse chi-squared function
# created by: Dieter.Wolf-Gladrow@awi.de 1/2016
# This software is provided 'as is' without warranty of
# any kind. But it's mine, so you can't sell it.
# How to use the function? source('invChiSq.R'); invChiSq(x,nu)
# ---------------------------------------------------------
