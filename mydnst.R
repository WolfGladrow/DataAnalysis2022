# file: mydnst.R
# purpose: density of non-standardized t PDF
# How to use?
# (1) include in your R script 
# or
# (2) source('mydnst.R') before calling mydnst()
# ----------------------------------------------------
mydnst = function(x, location, scale, df) {
  # density of non-standardized t PDF; DWG 6/2021
  tstat = (x-location)/scale
  return(dt(tstat,df)/scale)
  # factor 1/scale in density stems from d tstat/dx = 1/scale 
}