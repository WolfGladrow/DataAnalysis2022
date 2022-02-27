# file: myrnst.R
# purpose: random numbers from non-standardized t PDF
# How to use?
# (1) include in your R script 
# or
# (2) source('myrnst.R') before calling myrnst()
# ----------------------------------------------------
myrnst = function(M,location,scale,df) {
  # random numbers from non-standardized t PDF; DWG 6/2021
  tstat = rt(M,df)
  return(tstat*scale+location)
}