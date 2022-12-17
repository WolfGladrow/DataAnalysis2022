print('file: GerrodetteLikelihood.R')
print(date())
# Gerrodette11: vaquita abundance, summarized data, likelihood; 3D-plot
print(' ---------------------------------------------------')
print('(1) Frequentistic inference:')
print('(0) Data (summarized):')
N97 = 409; se97 = 250 # abundance & standard error 1997
N08 = 179; se08 = 74  # abundance & standard error 1997
z = (N08-N97)/sqrt(se97^2+se08^2); print(c(round(z,4),'test statistic')) # will be used much later
dN08N97 = N08-N97; print(c(dN08N97,'difference N08-N97'))
sed = sqrt(se97^2+se08^2); print(c(round(sed,1),'standard error of difference'))
print(c(se97^2,'variance estimate 1997'))
print(c(se08^2,'variance estimate 2008'))
# --------------------------------------------------------------------------
LNparams = function(mu,sigma){
  # Calculate parameters alpha, beta for lognormal PDF from
  # mean mu > 0 and standard deviation sigma
  # Dieter.Wolf-Gladrow@awi.de 7/2019 (based on function lnormal.params from bayescount)
  beta = sqrt(log(sigma^2/mu^2+1))
  alpha = log(mu) - ((beta^2)/2)
  return(c(alpha,beta))
}
# --------------------------------------------------------------------------
# (2) Likelihood inference: Gerrodette (2011, Figs.3A and 3B)
par97 = LNparams(N97,se97)
par08 = LNparams(N08,se08)
# --------------------------------------------------------------------------
mflag = 0
if(mflag == 1) { # find location of maximum; takes a few seconds
  print('Find maximum likelihood:')
  darr = seq(-1500,500,1); Ld = length(darr)      # fine resolution for finding the maximum & for
  #   calculating profile likelihhod;
  N97arr = seq(10,1400,1); LN97 = length(N97arr)  #   increment 10 for 3D-plot is much faster
  L = matrix(data=NA,nrow=Ld,ncol=LN97)
for(i in 1:Ld) {
  for(j in 1:LN97) {
    x = darr[i]; y = N97arr[j]; z = x+y
    L[i,j] = dlnorm(y,par97[1],par97[2])*dlnorm(z,par08[1],par08[2])}}
  Lmax = max(L); print(c(Lmax,'Lmax = maximum likelihood'))
  outij=which(L == Lmax, arr.ind=TRUE)
  print(c(darr[outij[1]],'difference at likelihood maximum'))
  print(c(N97arr[outij[2]],'N97 at likelihood maximum'))
}
# --------------------------------------------------------------------------
sflag = 4
if (sflag == 4) {
  # coarse resolution of likelihood for 3D-plot:
  darr3D = seq(-1500,500,10); Ld3D = length(darr3D) # increment 10 for 3D-plot is much faster
  N97arr3D = seq(10,1400,10); LN973D = length(N97arr3D)
  L3D = matrix(data=NA,nrow=Ld3D,ncol=LN973D)
  for(i in 1:Ld3D) {
    for(j in 1:LN973D) {
      x = darr3D[i]; y = N97arr3D[j]; z = x+y
      L3D[i,j] = dlnorm(y,par97[1],par97[2])*dlnorm(z,par08[1],par08[2])}}
  scaleL = 1/max(L3D)
  # install.packages('plot3D')
  library(plot3D)
  # png('Gerrodette11Fig3a200721.png',width=16,height=12,units='cm',res=300)
  persp3D(N97arr3D,darr3D,t(L3D)*scaleL,main='',clab='',theta = 30,phi = 30,box=TRUE,expand=0.5,
          ylab='d',xlab='N97',zlab='Likelihood',ticktype='detailed',nticks=2,cex.lab=1.5)
  # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: GerrodetteLikelihood.R"
# "Sat Dec 17 17:47:37 2022"
# " ---------------------------------------------------"
# "(1) Frequentistic inference:"
# "(0) Data (summarized):"
# "-0.8822"        "test statistic"
# "-230"           "difference N08-N97"
# "260.7"          "standard error of difference"
# "62500"          "variance estimate 1997"
# "5476"           "variance estimate 2008"
# -----------------------------------------------------------------------------
