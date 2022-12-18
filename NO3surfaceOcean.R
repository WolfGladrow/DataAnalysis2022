print('file: NO3surfaceOcean.R')
# read NO3 & surface ocean plot
# install.packages('ncdf4') # install package (apply only once on your computer)
library(ncdf4)
# install.packages('fields') # install package (apply only once on your computer)
library(fields)
info.nc = nc_open('nitrate_annual_1deg.nc')  # open netCDF file
nitrate.lon = ncvar_get( info.nc, 'lon')     # eastern longitude
nitrate.lat = ncvar_get( info.nc, 'lat')     # latitude
nitrate.con = ncvar_get( info.nc, 'n_an')    # NO3 concentrations
B = matrix(nitrate.con[1:360,1:180,1],nrow=360,ncol=180) # surface values
Bcutoff = 40
Bmax = 0
c = 0
for (k in 1:360) { 
  for (n in 1:180) {if (is.na(B[k,n]) == FALSE) {c=c+1;
  if (B[k,n] > Bmax) Bmax = B[k,n]; if (B[k,n] > Bcutoff) B[k,n] = Bcutoff}}}
brk = seq(0,33,1); L = length(brk)-1
# png('NO3surface160120.png',width=16,height=12,units='cm',res=300)
image.plot(nitrate.lon,nitrate.lat,B, breaks=brk, col=rainbow(L), 
           lab.breaks=names(brk),xlab='Eastern longitude',ylab='Latitude',
           main=expression(paste('Surface ',NO[3],' (',mu,'mol ',L^-1,')')),
           cex.lab=1.5,las=1)
# dev.off()