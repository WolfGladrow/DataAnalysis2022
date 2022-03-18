print('file: PO4surfaceOcean.R')
# read PO4 & surface ocean plot, short version
info.nc = nc_open('phosphate_annual_1deg.nc')  # open netCDF file
phosphate.lon = ncvar_get( info.nc, 'lon')     # eastern longitude
phosphate.lat = ncvar_get( info.nc, 'lat')     # latitude
phosphate.con = ncvar_get( info.nc, 'p_an')    # PO4 concentrations
B = matrix(phosphate.con[1:360,1:180,1],nrow=360,ncol=180) # surface values
Bcutoff = 4
Bmax = 0
c = 0
for (k in 1:360) { 
  for (n in 1:180) {if (is.na(B[k,n]) == FALSE) {c=c+1;
  if (B[k,n] > Bmax) Bmax = B[k,n]; if (B[k,n] > Bcutoff) B[k,n] = Bcutoff}}}
brk = seq(0,2.3,0.05); L = length(brk)-1
# png('PO4surface160120.png',width=16,height=12,units='cm',res=300)
image.plot(phosphate.lon,phosphate.lat,B, breaks=brk, col=rainbow(L), 
           lab.breaks=names(brk),xlab='Eastern longitude',ylab='Latitude',
           main=expression(paste('Surface ',PO[4],' (',mu,'mol ',L^-1,')')),
           cex.lab=1.5,las=1)
# dev.off()