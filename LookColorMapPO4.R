print('file: LookColorMapPO4.R')
# data source: http://www.nodc.noaa.gov/OC5/WOA09/netcdf_data.html
# install.packages('ncdf4') # install package (apply only once on your computer)
library(ncdf4)
# install.packages('fields') # install package (apply only once on your computer)
library(fields)
info.nc = nc_open('phosphate_annual_1deg.nc')  # open netCDF file
phosphate.lon = ncvar_get( info.nc, 'lon')     # eastern longitude
phosphate.lat = ncvar_get( info.nc, 'lat')     # latitude
phosphate.con = ncvar_get( info.nc, 'p_an')    # PO4 concentrations (annual mean)
B = matrix(phosphate.con[1:360,1:180,1],nrow=360,ncol=180) # surface values
Bmax = max(B,na.rm=TRUE)  # 2.2288
brk = seq(0,2.3,0.05); L = length(brk)-1
library(latex2exp)
# png('PO4surface220221.png',width=16,height=12,units='cm',res=300)
image.plot(phosphate.lon,phosphate.lat,B,breaks=brk,col=rainbow(L),
           xlab='Eastern longitude',ylab='Latitude',las=1,
           main=TeX('$\\[ PO_4 \\]\\, (\\mu mol\\, L^{-1})$'))
# dev.off()
# ---------------------------------------------------
# Results: 
print(c('max(PO4,surface) = ',round(Bmax,2),' (mumol/L)')) # 2.23
# ---------------------------------------------------
# Remarks:
# library(ncdf4)   for reading netcdf files
# library(fields)  contains plotting routine image.plot(); use ?image.plot for
#                     explanation of parameters
# col=rainbow(L)   rainbow color map
# ---------------------------------------------------