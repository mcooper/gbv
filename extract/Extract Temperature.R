library(raster)
library(lubridate)
library(foreach)
library(doParallel)

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

r <- stack('berkeleytmps/air.mon.mean.nc')

ns <- ymd(sapply(names(r), parseNames))

foreach(n=as.character(ns[ns > ymd("1981-01-01")]), .packages=c('raster', 'rgdal', 'lubridate')) %dopar% {
	print(n)
	i <- which(ns == n)
	sel <- r[[i]]
	names(sel) <- n
	writeRaster(sel, paste0('/mnt/climatedisk/berkeley-', tvar, '-', n, '.tif'), format='GTiff', overwrite=T)
	writeRaster(sel, paste0('/home/mattcoop/mortalityblob/berkeley_temps/berkeley-', tvar, '-', n, '.tif'), format='GTiff', overwrite=T)
}