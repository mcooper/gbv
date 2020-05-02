library(raster)
library(lubridate)
library(foreach)
library(doParallel)

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

r <- stack('/mnt/air.mon.mean.nc')

ns <- ymd(substr(names(r), 2, 11))

foreach(n=as.character(ns[ns > ymd("1980-12-31") & ns < ymd('2019-12-31')]), .packages=c('raster', 'rgdal', 'lubridate')) %dopar% {
	print(n)
	i <- which(ns == n)
	sel <- r[[i]]
	
	m <- as.matrix(sel)
	m2 <- cbind(m[1:73, 73:144], m[1:73, 1:72])
	
	newsel <- raster(m2, ymn=-91.25, ymx=91.25, xmn=-180, xmx=180, crs=CRS("+proj=longlat +datum=WGS84"))
	names(sel) <- n
	
	writeRaster(newsel, paste0('/mnt/climatedisk/ncar-tave-', n, '.tif'), format='GTiff', overwrite=T)
	writeRaster(newsel, paste0('/home/mattcoop/mortalityblob/temps_ncep_ncar/ncar-tave-', n, '.tif'), format='GTiff', overwrite=T)
}