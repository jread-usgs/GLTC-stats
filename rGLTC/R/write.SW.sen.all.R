

# get all the lat and lon vals:

year = 1985
library('ncdf4')
library('zyp')
data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/Radiation/"
nc	<-	nc_open(filename=paste(data.dir,'SRB_sw_dn_JAS.nc',sep=''))

lat.vals  <<-	ncvar_get(nc,varid="lat")
lon.vals	<<-	ncvar_get(nc,varid="lon")
time <<- ncvar_get(nc,varid="time")

years <- seq(1985,2009)

get.all.CC <-	function(years,period='JAS',lat.i,lon.i){
  vals.out <- rep( NA, length(years) )
  require(ncdf4)
  if (period=="JAS"){
    mm.idx  <-	c(7,8,9)
  }
  
  
  
  data.dir	<-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/Cloud_cover_PATMOSX/"
  for (i in 1:length(years)){
    year <- years[i]
    nc  <-	nc_open(filename=paste(data.dir,'CA_PATMOSX_NOAA_0130PM_',as.character(year),'.nc',sep=''))
    CA	<-	ncvar_get(nc=nc, varid="a_CA",start=c(lon.i,lat.i,mm.idx[1]),count=c(1,1,length(mm.idx)))
    nc_close(nc)
    vals.out[i] <- mean(CA)
  }
  return(vals.out)
}


#source("gap.match.R")
summaryTxt = paste('/Users/jread/Documents/GLTC-stats/rGLTC/data/SWRad_all_sen.csv',sep='')
cat("lat,lon,slope(SW)\n", file=summaryTxt,append=FALSE)



for (j in 1:length(lat.vals)){
  for (i in 1:length(lon.vals)){

    vals.out <- get.all.CC(years,period='JAS',lat.i=j,lon.i=i)
    y = vals.out
    x = as.numeric(as.Date(paste(years,'-1-1',sep='')))/365.25
    sens <- zyp.sen(y~x)$coefficients[[2]]
    cat(paste(c(lat.vals[j],',',lon.vals[i],',',sens,'\n'),collapse=''), file=summaryTxt,append=TRUE)
  }
  cat(j);cat(' of ');cat(length(lat.vals));cat('done......\n')
}