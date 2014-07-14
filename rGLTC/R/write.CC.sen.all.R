

# get all the lat and lon vals:

year = 1985
data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/Cloud_cover_PATMOSX/"
nc	<-	nc_open(filename=paste(data.dir,'CA_PATMOSX_NOAA_0130PM_',as.character(year),'.nc',sep=''))

lat.vals  <<-	ncvar_get(nc,varid="latitude")
lon.vals	<<-	ncvar_get(nc,varid="longitude")

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
summaryTxt = paste('/Users/jread/Documents/GLTC-stats/rGLTC/data/CC_all_sen.csv',sep='')
cat("lat,lon,slope(CC)\n", file=summaryTxt,append=FALSE)



for (j in 1:length(lat.vals)){
  for (i in 1:length(lon.vals)){

    vals.out <- get.all.CC(years,period='JAS',lat.i=j,lon.i=i)
    temp.df <- data.frame("Temp"=vals.out,"date"=as.Date(paste(years,'-1-1',sep='')))
    sens <- calc.sen.slope(temp.df)$slope
    cat(paste(c(lat.vals[j],',',lon.vals[i],',',sens,'\n'),collapse=''), file=summaryTxt,append=TRUE)
  }
  cat(j);cat(' of ');cat(length(lat.vals));cat('done......\n')
}