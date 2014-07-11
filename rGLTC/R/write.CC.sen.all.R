

# get all the lat and lon vals:

year = 1985
data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/Cloud_cover_PATMOSX/"
nc	<-	nc_open(filename=paste(data.dir,'CA_PATMOSX_NOAA_0130PM_',as.character(year),'.nc',sep=''))

lat.vals  <<-	ncvar_get(nc,varid="latitude")
lon.vals	<<-	ncvar_get(nc,varid="longitude")

years <- seq(1985,2009)

get.all.CC <-	function(years,period='JAS'){
  vals.out <- rep( list(list()), length(years) )
  names(vals.out) <- paste('yr',years,sep='')
  require(ncdf4)
  if (period=="JAS"){
    mm.idx  <-	c(7,8,9)
  }
  
  
  
  data.dir	<-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/Cloud_cover_PATMOSX/"
  for (i in 1:length(years)){
    year <- years[i]
    nc  <-	nc_open(filename=paste(data.dir,'CA_PATMOSX_NOAA_0130PM_',as.character(year),'.nc',sep=''))
    CA_1	<-	ncvar_get(nc=nc, varid="a_CA",start=c(1,1,mm.idx[1]),count=c(length(lon.vals),length(lat.vals),1)) # hack for mean of JAS
    CA_2  <-	ncvar_get(nc=nc, varid="a_CA",start=c(1,1,mm.idx[2]),count=c(length(lon.vals),length(lat.vals),1))
    CA_3  <-	ncvar_get(nc=nc, varid="a_CA",start=c(1,1,mm.idx[3]),count=c(length(lon.vals),length(lat.vals),1))
    year_CA = (CA_1+CA_2+CA_3)/3
    nc_close(nc)
    vals.out[[i]] <- year_CA
  }
  
  
  return(vals.out)
}

vals.out <- get.all.CC(years)
#source("gap.match.R")
sens <- matrix(data = NA,nrow = length(lon.vals),ncol=length(lat.vals))

for (j in 1:length(lat.vals)){
  for (i in 1:length(lon.vals)){
    vals <- vector(mode = 'numeric',length=length(vals.out))
    for (k in 1:length(vals.out)){
      vals[k] <- vals.out[[k]][i, j] # note i and j switched...
      cat(i);cat(j);cat(k);cat('\n')
    }
    temp.df <- data.frame("Temp"=vals,"date"=as.Date(paste(years,'-1-1',sep='')))
    cat(i);cat(j);cat('\n')
    sens[i,j] <- calc.sen.slope(temp.df)$slope
  }
  cat(j);cat(' of ');cat(length(lat.vals));cat('\n')
}