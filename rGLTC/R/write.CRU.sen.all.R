
getCRU  <-	function(period='JAS',use.years = seq(1985,2009),lat.i=NULL,lon.i=NULL){
  require(ncdf4)
  if (period=="JAS"){
    mm.idx  <-	c(7,8,9)
  } else if (period=='JFM'){
    mm.idx <- c(1,2,3)
  }	else if (period=='annual'){
    mm.idx <- seq(1,12)
  }
  
  
  
  vals = get.vals(years='1981.1990',lat.i,lon.i)
  
  CA = vals
  
  vals = get.vals(years='1991.2000',lat.i,lon.i)
  CA	<-	rbind(CA,vals)
  
  vals = get.vals(years='2001.2010',lat.i,lon.i)
  CA	<-	rbind(CA,vals)
  
  data.out <- data.frame("years"=use.years,"tMean"=vector(length=length(use.years)))
  for (j in 1:length(use.years)){
    use.i = as.numeric(format(CA$DateTime, "%Y"))==use.years[j] &
      as.numeric(format(CA$DateTime, "%m"))==mm.idx
    data.out$tMean[j] = mean(CA$tmp[use.i])
  }
  
  return(data.out)
}

get.vals <- function(years,lat.i=NULL,lon.i=NULL){
  
  start.time = 1
  
  data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_ts3.21/"
  nc	<-	nc_open(filename=paste(data.dir,'cru_ts3.21.',years,'.tmp.dat.nc',sep=''))
  lat.vals	<-	ncvar_get(nc,varid="lat")
  lon.vals	<-	ncvar_get(nc,varid="lon")
  days.since <- ncvar_get(nc,varid="time")
  
  vals = ncvar_get(nc=nc, varid="tmp",start=c(lon.i,lat.i,start.time),count=c(1,1,length(days.since)))
  nc_close(nc)
  
  days.since = as.Date('1900-01-01')+days.since
  df <- data.frame("DateTime"=days.since,"tmp"=vals)
  return(df)
}


# get all the lat and lon vals:
data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_ts3.21/"
nc	<-	nc_open(filename=paste(data.dir,'cru_ts3.21.1981.1990.tmp.stn.nc',sep=''))

lat.vals  <<-	ncvar_get(nc,varid="lat")
lon.vals	<<-	ncvar_get(nc,varid="lon")

years <- seq(1985,2009)

#source("getCRU_temp.R")
summaryTxt = paste('/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_all_sen.csv',sep='')
cat("lat,lon,slope(CC)\n", file=summaryTxt,append=FALSE)

require('zyp')

for (j in 1:length(lat.vals)){
  for (i in 1:length(lon.vals)){
    vals.out <- getCRU(period='JAS',use.years = years,lat.i=j,lon.i=i)
    y = vals.out$tMean
    x = years
    if (all(is.na(y))){
      cat(paste(c(lat.vals[j],',',lon.vals[i],',NA\n'),collapse=''), file=summaryTxt,append=TRUE)
    } else {
      sens <- zyp.sen(y~x)$coefficients[[2]]
      cat(paste(c(lat.vals[j],',',lon.vals[i],',',sens,'\n'),collapse=''), file=summaryTxt,append=TRUE)
    }
    
   
  }
  cat(j);cat(' of ');cat(length(lat.vals));cat('done......\n')
}