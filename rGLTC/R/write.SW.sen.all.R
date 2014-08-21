

# get all the lat and lon vals:

year = 1985
library('ncdf4')
library('zyp')
data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/Radiation/"
nc	<<-	nc_open(filename=paste(data.dir,'SRB_sw_dn_JAS.nc',sep=''))

lat.vals  <<-	ncvar_get(nc,varid="lat")
lon.vals	<<-	ncvar_get(nc,varid="lon")
#time <<- ncvar_get(nc,varid="time")

years <<- seq(1985,2007)

get.all.SW <-	function(lat.i,lon.i){
  
  vals.out <- ncvar_get(nc=nc, varid="sw_dn_JAS",start=c(lon.i,lat.i,1,2),count=c(1,1,1,length(years)))
  return(vals.out)
}


#source("gap.match.R")
summaryTxt = paste('/Users/jread/Documents/GLTC-stats/rGLTC/data/SWRad_all_sen.csv',sep='')
cat("lat,lon,slope(SW)\n", file=summaryTxt,append=FALSE)



for (j in 1:length(lat.vals)){
  for (i in 1:length(lon.vals)){

    vals.out <- get.all.SW(lat.i=j,lon.i=i)
    y = vals.out
    x = as.numeric(as.Date(paste(years,'-1-1',sep='')))/365.25
    sens <- zyp.sen(y~x)$coefficients[[2]]
    cat(paste(c(lat.vals[j],',',lon.vals[i],',',sens,'\n'),collapse=''), file=summaryTxt,append=TRUE)
  }
  cat(j);cat(' of ');cat(length(lat.vals));cat('done......\n')
}