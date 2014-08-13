
getCRU	<-	function(lat,long, period='JAS',use.years = seq(1985,2009),lat.i=NULL,lon.i=NULL){
	require(ncdf4)
  if (period=="JAS"){
    mm.idx  <-	c(7,8,9)
  } else if (period=='JFM'){
    mm.idx <- c(1,2,3)
  }	else if (period=='annual'){
	mm.idx <- seq(1,12)
  }
	
  
  
  vals = get.vals(lat,long,years='1981.1990',lat.i,lon.i)

  CA = vals
  
	vals = get.vals(lat,long,years='1991.2000',lat.i,lon.i)
	CA	<-	rbind(CA,vals)
  
	vals = get.vals(lat,long,years='2001.2010',lat.i,lon.i)
	CA	<-	rbind(CA,vals)
  
  data.out <- data.frame("years"=use.years,"tMean"=vector(length=length(use.years)))
  for (j in 1:length(use.years)){
    use.i = as.numeric(format(CA$DateTime, "%Y"))==use.years[j] &
      as.numeric(format(CA$DateTime, "%m"))==mm.idx
    data.out$tMean[j] = mean(CA$tmp[use.i])
  }
  
	return(data.out)
}

get.vals <- function(lat,long,years,lat.i=NULL,lon.i=NULL){
  
  start.time = 1
  
  data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_ts3.21/"
  nc	<-	nc_open(filename=paste(data.dir,'cru_ts3.21.',years,'.tmp.dat.nc',sep=''))
  lat.vals	<-	ncvar_get(nc,varid="lat")
  lon.vals	<-	ncvar_get(nc,varid="lon")
  days.since <- ncvar_get(nc,varid="time")
  
  if (is.null(lat.i)){
    lat.i  <-	which.min(abs(lat.vals-lat))[1]
  }
  if (is.null(lon.i)){
    lon.i  <-	which.min(abs(lon.vals-long))[1]
  }
  
  vals = ncvar_get(nc=nc, varid="tmp",start=c(lon.i,lat.i,start.time),count=c(1,1,length(days.since)))
  nc_close(nc)
  
  days.since = as.Date('1900-01-01')+days.since
  df <- data.frame("DateTime"=days.since,"tmp"=vals)
  return(df)
}

get.station <- function(lat,long,years){
  start.time = 1
  
  data.dir  <-  "/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_ts3.21/"
  nc	<-	nc_open(filename=paste(data.dir,'cru_ts3.21.',years,'.tmp.stn.nc',sep=''))
  lat.vals	<-	ncvar_get(nc,varid="lat")
  lon.vals	<-	ncvar_get(nc,varid="lon")
  days.since <- ncvar_get(nc,varid="time")
  
  lat.i	<-	which.min(abs(lat.vals-lat))[1]
  lon.i	<-	which.min(abs(lon.vals-long))[1]
  vals = ncvar_get(nc=nc, varid="stn",start=c(lon.i,lat.i,start.time),count=c(1,1,length(days.since)))
  nc_close(nc)
  
  days.since = as.Date('1900-01-01')+days.since
  df <- data.frame("DateTime"=days.since,"stn"=vals)
  return(df)
}

plot.cru <- function(cru.data,points){

  min.stn <- 10
  library(ggplot2)
  library(maps)
  library(RColorBrewer) # for brewer.pal
  na.i <- cru.data$z < min.stn
  s.1 <- cru.data$z >= min.stn & cru.data$z < 50
  s.2 <- cru.data$z >= 50 & cru.data$z < 100
  s.3 <- cru.data$z >= 100 & cru.data$z < 300
  s.4 <- cru.data$z >= 300 & cru.data$z < 500
  s.5 <- cru.data$z >= 500 
  
  cru.data$z[na.i] = 0
  cru.data$z[s.1] = 1
  cru.data$z[s.2] = 2
  cru.data$z[s.3] = 3
  cru.data$z[s.4] = 4
  cru.data$z[s.5] = 5
 # hist(cru.data$z)
  names(cru.data) = c('Longitude','Latitude','Station_count')
  world_map <- map_data("world")
 
 p1 <- ggplot()
 
 p1 <- p1 + geom_tile(data=cru.data,aes(x=Longitude,y=Latitude,fill=Station_count)) +
   scale_fill_gradientn(guide = 'legend', colours = c("red","darkgoldenrod2","darkolivegreen2","darkslategrey","darkorchid2","dodgerblue1"),#brewer.pal(n = 6, name = 'Set1'),
                        name="Station Count",
                        limits=c(0,max(cru.data$Station_count,na.rm=T)),
                        na.value='white',
                        labels=c("< 10", "10 >= station# < 50", "50 >= station# < 100", 
                                 "100 >= station# < 300", "300 >= station# < 500", ">= 500"))
 p1 <- p1 + geom_polygon(data=world_map,aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
 p1 <- p1 + theme(legend.justification=c(0,0), legend.position=c(.1,.2))
 p1 <- p1 + geom_point(data=points, aes(x = Longitude,y=Latitude),pch=23,bg='white',col='black',cex=2,lwd=3) + theme(panel.background = element_blank()) 
 p1
}

flatten.cru <- function(cru.stn){
  x = ncvar_get(cru.stn,varid="lon")
  y = ncvar_get(cru.stn,varid="lat")
  z = ncvar_get(cru.stn,varid="stn",start=c(1,1,49),count=c(720,360,1)) #only one month for now!!! (jan 1985)
  
  def.vec <- vector(length=(length(x)*length(y)))
  
  data.out <- data.frame(x=def.vec,y=def.vec,z=def.vec)
  x.vec = def.vec
  y.vec = def.vec
  z.vec = def.vec
  cnt = 1
  for (j in 1:length(x)){
    for (i in 1:length(y)){
      x.vec[cnt] = x[j]
      y.vec[cnt] = y[i]
      z.vec[cnt] = z[j, i]
      cnt = cnt+1
    }
    print(paste(j,'of',length(x)))
  }
  data.out = data.frame('x'=x.vec,'y'=y.vec,'z'=z.vec)
  return(data.out)
}
require(ncdf4)

period = 'JFM'

master  <-  read.table("/Users/jread/Documents/GLTC-stats/rGLTC/data/Master_names_lat_lon.txt",header=TRUE,sep='\t')
lake.names	<-	names(master)
lake.lat	<-	as.numeric(master[1,])
lake.lon	<-	as.numeric(master[2,])

data.dir  <-  "/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_ts3.21/"
years = '1981.1990'
nc  <-	nc_open(filename=paste(data.dir,'cru_ts3.21.',years,'.tmp.stn.nc',sep=''))
cru.data <- flatten.cru(nc)

points = data.frame(Longitude=lake.lon,Latitude=lake.lat,'Station_count'=rep(1,length(lake.lon)))
plot.cru(cru.data,points)





years = seq(1985,2009)
num.years = length(years)
num.lakes = length(lake.names)

lake.CRU <- matrix(nrow=num.years,ncol=num.lakes)
for (i in 1:num.lakes){
  lake.CRU[, i] = getCRU(lat=lake.lat[i],long=lake.lon[i],period, use.years = years)$tMean
  cat('done with ');cat(lake.names[i]);cat('\n')
}

lake.df = data.frame(lake.CRU)
names(lake.df) = lake.names
lake.df <- cbind(data.frame("years"=years),lake.df)

write.table(x=lake.df,file=paste("/Users/jread/Documents/GLTC-stats/rGLTC/data/CRUts3.21_",period,".tsv",sep=''),quote=FALSE,sep='\t',row.names=FALSE)