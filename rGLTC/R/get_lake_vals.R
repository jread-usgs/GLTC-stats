
get_data <- function(line, delim = ',', ncol){
  
  data = matrix(nrow = length(line), ncol = ncol)
  for (j in 1:length(line)){
    data[j, ] <- as.numeric(str_split(line[j], delim)[[1]][-2:-1])
  }
  return(data)
}

get_time <- function(line, delim = ','){
  time = vector(mode = 'numeric', length= length(line))
  for (j in 1:length(line)){
    time[j] <- as.numeric(str_split(line[j], delim)[[1]][2])
  }
  return(time)
}

build_lake_list <- function(time, data, sat_time, sat_data, lake_names, lat, lon, region){
  
  lake_data = vector("list", length(lake_names))
  names(lake_data) <- lake_names
  
  for (i in 1:length(lake_names)){
    temperatures <- data[, i]
    u_i <- !is.na(temperatures)
    time_sub <- time[u_i]
    temp_sub <- temperatures[u_i]
    if (!any(u_i)){
      # replace w/ sat data
      temperatures <- sat_data[, i]
      u_i <- !is.na(temperatures)
      if (any(u_i)){ # else it stays empty
        time_sub <- sat_time[u_i]
        temp_sub <- temperatures[u_i]
        lake_data[[i]] <- list('lat'=lat[i],'lon'=lon[i],'time'=time_sub,'temperature'=temp_sub, 'region' =region[i])
      }
    } else {
      lake_data[[i]] <- list('lat'=lat[i],'lon'=lon[i],'time'=time_sub,'temperature'=temp_sub, 'region' =region[i])
    }
    
  }
  return(lake_data)
}

get_region_matches <- function(lake_data, regions){
  
  u_i <- vector(length = length(lake_data))
  for (i in 1:length(lake_data)){
    if (any(regions %in% lake_data[[i]]$region)){
      u_i[i] = TRUE
    }
  }
  region_matches <- names(lake_data)[u_i]
  return(region_matches)
}

# open master
library("stringr")
regions <- c("ENA", "WNA", "SENA")
min_size <<- 1
delim = ','
con <- file('../data/Master_2014-06-03.csv', "r", blocking = FALSE)
time_rng <- c(1985,2009)
p_val <- 0.01
min_n <- 13
line <- readLines(con, n = 1)
lake_names <- str_split(line, delim)[[1]][-2:-1]
line <- readLines(con, n = 3) # code, etc
line <- readLines(con, n = 1)
region <- str_split(line, delim)[[1]][-2:-1]
line <- readLines(con, n = 1)
lat <- as.numeric(str_split(line, delim)[[1]][-2:-1])
line <- readLines(con, n = 1)
lon <- as.numeric(str_split(line, delim)[[1]][-2:-1])
line <- readLines(con, n = 14) # skip metadata
line <- readLines(con, n = 115)

situ_data <- get_data(line, ncol = length(lake_names))
situ_time <- get_time(line)

line <- readLines(con, n = 105)
line <- readLines(con, n = 27)
close(con)

sat_data <- get_data(line, ncol = length(lake_names))
sat_time <- get_time(line)

# lake_data is the list of the lat, lon, time and temp for each lake.
lake_data <- build_lake_list(situ_time, situ_data, sat_time, sat_data, lake_names, lat, lon, region)
plot_lakes <- get_region_matches(lake_data, regions)

library(Hmisc)

library(pracma)

match_cor <- function(lake_1,lake_2, detrend = TRUE){
  t_1 <- lake_1$time
  t_2 <- lake_2$time
  u_2 <- lake_2$time %in% lake_1$time
  u_1 <- lake_1$time %in% lake_2$time[u_2]
  temp_1 <- lake_1$temperature[u_1]
  temp_2 <- lake_2$temperature[u_2]
  
  to_test <- matrix(c(temp_1,temp_2), ncol=2)
  if (dim(to_test)[1] >= min_n){
    if (detrend){
      temp_1 <- detrend(x = temp_1, tt = 'linear')[,1]
      temp_2 <- detrend(x = temp_2, tt = 'linear')[,1]
      to_test <- matrix(c(temp_1,temp_2), ncol=2)
    }
    cor_v <- rcorr(to_test, type = 'pearson')
    p<- data.frame(p=cor_v$P[1,2], r =  cor_v$r[1,2])
  } else {
    p<- data.frame(p=NA, r =  NA)
  }
  
  return(p)
}

plot_region <- function(region){
  
  fake_lake <- list(region=region, lat=0,lon=0)
  location <- get_lat_region(fake_lake)
  points(location$lon,location$lat,cex = 16, pch=21, bg='white',col='black')
  text(location$lon,location$lat,region)
}

get_lat_region <- function(lake){
  lat <- data.frame('Africa' = 32, 'Asia' = 48, 'Oceania' = 25, 
                    'Europe' = 58, 'Middle.East' = 39, 'South.America' = 25)
  
  lon <- data.frame('Africa' = -156, 'Asia' = -160, 'Oceania' = -150, 
                    'Europe' = -55, 'Middle.East' = -55, 'South.America' = -65)
  
  region <- strrep(lake$region, " ", ".")
  lake$lat <- as.numeric(lat[region])
  lake$lon <- as.numeric(lon[region][1])
  return(lake)
}



library(maps)
library(mapdata)
library(geosphere)
xlim <- c(-173, -48)
ylim <- c(18.039321, 75.856229)

map("worldHires",c("Canada","USA", "Mexico"), xlim=xlim, ylim=ylim) #col="grey80", fill=TRUE, bg="white", lwd=0.05, 
node_size <- function(p_vals){
  sig <- sum(p_vals < p_val, na.rm = T)
  sz <- min_size+sig/25
  return(sz)
}

add_arc <- function(lake_1,lake_2,r_val){
  inter <- gcIntermediate(c(lake_1$lon, lake_1$lat), c(lake_2$lon, lake_2$lat), n=20, addStartEnd=TRUE)
  lines(inter, lwd = .2, col=rgb(0,0,0,.2*abs(r_val),1))
}


for (k in 1:length(plot_lakes)){
  u_i <- which(names(lake_data) == plot_lakes[k])
  lake_1 <- lake_data[[u_i]] # lake_2 is all other lakes 
  u_i <- lake_1$time >= time_rng[1] & lake_1$time <= time_rng[2]
  lake_1$time <- lake_1$time[u_i]
  lake_1$temperature <- lake_1$temperature[u_i]
  other_lakes <- lake_names[lake_names != plot_lakes[k]]
  p_vals <- vector(length=length(other_lakes))
  for (i in 1:length(other_lakes)){
    
    lake_2 <- lake_data[[other_lakes[i]]]
    other_region <- lake_2$region
    cor_v <- match_cor(lake_1,lake_2)
    p_vals[i] <- cor_v$p
    r_val <- cor_v$r
    if (!is.na(p_vals[i]) & p_vals[i] < p_val){
      
      if (!lake_2$region %in% regions){
        lake_2 <- get_lat_region(lake_2)
      }
      add_arc(lake_1,lake_2,r_val)
    }
    #cat(p_vals[i]); cat('\n')
  }
  sz <- node_size(p_vals)
  if (sz == min_size){
    bg_col <- rgb(.6,0,0,1,1)
  } else {
    bg_col <- rgb(0,0,0,.2,1)
  }
  points(x = lake_1$lon, y = lake_1$lat,cex = sz, pch = 20, col = bg_col, bg=rgb(0,0,0,.4,1))
}
region_markers <- unique(region)[!(unique(region) %in% regions)]
for (k in 1:length(region_markers)){
  plot_region(region_markers[k])
}


