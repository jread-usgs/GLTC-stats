
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

build_lake_list <- function(time, data, sat_time, sat_data, lake_names, lat, lon){
  
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
        lake_data[[i]] <- list('lat'=lat[i],'lon'=lon[i],'time'=time_sub,'temperature'=temp_sub)
      }
    } else {
      lake_data[[i]] <- list('lat'=lat[i],'lon'=lon[i],'time'=time_sub,'temperature'=temp_sub)
    }
    
  }
  return(lake_data)
}

# open master
library("stringr")
delim = ','
con <- file('../data/Master_2014-06-03.csv', "r", blocking = FALSE)
time_rng <- c(1985,2009)
p_val <- 0.01
line <- readLines(con, n = 1)
lake_names <- str_split(line, delim)[[1]][-2:-1]
line <- readLines(con, n = 4) # code, etc
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
lake_data <- build_lake_list(situ_time, situ_data, sat_time, sat_data, lake_names, lat, lon)
library(Hmisc)
match_cor <- function(lake_1,lake_2){
  t_1 <- lake_1$time
  t_2 <- lake_2$time
  u_2 <- lake_2$time %in% lake_1$time
  u_1 <- lake_1$time %in% lake_2$time[u_2]
  
  to_test <- matrix(c(lake_1$temperature[u_1],lake_2$temperature[u_2]), ncol=2)
  if (dim(to_test)[1]>4){
    p<- rcorr(to_test, type = 'pearson')$P[1,2] # TEST THIS!!!!!
  } else {
    p = NA
  }
  
  
  return(p)
}

library(maps)
library(mapdata)
library(geosphere)
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)

map("world", col="grey80", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
node_size <- function(p_vals){
  sig <- sum(p_vals < p_val, na.rm = T)
  sz <- .5+sig/30
  return(sz)
}

add_arc <- function(lake_1,lake_2){
  inter <- gcIntermediate(c(lake_1$lon, lake_1$lat), c(lake_2$lon, lake_2$lat), n=20, addStartEnd=TRUE)
  lines(inter, lwd = .2, col=rgb(0,0,0,.05,1))
}
for (k in 1:length(lake_names)){
  lake_1 <- lake_data[[k]] # lake_2 is all other lakes 
  u_i <- lake_1$time >= time_rng[1] & lake_1$time <= time_rng[2]
  lake_1$time <- lake_1$time[u_i]
  lake_1$temperature <- lake_1$temperature[u_i]
  other_lakes <- lake_names[lake_names != lake_names[k]]
  p_vals <- vector(length=length(other_lakes))
  for (i in 1:length(other_lakes)){
    lake_2 <- lake_data[[other_lakes[i]]]
    p_vals[i] <- match_cor(lake_1,lake_2)
    if (!is.na(p_vals[i]) & p_vals[i] < p_val){
      add_arc(lake_1,lake_2)
    }
    #cat(p_vals[i]); cat('\n')
  }
  sz <- node_size(p_vals)
  if (sz == 1){
    bg_col <- rgb(.6,0,0,1,1)
  } else {
    bg_col <- rgb(0,0,0,.2,1)
  }
  points(x = lake_1$lon, y = lake_1$lat,cex = sz, pch = 20, col = bg_col, bg=rgb(0,0,0,.4,1))
}



