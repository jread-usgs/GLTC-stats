
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

