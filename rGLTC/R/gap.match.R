gap.match.pair <- function(lake.name,data.nm.1,data.nm.2,master){
  # returns a list of two data.frames, which fit the format of inputs for "calc.sen.slope" method
  year.col <- 2 # column for years
  if (missing(master)){
    print('loading master...')
    master <- load.master()
  }

  if (!data.nm.1 %in% master$Data.type | !data.nm.2 %in% master$Data.type){
    stop(paste('data format names not found in Data.types. Options are:',unique(master$Data.type)))
  }
  if (!lake.name %in% names(master[c(-1,-2)])){
    stop('lake name not found in master file')
  }
  
  # subset data for these two data.types
  use.i.1 <- which(master$Data.type==data.nm.1)
  use.i.2 <- which(master$Data.type==data.nm.2)
  temp.1 <- as.numeric(master[lake.name][[1]][use.i.1])
  yyyy.1 <- master[use.i.1, year.col]
  temp.2 <- as.numeric(master[lake.name][[1]][use.i.2])
  yyyy.2 <- master[use.i.2, year.col]
  sub.dat.1 <- data.frame("Temp"=temp.1,"date"= as.Date(paste(yyyy.1,'-1-1',sep='')))
  sub.dat.2 <- data.frame("Temp"=temp.2,"date"= as.Date(paste(yyyy.2,'-1-1',sep='')))

  
  return(list(df.1=df.1,df.2=df.2))
}

calc.sen.slope <- function(temp.df){
  require("openair")
  # data.frame input with slots for "Temp", "date"
  
  
  sen.slope <- TheilSen(mydata=data.frame("Temp"=NCEP12, "date"=seq(from=as.Date("1985-01-01"), to=as.Date("2009-01-01"),by="years")),pollutant="Temp")
  return(sen.slope)
}

load.master <- function(){
  master<-read.csv("../data/Master_2014-04-23.csv",stringsAsFactors=F)
}