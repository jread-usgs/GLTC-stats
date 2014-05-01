gap.match.pair <- function(lake.name,data.nm.1,data.nm.2,master,match=TRUE){
  # returns a list of two data.frames, which fit the format of inputs for "calc.sen.slope" method
  year.col <- 2 # column for years
  
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

  sub.dat.1 <- sub.dat.1[!is.na(sub.dat.1[, 1]), ]
  sub.dat.2 <- sub.dat.2[!is.na(sub.dat.2[, 1]), ]
  
  if (match){
    match.list <- match.df(sub.dat.1,sub.dat.2)
  } else {
    match.list <- list(sub.dat.1,sub.dat.2)###return list w/o matching!
  }
  

  return(match.list)
}

calc.sen.slope <- function(temp.df){
  require("openair")
  # data.frame input with slots for "Temp", "date"
  
  
  sen.stats <- TheilSen(mydata=temp.df,pollutant="Temp")
  
  slope.p<-sen.stats$data$res2[1,10]
  slope.slope<-sen.stats$data$res2[1,11]
  slope.lower<-sen.stats$data$res2[1,15]
  slope.upper<-sen.stats$data$res2[1,16]
  
  graphics.off()
  return(data.frame('p'=slope.p,'slope'=slope.slope,'lower'=slope.lower,'upper'=slope.upper))
  
}

match.df <- function(df.1,df.2){
  
  if (dim(df.1)[1]==0 | dim(df.2)[1]==0){
    return(NULL)
  } else {
    i.1 <- df.1$date %in% df.2$date
    df.1 <- df.1[i.1, ]
    i.2 <- df.2$date %in% df.1$date
    df.2 <- df.2[i.2, ]
    if (any(!df.1$date==df.2$date)){stop('error with dates returned. df.1 dates are not equal to df.2 dates')}
    return(list(df.1=df.1,df.2=df.2))
  }
}

load.master <- function(){
  master<-read.csv("../data/Master_2014-04-23.csv",stringsAsFactors=F)
}

lake.names <- function(master){
  lake.names <- names(master[c(-1,-2)])
  return(lake.names)
}


compare.trends <- function(type.1,type.2,master,match=TRUE){
  
  min.points <- 5 # for trend calc
  if (missing(master)){
    print('loading master...')
    master <- load.master()
  }
  lake.names <- lake.names(master)
  
  s.vals <- matrix(nrow=length(lake.names),ncol=2)
  
  for (j in 1:50){#length(lake.names)){
    df.list <- gap.match.pair(lake.names[j],type.1,type.2,master,match)
    print(j)
    if (is.null(df.list)){
      print(paste('skipping',j))
    } else {
      if (nrow(df.list[[1]])> min.points & nrow(df.list[[2]])> min.points){
        sn.1 <- calc.sen.slope(df.list[[1]])
        sn.2 <- calc.sen.slope(df.list[[2]])
        print(sn.1$slope)
        s.vals[j,1] <- sn.1$slope
        s.vals[j,2] <- sn.2$slope
      }
      
    }
    
    
  }
  sens <- data.frame("Lake.names"=lake.names,'t1'=s.vals[, 1],'t2'=s.vals[, 2])
  names(sens) <- c("Lake.names",type.1,type.2)
  return(sens)
}

type.1 <- "Satellite"
type.2 <- "CRU 3 month Tmax 1C"
s.vals <- compare.trends("Satellite","CRU 3 month Tmax 1C")
ylabel <- paste0(type.2,' trends')
xlabel <- paste0(type.1,' trends')
s.no.match <- compare.trends("Satellite","CRU 3 month Tmax 1C",match=F)

plot(s.vals[, 2],s.vals[, 3],ylab=ylabel,xlab=xlabel)
points(s.no.match[, 2],s.no.match[, 3],ylab=ylabel,xlab=xlabel,pch=19)

