gap.match.pair <- function(lake.name,data.nm.1,data.nm.2,master,match=TRUE,min.yyyy=1985,max.yyyy=2009){
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
  
  
  sub.dat.1 <- lim.yr(temp.1,yyyy.1,min.yyyy,max.yyyy)
  sub.dat.2 <- lim.yr(temp.2,yyyy.2,min.yyyy,max.yyyy)

  sub.dat.1 <- sub.dat.1[!is.na(sub.dat.1[, 1]), ]
  sub.dat.2 <- sub.dat.2[!is.na(sub.dat.2[, 1]), ]
  
  if (match){
    match.list <- match.df(sub.dat.1,sub.dat.2)
  } else {
    match.list <- list(sub.dat.1,sub.dat.2)###return list w/o matching!
  }
  

  return(match.list)
}

lim.yr <- function(temp,yyyy,min.yyyy,max.yyyy){
  use.i <- min.yyyy <= yyyy & yyyy <= max.yyyy
  
  return(data.frame("Temp"=temp[use.i],"date"=as.Date(paste(yyyy[use.i],'-1-1',sep=''))))
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
  ret.sen.1 <- data.frame("No.years"=c(),'p'=c(),'slope'=c(),'lower'=c(),'upper'=c())
  ret.sen.2 <- data.frame("No.years"=c(),'p'=c(),'slope'=c(),'lower'=c(),'upper'=c())
  na.df <- data.frame("No.years"=NA,'p'=NA,'slope'=NA,'lower'=NA,'upper'=NA)
  
  for (j in 1:length(lake.names)){
    df.list <- gap.match.pair(lake.names[j],type.1,type.2,master,match)
    cat(j);cat(' of '); cat(length(lake.names)); cat('\n')
    if (is.null(df.list)){
      ret.sen.1 <- rbind(ret.sen.1,na.df)
      ret.sen.2 <- rbind(ret.sen.2,na.df)
    } else {
      if (nrow(df.list[[1]])> min.points & nrow(df.list[[2]])> min.points){
        sn.1 <- calc.sen.slope(df.list[[1]])
        sn.2 <- calc.sen.slope(df.list[[2]])
        
        if (nrow(df.list[[1]])!=nrow(df.list[[2]])){stop('df.1 != df.2 length')}
        no.years <- nrow(df.list[[1]]) # must be equal to nrow(sn.2)
        
        sn.1 <- cbind("No.years"=no.years,sn.1)
        sn.2 <- cbind("No.years"=no.years,sn.2)
        ret.sen.1 <- rbind(ret.sen.1,sn.1)
        ret.sen.2 <- rbind(ret.sen.2,sn.2)
      } else {
        ret.sen.1 <- rbind(ret.sen.1,na.df)
        ret.sen.2 <- rbind(ret.sen.2,na.df)
      }
      
    }
    
    
  }
  ret.sen.1 <- cbind(data.frame("Lake"=lake.names),ret.sen.1)
  ret.sen.2 <- cbind(data.frame("Lake"=lake.names),ret.sen.2)
  #sens <- data.frame("Lake.names"=lake.names,'t1'=s.vals[, 1],'t2'=s.vals[, 2])
  sens <- list(type.1=ret.sen.1,type.2=ret.sen.2)
  return(sens)
}

plot.gap.fig <- function(fig.name,write.dat,xlabel,ylabel,ylim,tick.y){
  fig.w  <-  2.25
  fig.h <- 2.25 
  l.mar	<-	0.35
  r.mar	<-	0.03#v.spc
  t.mar	<-	0.03
  b.mar	<-	0.35
  cex.box = 0.7
  cex.ttl = 0.8
  tck	<-	0.01
  
  tick.x <- seq(-.5,.5,.1)
  if (missing(tick.y)){
    tick.y <- seq(-.5,.5,.1)
  }
  if (missing(ylim)){
    ylim=c(-.15,0.4)
  }
  
  par.mgp  <-	data.frame(x=c(.85,-.20,0),y=c(.95,.1,0))
  png(filename = paste0("../Figures/",fig.name,".png"),
      width = fig.w, height = fig.h, units = "in", res=300,family="Arial Narrow")
  
  suppressWarnings(par(mgp=par.mgp$x,omi=c(0,0,0,0),mai=c(b.mar,l.mar, t.mar, r.mar),family="Arial Narrow"))
  plot(write.dat[, 2],write.dat[, 3],
       axes=F,ylab=ylabel,xlab=xlabel,xlim=c(-0.15,0.4),ylim=ylim,pch=1)
  points(write.dat[, 4],write.dat[, 5],ylab=ylabel,xlab=xlabel,pch=18)
  
  axis(1,las=1,cex.axis=cex.box, tck=tck,at=tick.x)
  axis(3,at=c(-1000,1000),las=1, cex.axis=cex.box)#, tck=tck,labels=NA)
  par(mgp=par.mgp$y)
  axis(2,las=1, cex.axis=cex.box, tck=tck,at=tick.y)
  axis(4,at=c(-1000,1000),las=1, cex.axis=cex.box)
  if (missing(ylim)){
    abline(a=0,b=1,lty='solid',col='black',lwd=1.5)
  }
  
  legend('topleft', c('matched','not matched') , 
         lty=0, pch=c(1,18), cex=.75,bty='n')
  dev.off()
}

wrap.write <- function(type.1="In situ",type.2="CRU 3 month Tmean 1C"){
   result <- compare.trends(type.1,type.2,master,match=TRUE)
   
   t.1.nm <- gsub(pattern=' ',replacement='.',x=type.1)
   t.2.nm <- gsub(pattern=' ',replacement='.',x=type.2)
   
   names.1 <- names(result$type.1)
   names.1[-1] <- paste0(names.1[-1], '.', t.1.nm)
   names(result$type.1) <- names.1
   
   names.2 <- names(result$type.2)
   names.2[-1] <- paste0(names.2[-1], '.', t.2.nm)
   names(result$type.2) <- names.2
   
   write.dat <- cbind(result$type.1,result$type.2[-1])
   file.out <- paste('../data/',paste(t.1.nm ,t.2.nm ,'gap.matched.csv',sep='.'),sep='')
   write.table(x=write.dat,file=file.out,quote=F,row.names=F,col.names=T,sep=',')
   
   return(result)
}
type.1 <- "In situ"
type.2 <- "CRU 3 month Tmean 1C"
plot.gap.fig(fig.name=paste(t.1.nm ,t.2.nm,sep='.'),write.dat,xlabel,ylabel)#,ylim=c(-0.01,0.01),tick.y=seq(-0.015,.015,.005))
s.vals <- compare.trends(type.1,type.2)
ylabel <- paste0(type.2,' trends')
xlabel <- paste0(type.1,' trends')
s.no.match <- compare.trends(type.1,type.2,match=F)
