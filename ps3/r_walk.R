#ps3 random walk
set.seed(0)
library(methods)
setwd("~/src/ps3")


# generate random number

r_walk = function(steps,full=TRUE,start=c(0,0)) {
  
  #checks
  if(!is.numeric(steps)) stop("The Number of steps must be numeric")
  if(!is.logical(full)) stop("Must be logical. Either TRUE for full walk or FALSE for final position")
  if(!isTRUE(steps == floor(steps) & steps>=1)) stop("steps must be a positive integer")
  
  if(!is.double(start)) stop("start must be a vector with two integers")
  if(!isTRUE(length(start)==2)) stop("start must be a vector with two integers")
  if(!isTRUE(start[2] == floor(start[2])) & !isTRUE(start[2] == floor(start[2]))) stop("start must be a vector with two integer values")

  walk<-matrix(start,2,steps-1)
  
  prob<-runif(steps-1)
  
  walk[,which(prob<=0.25)]=walk[,which(prob<=0.25)]+c(1,0)
  walk[,which(0.25<prob & prob<=0.5)]=walk[,which(0.25<prob & prob<=0.5)]+c(-1,0)
  walk[,which(0.5<prob & prob<=0.75)]=walk[,which(0.5<prob & prob<=0.75)]+c(0,1)
  walk[,which(0.75<prob & prob<=1)]=  walk[,which(0.75<prob & prob<=1)]+c(0,-1)
  
  realWalk<-matrix(c(c(start[1],cumsum(walk[1,])),c(start[2],cumsum(walk[2,]))),2,steps,byrow = TRUE)
  
  
  if(full) {
    return(realWalk)
  }
  else {
    return(realWalk[,steps])
  }

}