source("r_walk.R")

# CONSTRUCTOR

rw = function(steps,start=c(0,0)) {
  
  realWalk<-r_walk(steps,start=start) # Create the ranom walk
  #r_walk will check inputs
  
  value<-list(steps=steps, walk=realWalk, start=start,final=realWalk[,steps])
  
  class(value)<-"rw"
  value # print info
}


# PRINT METHOD

print.rw <- function(rw) {
  cat("Random Walk of ", rw$steps, " of steps\n")
  cat("Start position (x y)_start = ", rw$start[1],rw$start[2], "\n")
  cat("Final position (x y)_final = ", rw$final[1],rw$final[2],"\n")
}

# `[` operator

`[.rw` <- function (rw,i)  {
  if(!is.numeric(i)) stop("i must be a positive integer")
  if(!isTRUE(i == floor(i) & i>=1 & i<=rw$steps)) stop("i must be a positive integer between 1 and rw steps")
  y<-rw$walk[,i]   # find position i in the random walk
  return (y)
}


# REPLACEMENT METHOD

`start<-` <- function(x, ...) UseMethod("start<-")
`start<-.rw` <- function(rw,value){
  #checks
  if(!is.double(value)) stop("start must be a vector with two integers")
  if(!isTRUE(length(value)==2)) stop("start must be a vector with two integers")
  if(!isTRUE(value[2] == floor(value[2])) & !isTRUE(value[2] == floor(value[2]))) {
      stop("start must be a vector with two integer values")
  }
  
  old <- rw$start   # old starting position
  rw$start <- value # set new start position
  rw$walk <- rw$walk + rw$start - old # add to the walk
  rw$final <- rw$final + rw$start - old # add to info about final
  return(rw)
}


# PLOTTING METHOD

plot.rw <- function(rw) {
  walk<-rw$walk
  plot(0,type="n",xlab="x",ylab="y",main="Random Walk",col=1:10,xlim=range(walk[1,]),ylim=range(walk[2,])) # initialize plot
  segments(head(walk[1,], -1) 
           , head(walk[2,], -1) 
           , tail(walk[1,], -1) 
           , tail(walk[2,], -1) 
           , col = rainbow(ncol(walk) -1)  # a range of colorsfor every step
  )
  text(walk[1,1:rw$steps],walk[2,1:rw$steps],(1:rw$steps), cex=0.65) # print numbers on every step
  
  points(walk[1,1],walk[2,1],pch=1,col="green", cex = 2) # mark beginning
  points(walk[1,rw$steps],walk[2,rw$steps],pch=11,col="red", cex = 2) # mark end
}

