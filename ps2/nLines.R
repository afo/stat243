# Read the number of lines in the data file, just provide file connection

nLines = function(file) {
  testcon <- bzfile(file,open="r") #open connection
  readsizeof <- 20000
  nooflines <- 0
  ( while((linesread <- length(readLines(testcon,readsizeof))) > 0 ) #count the number of lines 
    nooflines <- nooflines+linesread ) #add number of lines read in every iteration
  close(testcon)
  return(nooflines)
}
