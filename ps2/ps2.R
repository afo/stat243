## Main

## Initial setup ----
set.seed(0)
setwd("~/src/stat243/ps2")
download.file('http://www.stat.berkeley.edu/share/paciorek/ss13hus.csv.bz2','dat.csv.bz2')

## Find column indicies and set column classes ----

head<-readLines(bzfile("dat.csv.bz2"),1) # Extract data header
cols=c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL") #specify what columns to work with

source("findCols.R") #function to find column indices and column classes
index<-findCols(head,cols)[[1]]
colclass<-findCols(head,cols)[[2]]
cols<-findCols(head,cols)[[3]] #the correct placement of the columns
cols


cat(index,sep=",",file="index.txt") #input for bash pre-processing

## Count number of lines in data file ----
con <- file("dat.csv.bz2",open="r")
chunks <- 20000
nLines <- 0
( while((linesread <- length(readLines(con,chunks))) > 0 ) 
  nLines <- nLines+linesread )
close(con)
nLines


## Setup data chunks, sample size and total number of lines ----

blockSize=100000 #read in 100.000 rows at a time
n=nLines # total lines to read in, could be nbrOfLines
sampleSize=10000
use1 = sort(sample(n-1,sampleSize)) #Sample to read


## Reference table ----
# con <- bzfile(description="dat.csv.bz2", open="r")
# tmpRef <- read.csv(con, nrow=n, sep = ',', header=TRUE, stringsAsFactors=FALSE, colClasses = colclass)
# close(con)
# tmpRef[use1,]


## Extract random sample using read.csv ----

source("readcsv.R")
result<-readCSV(data = "dat.csv.bz2",blockSize = blockSize, sampleSize = sampleSize, n = n, use = use1)
subsetRCSV<-result[[1]]
RCSVtime<-result[[2]]

subsetRCSV[1:5,]
rm(result)

## Print out random sample to csv file

write.table(subsetRCSV,sep=",",quote=FALSE,row.names=FALSE,file="dat.csv") #prints output to file dat.csv



## Problem b) Compare with readLine

source("readL.R")
result<-readL(data = "dat.csv.bz2",blockSize = blockSize, sampleSize = sampleSize, n = n, use = use1)
subsetRL<-result[[1]]
RLtime<-result[[2]]

subsetRL[1:5,]
rm(result)

RCSVtime
RLtime

## Problem c) Pre-processing in bash ----

system("bunzip2 -c dat.csv.bz2 | head -10 | cut -d, -f$(cat index.txt) | nl -s, -w1 -v0 | bzip2 > dat2.csv.bz2")
system("bunzip2 -c dat2.csv.bz2 | head")

## Problem d) Cross-tabulation

nbrBedrooms<-as.numeric(subsetRCSV$BDSP)
nbrRooms<-as.numeric(subsetRCSV$RMSP)
table(nbrBedrooms,nbrRooms) #number of rooms cross-tabbed with number of bedrooms 

nbrPersons<-as.numeric(subsetRCSV$NP)
nbrChildren<-as.numeric(subsetRCSV$NOC)
table(nbrChildren,nbrPersons) #number of vehicles in household cross-tabbed with number of children
