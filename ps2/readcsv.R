## read.csv method ps2

readCSV = function(data,blockSize,sampleSize,n,use)
{
subsetRCSV<-data.frame(matrix("NA",sampleSize,length(cols)),stringsAsFactors=FALSE) #create full data frame in advance
names(subsetRCSV)<-cols #correct header
use=use1
it=1 #iteration
con <- bzfile(description=data, open="r")
RCSVtime<-system.time(for(i in 1:ceiling(n/blockSize)) {
  if(i==1) {
    tmp <- read.csv(con, nrow=blockSize, sep = ',', stringsAsFactors=FALSE, header=TRUE, colClasses = colclass)
  }
  else {
    tmp <- read.csv(con, nrow=blockSize, sep = ',', stringsAsFactors=FALSE, header=FALSE, colClasses = colclass)
  }
  activeIndex<-which(use<=blockSize & use>0)
  if(length(activeIndex)>0) {
    subsetRCSV[it:(it+length(activeIndex)-1),]<-tmp[use[activeIndex],]
    it=it+length(activeIndex)
  }
  use=use-blockSize
}
)
close(con)
return(list(subsetRCSV,RCSVtime))
}