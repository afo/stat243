## readLines method ps2

readL = function(data,blockSize,sampleSize,n,use)
{
  subsetRL<-data.frame(matrix("NA",sampleSize,length(cols)),stringsAsFactors=FALSE)
  names(subsetRL)<-cols
  use=use1+1
  it=1 #iteration
  
  con <- bzfile(description="dat.csv.bz2", open="r")
  RLtime1<-system.time(for(i in 1:10) {
    tmp <- readLines(con,n=blockSize)
    tmp<-strsplit(tmp,",")
    activeIndex<-which(use<=blockSize & use>0)
    if(length(activeIndex)>0) {
      for(j in 1:length(activeIndex)) {
        subsetRL[it,]<-tmp[[use[activeIndex[j]]]][index]
        it=it+1
      }
    }
    use=use-blockSize
  }
  
  )
  close(con)
  return(list(subsetRCSV,RLtime1))
}