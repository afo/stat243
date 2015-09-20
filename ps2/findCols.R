## Find columns to work with, index and classes


findCols = function(head,cols) {
  head=strsplit(head,",")
  headtxt<-unlist(head)
  index=rep(0,length(cols)) #find indices
  realcols=rep("NA",length(cols))
  for (i in 1:length(cols)) {
    index[i]=match(cols[i],headtxt)
  }
  index=sort(index)
  cols<-headtxt[index]
  colclass=rep('NULL',length(headtxt))
  colclass[index]='character' #define colClass. Could be numeric, but then we would not catch e.g 00100000 (cuts away 0's)
  return(list(index,colclass,cols))
}