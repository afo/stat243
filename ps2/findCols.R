## Find columns to work with, index and classes


findCols = function(head,cols) {
  head=strsplit(head,",") #split the strings
  headtxt<-unlist(head) #unlist result
  index=rep(0,length(cols)) #find indices
  for (i in 1:length(cols)) {
    index[i]=match(cols[i],headtxt)
  }
  index=sort(index) #index in the right order
  cols<-headtxt[index] #correct columns
  colclass=rep('NULL',length(headtxt))
  colclass[index]='character' #define colClass. Could be numeric, but then we would not catch e.g 00100000 (cuts away 0's)
  return(list(index,colclass,cols))
}