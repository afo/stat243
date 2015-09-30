# Speaker Chunks

speaker_chunks = function(txt) {
  speakers=unlist(unique(str_extract_all(txt,"[A-Z]+:")))
  spokenChunks <- lapply(rep(NA,length(speakers)), function(i) i)
  names(spokenChunks)<-gsub(":","",speakers) #correct header
  
  for (i in 1:length(speakers)) {
    txt2<-gsub("\\([A-Za-z]+\\)","",txt) #remove laughter etc
    
    #spokenChunks[i]<-gsub(paste(speakers[i]," ",sep=""),"",paste(grep(speakers[i],txt2,value=TRUE),sep="",collapse=""))
    spokenChunks[[i]]<-gsub(paste(speakers[i]," ",sep=""),"",grep(speakers[i],txt2,value=TRUE))
  }
  return(spokenChunks)
}