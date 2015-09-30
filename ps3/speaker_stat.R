# Extract text for each speaker and count laughs etc

speaker_stat = function (txt) {
  speakers=unlist(unique(str_extract_all(txt,"[A-Z]+:")))
  

#   nonSpoken=unique(unlist(unique(str_extract_all(txt,"\\(([A-Za-z]+\\))"))))
  nonSpoken=c("\\(APPLAUSE\\)|\\(Applause\\)","\\(LAUGHTER\\)")
  nonSpokenCount<-data.frame(matrix(0,length(nonSpoken),length(speakers)),stringsAsFactors=FALSE)
  names(nonSpokenCount)<-gsub(":","",speakers)
  row.names(nonSpokenCount)<-c("APPLAUSE","LAUGHTER")
  
  for (i in 1:length(speakers)) {
    for (j in 1:length(nonSpoken)) {
      nonSpokenCount[j,i]<-sum(str_count(grep(speakers[i],txt,value=TRUE),nonSpoken[j]))
    }
    
  }
  
  ## LÄGG BARA IHOP APPLAUSE OCH LAUGHTER
  

  return(nonSpokenCount)
}