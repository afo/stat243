#Text extract


txt_extract = function(url,title,print=FALSE) {
  doc<-htmlParse(url)
  listOfANodes <- getNodeSet(doc, "//p")
  txt<-unlist(sapply(listOfANodes, xmlValue))
  rm(listOfANodes)
  if(!(length(grep("2012",url))==1)) { 
    #All debates are stored the same except 2012 & to avoid copies of 2008 debate
    txt<-txt[2]
  }
  
  txt<-paste(txt,collapse=" ")
  if(length(grep("2000",url)==1)) {
    txt<-gsub("MODERATOR:","LEHRER:",txt)
  }
  regex="(LEHRER):.*(Thank)\\s(yo)[(A-Z)|\\s|,|(a-z)]+\\.(\\s(God)[(A-Z)|(a-z)|\\s]+\\.)?(\\(APPLAUSE\\))?"
  txt<-str_extract(txt,regex)
  txt<-unlist(strsplit(gsub("([A-Z]+:)","~\\1",txt),"~"))
  txt<-txt[2:length(txt)]
  txt<-gsub("â€.","",txt) #remove weird characters
  txt<-gsub('\"',"'",txt) #change quotation standard
  if(print) cat(c(title,txt),sep="\n\n")
  return(txt)
}