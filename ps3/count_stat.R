#Count Statistics and Phrase Occurences

count_stat = function(df,ci,chunkList) {
  
  regexSentence<-c("([A-Z0-9]([a-z])?([0.9]+)?(\\.)+([A-Za-z])?([0-9]+)?(\\.)?|[[[:alnum:]]](((\\.)+([[:alnum:]])+))?|\\s|[\\,\\'-]|;|&|\\$|\\%)+([\\.?!\\:]|--)")
  regexWords<-"([[:alnum:]]+([\\'\\-\\$\\%]+)?((\\.)?[[:alnum:]]+)?)"
  
  sentence<-str_extract_all(chunkList,regexSentence)
  words<-str_extract_all(sentence,regexWords)
  
  
  df[3,cI]<-nWords<-unlist(lapply(words,length))
  df[4,cI]<-nChar<-unlist(lapply(lapply(words,nchar),sum))
  df[5,cI]<-as.character(round(nChar/nWords,digits=3))
  
  # Phrases and 
  df[6,ci]<-str_count(chunkList,"((\\s)?I(\\s|\\.|\\?|!|,|:|;|'))")
  df[7,ci]<-str_count(chunkList,"((We|\\s(we))(\\s|\\.|\\?|!|,|:|;|'))")
  df[8,ci]<-str_count(chunkList,"(America)(\\s|\\.|\\?|!|,|:|;|')|(American)(\\s|\\.|\\?|!|,|:|;|')")
  df[9,ci]<-str_count(chunkList,"(Democracy|\\s(democracy))(\\s|\\.|\\?|!|,|:|;|')
                      |(Democratic|\\s(democratic))(\\s|\\.|\\?|!|,|:|;|')")
  df[10,ci]<-str_count(chunkList,"(Republic|\\s(republic))(\\s|\\.|\\?|!|,|:|;|')")
  df[11,ci]<-str_count(chunkList,"(Democrat)(\\s|\\.|\\?|!|,|:|;|')
                       |(Democratic)(\\s|\\.|\\?|!|,|:|;|')")
  df[12,ci]<-str_count(chunkList,"(Republican)(\\s|\\.|\\?|!|,|:|;|')")
  df[13,ci]<-str_count(chunkList,"(Free|\\s(free))(\\s|\\.|\\?|!|,|:|;|')
                       |(Freedom|\\s(freedom))(\\s|\\.|\\?|!|,|:|;|')")
  df[14,ci]<-str_count(chunkList,"(War|\\s(war))(\\s|\\.|\\?|!|,|:|;|')")
  df[15,ci]<-str_count(chunkList,"(God)(\\s|\\.|\\?|!|,|:|;|')[^([Bb]less)]")
  df[16,ci]<-str_count(chunkList,"((God)\\s([Bb]less))(\\s|\\.|\\?|!|,|:|;|')")
  df[17,ci]<-str_count(chunkList,"(Jesus|Christ|Christian)(\\s|\\.|\\?|!|,|:|;|')")
  #do grep with ignore case=TRUE
  return(df)
}