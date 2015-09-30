#URL and title extract

url_title_extract = function(year,links1,titles1) {
  

  
  
  # Put in a function (grep debate URL's)
  ind1<-grep("The First",titles1) #grep for all the first debates
  links<-links1[ind1]
  titles<-titles1[ind1]
  ind<-grep(year,titles)
  title<-titles[ind]
  url<-links[ind]


  
  return(c(url,title))

}