complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  m <- data.frame()
  n <- data.frame()
  
  
  for (i in seq_along(id)) 
  {
    ## On transforme l'index en nom de fichier, attention au 00 en préfixe
    filename <- 
      paste(
        
        if (id[i] < 10) {paste("00",id[i],sep="")} 
        else if (id[i] < 100) {paste("0",id[i],sep="")} 
        else {as.character(id[i])}
        
        ,".csv"
        ,sep=""
      )
    m <- read.csv(paste(getwd(),"/",directory,"/",filename,sep=""))
    ##print(filename)
    ams <- subset(m,!is.na(m[,2]) & !is.na(m[,3]))
    
    n <- rbind(n,c(id[i],dim(ams)[1]))
  }
  names(n) <- c("id","nobs")
  return(n)
}