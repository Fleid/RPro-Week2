corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  init_m <- complete(directory)
  init_ms <- subset(init_m,init_m[,2]>=threshold)
  
  id <- init_ms[,1]
  
  m <- data.frame(matrix(ncol = 4))
  n <- c()
  names(m) <- c("Date","sulfate","nitrate","ID")
  
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
    
    ## on ne prend que les lignes complètes
    Y <- subset(m[,2:3],!is.na(m[,2]) & !is.na(m[,3]))
    ## on enchaine les corrélations
    n <- c(n,cor(Y[,1],Y[,2]))
  }

  return(n)
}