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
  
  m <- data.frame(matrix(ncol = 4))
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
    m <- rbind(m,read.csv(paste(getwd(),"/",directory,"/",filename,sep="")))
    ##print(filename)
  }
  #return(m)
  ## on veut une matrice
  am <- as.matrix(m[2:4])
  ## on enlève les NA
  ams <- subset(am,!is.na(am[,2]) & !is.na(am[,3]) & !is.na(am[,1]))
  ## on crée une matrice avec des 1...
  amc <- cbind(ams[,3],1)
  ## ...qu'on somme pour faire un total
  dt <- data.table(amc)
  dt2 <- as.matrix(dt[,sum(V2),by=V1])
  colnames(dt2) <- c("id","nobs")
  return(dt2)
}