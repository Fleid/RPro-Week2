pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
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
  ## return(m)
  return(
      mean(
        as.matrix(m[pollutant])
        ,na.rm = TRUE
      )
  )
}