complete <- function(directory, id) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  nobs <- vector()
  
  for(i in id) {
    # Convert directory and id to useable filename and path and get the file
    filename <- paste(sprintf("%03.f",i), "csv", sep = ".")
    path <- paste(directory, filename, sep = "/")
    tempdata <- read.csv(path)
    
    # Process the file and get number of complete observation (nobs)
    redcol <- subset(tempdata, select=c(sulfate,nitrate))
    na_count <- apply(redcol,1, function(x) sum(is.na(x)))
    nobs <- append(nobs, nrow(redcol[!(na_count > 0),]))
  }
  df <- data.frame(id,nobs)
  return(df)
}