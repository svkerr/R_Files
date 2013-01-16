getmonitor <- function(id, directory, summarize = FALSE){
  
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character
  ## 'directory' is a character vector of length 1 indicating, or a numeric.
  ## the location of the CSV files
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  if (is.character(id)) id <- as.numeric(id)
  filename <- paste(sprintf("%03.f",id), "csv", sep = ".")
  path <- paste(directory, filename, sep = "/")
  content <- read.csv(path)
  if(summarize == TRUE){
    print(summary(content))
  }
  invisible(content)
}

