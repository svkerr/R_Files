corr <- function(directory, threshold = 0){

  # 'directory' is a character vector of length 1 indicating
  # the location of the CSV files
  
  # 'threshold' is a numeric vector of length 1 indicating the
  # number of completely observed observations (on all
  # variables) required to compute the correlation between
  # nitrate and sulfate; the default is 0
  
  # Return a numeric vector of correlations
  
  corvec <- vector(mode="numeric")        # Declare vector to store cor values for each complete set
  
  filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE) # Read in all CSV filenames
  for (i in filenames) {
    tempdata <- read.csv(i)
    
    # For each file find the number of complete obs in order to compare with threshold
    
    redcol <- subset(tempdata, select=c(sulfate,nitrate))
    na_count <- apply(redcol,1, function(x) sum(is.na(x)))
    comp_obs <- redcol[!(na_count > 0),]
    nobs <- nrow(comp_obs)
    if(nobs > threshold) {
      cor_value <- cor(comp_obs$sulfate, comp_obs$nitrate)
      corvec <- append(corvec, cor_value)
    }  
  }
  return(corvec)
}