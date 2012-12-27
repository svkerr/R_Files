GenStats <- function(x,column) {
  theMean <- apply(x[column],2,mean)
  stanDev <- apply(x[column],2,sd)
  median <- apply(x[column],2,median)
  meanAbsDev <- apply(x[column],2,mad)
  MIN <- apply(x[column],2,min)
  MAX <- apply(x[column],2,max)
  cat('Min: ', MIN,'\n')
  cat('Max: ', MAX,'\n')
  cat('Mean: ',theMean,'\n')
  cat('Median',median,'\n')
  cat('SD: ', stanDev,'\n')
  cat('MAD: ',meanAbsDev,'\n')
}
