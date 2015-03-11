

setwd("/Users/stuart/R_Files")

sf311 = read.table("/Users/stuart/DataSets/sanfran/sf311_1.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

install.packages("devtools")
devtools::install_github("twitter/BreakoutDetection")
