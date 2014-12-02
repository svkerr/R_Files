setwd("/Users/stuart/R_Files/TCS")

library(ggplot2)  # installing ggplot2 for my own plot compare and contrast

# Read data 
zip = read.table("DAuditNew_ZipandDownloadSorted", header=T)
data = read.table("fetch_results_sorted.dat", header=F)
str(zip)
plot(zip)
plot(zip$Count)