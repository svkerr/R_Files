# Test Domino cloud service and Domino R-package

setwd("/Users/stuart/R_Files/quick-start")
library(dplyr)
library(domino)
domino.login("skerr", "begonia1!")

domino.get("quick-start")
domino.run("main.R")

