# Stock Ticker data
#### begin script

tickers <- c("aapl","sbux","ibm")

library(XML)

dir.create("tickers") 
setwd("tickers")

tryAsNumeric = function(node) { 
  val = xmlValue(node) 
  ans = as.numeric(gsub(",", "", val)) 
  if(is.na(ans)){ 
    ##print(val) 
    val 
  } 
  else{ 
    ##print(ans) 
    ans 
  } 
}

notused <- sapply(tickers, function(ticker){ 
  ##ticker <- tickers[1] 
  dir.create(ticker) 
  setwd(ticker)
  
  url <- paste0("http://stockreports.nasdaq.edgar-online.com/",ticker,".html")
  
  doc <- htmlParse(url) 
  tableNodes <- getNodeSet(doc, "//table")
  
  kk <- seq(11, length(tableNodes), by=3)
  
  notused <- sapply(kk, function(k){ 
    ##k <- kk[1] 
    tab <- readHTMLTable(tableNodes[[k]], elFun = tryAsNumeric, header=TRUE) 
    colnames(tab) <- gsub("r|n|t","",colnames(tab)) 
    write.table(tab, file=sprintf("tab-%02d.csv", k), sep=";", row.names=FALSE, quote=FALSE) 
  }) 
  setwd("..") 
}) 
#### end script