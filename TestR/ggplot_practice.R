# Set working directory
setwd("/Users/stuart/R_Files/TestR")

# load ggplot2 library
library(ggplot2)

# load diamonds dataset contained in ggplot2
data(diamonds)
hist(diamonds$carat, xlab = 'Carat')
plot(diamonds$carat, diamonds$price)

ggplot(data=diamonds) + geom_histogram(aes(x=carat))

ggplot(data=diamonds) + geom_density(aes(x=carat), fill='grey60')

ggplot(diamonds, aes(x=carat,y=price)) + geom_point()

g <- ggplot(diamonds,aes(x=carat,y=price))
# we can now add any layer to g
g + geom_point()
g + geom_point(aes(color=color))
g + geom_point(aes(color=clarity))

hello.person <- function(name) {
  sprintf('Hello %s', name)
}

double.num <- function(x)
{
  return(x * 2)
}

double.list <- function(list)
{
  return(list * 2)
}

a <- vector(1:12)