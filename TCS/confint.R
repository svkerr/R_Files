confint <- function(x,alpha = 0.05) {
  conflevel <- (1 - alpha)*100
  stderr <- sd(x)/sqrt(length(x))
  tcrit <- qt(1 - alpha/2, length(x)-1)
  margin <- stderr * tcrit
  lower <- mean(x) - margin
  upper <- mean(x) + margin
  cat(conflevel, 'Percent Confidence Interval','\n')
  cat('Mean:', mean(x), 'Std. Error:', stderr, '\n')
  cat('Lower Limit:', lower, '\n')
  cat('Upper Limit:', upper, '\n')
  length(x)
}