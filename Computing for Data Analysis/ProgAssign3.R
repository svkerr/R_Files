#Programming Assignment #3

# calculate medians
med_ha <- median(outcome[,11], na.rm = T?RUE)
med_hf <- median(outcome[,17], na.rm = TRUE)
med_pn <- median(outcome[,23], na.rm = TRUE)

# calculate means
mean_ha <- mean(outcome[,11], na.rm = TRUE)
mean_hf <- mean(outcome[,17], na.rm = TRUE)
mean_pn <- mean(outcome[,23], na.rm = TRUE)

# do histograms

hist(outcome[,11],
     main = substitute(bar(X) == k, list(k = mean(outcome[,17], na.rm = TRUE))),
     xlab = "30-day Death Rate",
     xlim = c(0,20),
     ylim = c(0,1200)
     )
abline(v = med_ha, col = "blue", lwd = 2)

hist(outcome[,17], 
     main = paste("Heart Failure"),
     xlab = "30-day Death Rate",
     xlim = c(0,20),
     ylim = c(0,1200),
     prob = TRUE
)
abline(v = med_hf, col = "red", lwd = 2)
plot(density(outcome[,17], na.rm = TRUE))

hist(outcome[,23],
     main = paste("Pneumonia"),
     xlab = "30-day Death Rate",
     xlim = c(0,20),
     ylim = c(0,1200)
)
abline(v = med_pn, col = "green", lwd = 2)

par(mfrow = c(1,3))
