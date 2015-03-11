# File to test SVM

library(e1071)
library(rpart)
library(mlbench)
library(dplyr)

# In this example, we use the glass data from the UCI Repoistory of Machine Learning Databases for classification. The task is to predict the type of a glass on basis of its chemical analysis. We start by splitting the data into a train and test set.

data(Glass)

# Split data into train and test sets:
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))  # Note default replace is false
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

# Both for the SVM and the partitioning tree (via rpart()), we fit the model and try to predict the test set values
# SVM
svm.model <- svm(Type ~ ., data=trainset, cost=100, gamma=1)
svm.pred <- predict(svm.model, testset[,-10])
# Compute the svm confusion matrix
table(pred = svm.pred, true = testset[,10])
print(svm.model)
