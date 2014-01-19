# Text Reference: Machine Learning With R
# Text Reference: Intro to Statistical Learning: With Apps in R

# Set working directory
setwd("/Users/stuart/Desktop/BigDataDocumentation/DataSets/MachineLearningWithRDatasets/")

# Load Libraries
library(gmodels)   # For CrossTables
library(class)     # For kNN classification
library(kernlab)   # For SVN classification
library(e1071)     # For SVN classification using Hastei Book

##### Exploring data #####
usedcars <- read.csv("chapter 2/usedcars.csv", stringsAsFactors = FALSE)
usedcars$conservative <- usedcars$color %in% c("Black","Gray","Silver","White")
table(usedcars$conservative)
CrossTable(x = usedcars$model, y=usedcars$conservative)

##### kNN Algorithm #############################################
# Step 1: Collecting Data
# Get breast cancer dataset
wbcd <- read.csv("chapter 3/wisc_bc_data.csv", stringsAsFactors = FALSE)

# Step2: Exploring and Preparing the Data
# NOTE: Regardless of the ML algorithm, id variables should always be excluded (messes up results)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)

# Many R ML classifiers require that the "target feature" is coded as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
prop.table(table(wbcd$diagnosis))*100

# Let's take a look at some of the features (since numeric, we use summary)
summary(wbcd[c("radius_mean","area_mean", "smoothness_mean")])

# After looking at above summary results, we see that since kNN is dependent upon measurement scale of the input, we'll need to normalize the dataset values, else the classifier will be messed up
# Create normalize function
normalize <- function(x){
  return( (x - min(x))/(max(x) - min(x)) )
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))    # this is where i remove B,M label too
summary(wbcd_n[c("radius_mean","area_mean", "smoothness_mean")])

# Note: One characteristic of normalizing using this formula is that all "outliers" which may be important are compressed to be at Max: 1

# Create training and test data sets 
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# When we created our training and test data sets, we left out the diagnosis, now we need to add back

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# Step 3: Training a model on the data
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

# Step 4: Evaluating model performance
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
# Analyzing results show that our predicition resulted in 2 False Negatives

# Step 5: Improving model performance
# As noted above, our normalizing algorithm may have suppressed some important outliers, so let's use Z-Scores (no predefined min/max) transformation instead, and then use differnt values for k

wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

# Analyzing results, we see by transforming using Z-Scores, decreases our prediction accuracy

################## Support Vector Machines -- Machine Learning With R ###################
# STEP 2: Explore and prepare the data
letters <- read.csv("chapter 7/letterdata.csv")

# Recall SVM learners require that all features are to be numeric, and each feature scaled to a small intervals. Though some of our features have wide ranges, the SVM R package used will (scale) data automatically

# We'll use 80% of the dataset for training, and the other 20% for testing
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

# STEP 3: Train Model (Let's start building our classifier)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

# STEP 4: Evaluate model performance
letter_predictions <- predict(letter_classifier, letters_test)

# This returns a vector containing a predicted letter for each row of values in the testing data

# Now, let's compare the predicted letter to the true letter in the testing dataset:
table(letter_predictions, letters_test$letter)

################## Support Vector Machines -- Intro to Statistical Learning: With Apps in R  ###################
set.seed(1)
x <- matrix(rnorm(20*2),ncol=2)    # create data matrix
y <- c(rep(-1,10),rep(1,10))       # create label vector
x[y==1,] = x[y==1,] + 1            # What does this do? - Adds 1 to those x.1 and x.2 in bottom half of x

# Are the classes linearlly separable?
plot(x,col=3-y)                    # From this plot, the answer is no

# Next we fit the support vector classifier. Note for svm() to do classification, we need to encode the response as a factor variable
dat <- data.frame(x=x, y= as.factor(y))

library(e1071)
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit,dat)

# Note: The support vectors are plotted as crosses and temaining observations are plotted as circles. We can determine which observations are support vectors by:
svmfit$index
attributes(svmfit)                 # Tells us what is available from svnfit object

# The e1071 library includes a built-in function tune() to perform cross-validation. By default, tune() performs a 10-fold cross-validation. The following set of commands indicate we want to compare SVMs with a linear kernel, using a reange of values of the cost parameter
set.seed(1)
tune.out <- tune(svm,y~., data=dat, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
# From these results, we see that cost = 0.1 results in lowest cross-validation error rate.
# The tune() function stores the best model obtained, which can be accessed as follows:
bestmod <- tune.out$best.model

# Let's check how accurate the SVM fit the hyperplane to classify the labels:
CrossTable(x = y, y = svmfit$fitted, prop.chisq=FALSE)
# Here we see that it misclassified 3 labels

# The predict function can be used to predict the class label on a set of test observations, at any given value of the cost parameter
# Generate test data
xtest <- matrix(rnorm(20*2),ncol=2)
ytest <- sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

# Now we predict the class labels of these test obs. We use the best model obtained through cross-valiation in order to make predictions
ypred <- predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
CrossTable(x = ytest, y = ypred, prop.chisq=FALSE)
# So 19 of the test observations are correctly classified. What if we had instead cused cost = 0.01?

svmfit<- svm(y~., data=dat, kernel="linear",cost=0.01,scale=FALSE)
ypred <- predict(svmfit,testdat)
CrossTable(x = testdat$y, y = ypred, prop.chisq=FALSE)
# In this case, one additional observation is misclassified
