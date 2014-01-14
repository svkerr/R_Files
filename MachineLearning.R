# Text Reference: Machine Learning With R

# Set working directory
setwd("/Users/stuart/Desktop/BigDataDocumentation/DataSets//MachineLearningWithRDatasets/")

# Load Libraries
library(gmodels)   # For CrossTables
library(class)     # For kNN classification

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

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
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
# Analyzing results show that our predicition resulted in 2 False Positives

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

