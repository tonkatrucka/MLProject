Predictive Machine Learning - Coursera Course Project
========================================================

## Introduction

The purpose of this project is to predict in which manner 6 people performed barbell lifts. They were asked to perform the lifts correctly and incorrectly in 5 different ways. The data is obtained by accelerometers worn by participants on their belt, forearm, arm and barbell. A machine learning model will be trained using the data from these activity monitors and used to class the test data set into their predicted lift type.   

## Analysis

Required for this analysis we need the following packages. Also setting a seed will allow reproducibility.


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(knitr)

set.seed(2504)
```

### Setting up the data

Let's first read in the data sets required into R.


```r
#### Training data set
raw_train <- read.csv("pml-training.csv", stringsAsFactors=FALSE)

#### Test data set
raw_test <- read.csv("pml-testing.csv", stringsAsFactors=FALSE)
```

Next we will clean the data set.

Creating functions to:

1. Remove any features with NA's
2. Remove any features with empty strings
3. Removing remaining fields that are not related variables for prediction.


```r
filterData <- function(WRK_SET) {
  
  clean <- !sapply(WRK_SET, function(x) any(is.na(x)))
  WRK_SET <- WRK_SET[, clean]
  clean <- !sapply(WRK_SET, function(x) any(x==""))
  WRK_SET <- WRK_SET[, clean]
  
  # Remove the columns that aren't the predictor variables
  remove <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
              "cvtd_timestamp", "new_window", "num_window")
  WRK_SET_remove <- which(colnames(WRK_SET) %in% remove)
  WRK_SET <- WRK_SET[, -WRK_SET_remove]
  
  return(WRK_SET)
}
```

Apply the cleaning function to the data set

Convert the "Classe" into a feature for prediction

```r
raw_train <- filterData(raw_train)
raw_train$classe <- factor(raw_train$classe)

raw_test <- filterData(raw_test)
```

We will apply 5-fold cross-validation using traincontrol to help tune the model


```r
modelpara <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

# We will use random forest as the machine learning model
Modeltrain <- train(classe ~ ., data = raw_train, method = "rf", trControl = modelpara)
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```
## + Fold1: mtry= 2 
## - Fold1: mtry= 2 
## + Fold1: mtry=27 
## - Fold1: mtry=27 
## + Fold1: mtry=52 
## - Fold1: mtry=52 
## + Fold2: mtry= 2 
## - Fold2: mtry= 2 
## + Fold2: mtry=27 
## - Fold2: mtry=27 
## + Fold2: mtry=52 
## - Fold2: mtry=52 
## + Fold3: mtry= 2 
## - Fold3: mtry= 2 
## + Fold3: mtry=27 
## - Fold3: mtry=27 
## + Fold3: mtry=52 
## - Fold3: mtry=52 
## + Fold4: mtry= 2 
## - Fold4: mtry= 2 
## + Fold4: mtry=27 
## - Fold4: mtry=27 
## + Fold4: mtry=52 
## - Fold4: mtry=52 
## + Fold5: mtry= 2 
## - Fold5: mtry= 2 
## + Fold5: mtry=27 
## - Fold5: mtry=27 
## + Fold5: mtry=52 
## - Fold5: mtry=52 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```r
max(head(Modeltrain$results)$Accuracy)
```

```
## [1] 0.9945
```

Next apply the trained model to the test data set.


```r
test_prediction <- predict(Modeltrain, raw_test)

# Push results into a data frame

predicted <- data.frame(RF_pred = test_prediction)
```

Use the code provided by coursera to spit out prediction results into individual submission text files.


```r
answers <- predicted[,1]

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
```

The answers were all submitted using the Coursera portal and found to be correct.














