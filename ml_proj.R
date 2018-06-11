rm(list=ls())

if (!require("pacman"))
  install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(knitr, dplyr, ggplot2, GGally, tidyr, car,
               broom, tibble, caret, data.table, xgboost, class)

url1 <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
url2 <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
destfile1 <- 'pml-training.csv'
destfile2 <- 'pml-testing.csv'

setwd('/home/kevintham/work/datasciencecoursera/ml_proj/')

if (!file.exists(destfile1))
  download.file(url1, destfile1, method = "auto")
if (!file.exists(destfile2))
  download.file(url2, destfile2, method = "auto")

train <- fread(destfile1, na.strings = c("NA", "", "#DIV/0!"))
test <- fread(destfile2, na.strings = c("NA", "", "#DIV/0!"))

ytrain <- as.factor(train$classe)
ytest <- as.factor(test$classe)

train <- train[,-160]
test <- test[,-160]

na_count <- sapply(train, function(y) sum(is.na(y)))

nacols <- which(na_count>100)
train <- select(train, -nacols)
test <- select(test, -nacols)

train <- select(train, -c(1:7))
test <- select(test, -c(1:7))

str(train)

set.seed(1)

index <- createDataPartition(ytrain, p=0.8)$Resample1

trainmat <- model.matrix(~.-1,train)

X <- trainmat[index,]
Xval <- trainmat[-index,]
y <- ytrain[index]
yval <- ytrain[-index]

ctrl <- trainControl(method='cv',number=5, classProbs = TRUE, verboseIter=TRUE)

xgbmodel <- train(X, y, method='xgbTree', trControl=ctrl)
