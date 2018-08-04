#XGBoost

# Function for recall, precision, f1.score, accuracy
measurePrecisionRecall <- function(actual_labels, predict){
  confusion.matrix <- table(actual_labels, predict)
  tn <- table(actual_labels, predict)[1]
  fn <- table(actual_labels, predict)[2]
  fp <- table(actual_labels, predict)[3]
  tp <- table(actual_labels, predict)[4]
  total <- tn+fn+fp+tp
  
  accuracy <- (tn+tp)/total
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  f1.score <- 2*precision*recall/(precision+recall)
  
  
  cat("Accuracy", accuracy, '\n')
  cat("Precision", precision, '\n')
  cat("Recall", recall, '\n')
  cat("F1 Score", f1.score, '\n', '----', '\n')
  
  return(c(accuracy, precision, recall, f1.score))
}

######################################################
# Preprocessing the data
######################################################

# Importing the dataset
df <- read.csv("./archive/Churn_Modelling.csv")

# Focusing on the variable that can have an impact
# Eliminating Row Number, CustomerID, and Surname
df <- df[4:14]

# Encoding the categorical variables as factors
df$Geography <- as.numeric(factor(df$Geography,
                                  levels = c('France', 'Germany', 'Spain'),
                                  labels = c(1, 2, 3)))

df$Gender <- as.numeric(factor(df$Gender,
                               levels = c('Female', 'Male'),
                               labels = c(1, 2)))

######################################################
# Splitting the dataset into training and testing
######################################################

# Creating the split which provides a boolean table which we will match on the dataset table
library(caTools)
set.seed(123)
split <- sample.split(df$Exited, SplitRatio = 0.8)

training.set <- subset(df, split == TRUE)
testing.set <- subset(df, split == FALSE)

# Checking the datatype of the variables
sapply(testing.set, class)

######################################################
# Fitting the XGBoost
######################################################

library(xgboost)
classifier <- xgboost(data = as.matrix(training.set[ ,!(colnames(testing.set) == "Exited")]), 
                      label = training.set$Exited, 
                      nrounds = 10)

######################################################
# Using the k fold
######################################################

# Applying the k-fold
library(caret)
folds <- createFolds(training.set$Exited, k = 10)

cv <- lapply(folds, function(x) {
  training.fold <- training.set[-x,]
  testing.fold <- training.set[x,]
  classifier <- xgboost(data = as.matrix(training.set[ ,!(colnames(training.set) == "Exited")]), 
                        label = training.set$Exited, 
                        nrounds = 10)
  y.pred <- predict(classifier, newdata = as.matrix(testing.fold[-11])) # 11 is the dependent variable
  y.pred <- (y.pred >= 0.5)
  confusion.matrix <- table(testing.fold$Exited, y.pred)

  performance.metrics <- measurePrecisionRecall(y.pred, testing.fold$Exited)
  accuracy <- performance.metrics[1]
  return(accuracy)
})


accuracy <- mean(as.numeric(cv)) #0.87775
