# K-Fold Cross Validation

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



# Importing the dataset
df <- read.csv("./archive/Social_Network_Ads.csv")
df <- subset(df, select=c("Gender", "Age", "EstimatedSalary", "Purchased"))

# Changing the categorical data of gender
df$Gender <- factor(df$Gender,
                    levels = c('Male','Female'),
                    labels = c(0,1))


######################################################
# Splitting the dataset into training and testing
######################################################
library(caTools)
set.seed(123)
split <- sample.split(df$Purchased, SplitRatio = 0.8)

training.set <- subset(df, split == TRUE)
testing.set <- subset(df, split == FALSE)

# Checking the datatype of the variables
sapply(testing.set, class)

# Gender is a factor, must change it to integer
training.set$Gender <- as.numeric(training.set$Gender)
testing.set$Gender <- as.numeric(testing.set$Gender)

######################################################
# Feature Scaling
######################################################
testing.set[, c("Age","EstimatedSalary")] <-  
  scale(subset(testing.set, select = c("Age", "EstimatedSalary")))

training.set[, c("Age","EstimatedSalary")] <- 
  scale(subset(training.set, select = c("Age", "EstimatedSalary")))

####
# Fitting the SVM
####
library(e1071)

# For looping through the diffeernt type of possible kernels
# Checkin their performance
kernels <- c('polynomial', 'radial', 'sigmoid')
for (kernel in kernels){
  classifier <- svm(Purchased ~.,
                    data = training.set,
                    type = 'C-classification',
                    kernel = kernel)
  
  # Predicting the results
  y.pred <- predict(classifier, newdata = testing.set[,c("Age", "EstimatedSalary", "Gender")])
  
  # Make the confusion matrix
  confusion.matrix <- table(testing.set$Purchased, y.pred)
  print(kernel)
  print(measurePrecisionRecall(testing.set$Purchased, y.pred))
}


# Applying the k-fold
# We are breaking the dataset into 10 pieces and then using one of those for the testing set
# the other 9 would be used as the training set
library(caret)
folds <- createFolds(training.set$Purchased, k = 10)

# For each of the folds, we are going to make a training fold and testing fold
# we will be the Gaussian kernel, and then creating our performance metrics 
# thus, we will store the values of the accuracy into a vector
cv <- lapply(folds, function(x) {
  training.fold <- training.set[-x,]
  testing.fold <- training.set[x,]
  classifier <- svm(Purchased ~.,
                    data = training.fold,
                    type = 'C-classification',
                    kernel = 'radial')
  
  y.pred <- predict(classifier, newdata = testing.fold[,c("Age", "EstimatedSalary", "Gender")])
  confusion.matrix <- table(testing.fold$Purchased, y.pred)
  performance.metrics <- measurePrecisionRecall(y.pred, testing.fold$Purchased)
  accuracy <- performance.metrics[1]
  return(accuracy)
  
})
accuracy <- mean(as.numeric(cv)) #0.90625


# Changing the actors of our dependent variables to make it a classification model
training.set$Purchased <- as.factor(training.set$Purchased)

# Appyling Grid Search
# We are performing grid search to find the optimal parameter for the svmRadial!
classifier <- train(form = Purchased ~., data = training.set, method = 'svmRadial')
classifier # Optimal parameters: The final values used for the model were sigma = 0.5562473 and C = 1.

classifier$bestTune 

# Reminder: Cohen's Kappa? Kappa = (Observed Accuracy - Expected Accuracy) / (1 - Expected Accuracy)




