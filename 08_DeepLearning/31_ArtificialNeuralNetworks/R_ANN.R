# Artificial Neural Network

# The goal of this project is to predict whether a customer will leave the bank

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
# Feature Scaling
######################################################

testing.set[ ,!(colnames(testing.set) == "Exited")] <-  
  scale(testing.set[ ,!(colnames(testing.set) == "Exited")])

training.set[ ,!(colnames(training.set) == "Exited")] <-  
  scale(training.set[ ,!(colnames(training.set) == "Exited")])

######################################################
# Fitting the ANN
######################################################
library(h2o)
h2o.init(nthreads = -1) # -1 will optimize the # of cores that will be used

# Hidden layers, use the avg of # of inputs + # of outcomes, 1+11=12/2=6

classifier <- h2o.deeplearning(y = 'Exited',
                               training_frame = as.h2o(training.set), 
                               activation = 'Rectifier',
                               hidden = c(6,6),
                               epochs = 100,
                               train_samples_per_iteration = -2)

# Predicting the results
prob.pred <- h2o.predict(classifier, 
                         newdata = as.h2o(testing.set[ ,!(colnames(testing.set) == "Exited")]))

# Creating the prob classes
y.pred <- (prob.pred > 0.5)

# Reformatting the predictions
y.pred <- as.vector(y.pred)

######################################################
# Creating the confusion matrixs (& other infos)
######################################################

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
  cat("F1 Score", f1.score)
}


# Make the confusion matrix
measurePrecisionRecall(testing.set$Exited, y.pred)
confusion.matrix <- table(testing.set$Exited, y.pred)


# Shut down the h2o connection
h2o.shutdown()
