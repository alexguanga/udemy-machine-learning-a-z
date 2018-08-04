# LDA - with SVM


# Importing the dataset
df <- read.csv("./archive/Wine.csv")

# Splitting the dataset into training and testing
library(caTools)
set.seed(123)
split <- sample.split(df$Customer_Segment, SplitRatio = 0.8)

training.set <- subset(df, split == TRUE)
testing.set <- subset(df, split == FALSE)

# Checking the datatype of the variables
sapply(testing.set, class)

# Feature Scaling
training.set[-14] <-  scale(training.set[-14])
testing.set[-14] <-  scale(testing.set[-14])

# Applying LDA's
library(MASS)

lda <- lda(formula = Customer_Segment ~., data = training.set)
training.set = as.data.frame(predict(object = lda, training.set))

# Fixing the order of the training/testing set
training.set <- training.set[c(5,6,1)]

# Redoing the predict/reordering for the testing set
testing.set = as.data.frame(predict(object = lda, testing.set))
testing.set <- testing.set[c(5,6,1)]


####
# Fitting the SVM
library(caret)
####
classifier <- svm(class ~.,
                  data = training.set,
                  type = 'C-classification',
                  kernel = 'linear')

# Predicting the results
y.pred <- predict(classifier, newdata = testing.set[ ,!(colnames(testing.set) == "class")])

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

# Confusion matrix
measurePrecisionRecall(testing.set$class, y.pred)
confusion.matrix <- table(testing.set$class, y.pred) # We have one incorrect decisions


####
# Visualizing the results
# For visuals, I need to make this a 2-Variable Regression
####

library(ElemStatLearn)
set = training.set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'SVM (Train set)',
     xlab = 'LD1', ylab = 'LD1',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1,'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))


### Comparing it to the testing set
set = testing.set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)


plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1,'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
