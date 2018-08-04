# KNN

# Importing the dataset
df <- read.csv("./archive/Social_Network_Ads.csv")
df <- subset(df, select=c("Gender", "Age", "EstimatedSalary", "Purchased"))

# Changing the categorical data of gender
df$Gender <- factor(df$Gender,
                    levels = c('Male','Female'),
                    labels = c(0,1))

# Splitting the dataset into training and testing
library(caTools)
set.seed(123)
split <- sample.split(df$Purchased, SplitRatio = 0.8)

training.set <- subset(df, split == TRUE)
testing.set <- subset(df, split == FALSE)

# Checking the datatype of the variables
sapply(testing.set, class)

# Gender is a factor, must change it to integer
training.set$Gender <- as.integer(as.character(training.set$Gender))
testing.set$Gender <- as.integer(as.character(testing.set$Gender))

# Feature Scaling
testing.set[, c("Age","EstimatedSalary")] <-  
  scale(subset(testing.set, select = c("Age", "EstimatedSalary")))

training.set[, c("Age","EstimatedSalary")] <- 
  scale(subset(training.set, select = c("Age", "EstimatedSalary")))


####
# Fitting the KNN Model and Predicting the results
####
library("class")
y.pred <- knn(train = training.set[,!colnames(training.set) == "Purchased"],
              test = testing.set[,!colnames(testing.set) == "Purchased"],
              cl = training.set[, "Purchased"], 
              k = 5)

# Make the confusion matrix
confusion.matrix <- table(testing.set$Purchased, y.pred)

####
# Visualizing the results
####

library(ElemStatLearn)
set.train = training.set[,c("Age", "EstimatedSalary", "Purchased")]
X1 = seq(min(set.train[, 1]) - 1, max(set.train[, 1]) + 1, by = 0.01)
X2 = seq(min(set.train[, 2]) - 1, max(set.train[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = knn(train = set.train[,!colnames(set.train) == "Purchased"],
             test = grid_set,
             cl = set.train[, "Purchased"], 
             k = 5)

plot(set.train[, -3],
     main = 'KNN (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set.train, pch = 21, bg = ifelse(set.train[, 3] == 1, 'green4', 'red3'))



### Comparing it to the testing set

set.test <- testing.set[,c("Age", "EstimatedSalary", "Purchased")]
X1 <- seq(min(set.test[, 1]) - 1, max(set.test[, 1]) + 1, by = 0.01)
X2 <- seq(min(set.test[, 2]) - 1, max(set.test[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')

y_grid <- knn(train = set.train[,!colnames(set.train) == "Purchased"],
             test = grid_set,
             cl = set.train[, "Purchased"], 
             k = 5)

plot(set.train[, -3],
     main = 'KNN (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set.test, pch = 21, bg = ifelse(set.test[, 3] == 1, 'green4', 'red3'))
