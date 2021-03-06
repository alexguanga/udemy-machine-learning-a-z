# Decision Trees

# Importing the dataset
df <- read.csv("./archive/Social_Network_Ads.csv")
df <- subset(df, select=c("Gender", "Age", "EstimatedSalary", "Purchased"))

# Changing the categorical data of gender
df$Gender <- factor(df$Gender,
                    levels = c('Male','Female'),
                    labels = c(0,1))

# Encoding the target feature as a factor
df$Purchased <- factor(df$Purchased, levels = c(0,1))

# Splitting the dataset into training and testing
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


# Feature Scaling
testing.set[, c("Age","EstimatedSalary")] <-  
  scale(subset(testing.set, select = c("Age", "EstimatedSalary")))

training.set[, c("Age","EstimatedSalary")] <- 
  scale(subset(training.set, select = c("Age", "EstimatedSalary")))


####
# Fitting the Decision Trees
####
library(rpart)
classifier <- rpart(formula = Purchased ~., 
                    data = training.set,
                    method = "class")

# Predicting the results
y.pred <- predict(classifier, 
                  newdata = testing.set[ ,!(colnames(testing.set) == "Purchased")],
                  type = "class")

# Make the confusion matrix
confusion.matrix <- table(testing.set$Purchased, y.pred)


####
# Visualizing the results
# For visuals, I need to make this a 2-Variable Regression
####

library(ElemStatLearn)
set = training.set[,c("Age", "EstimatedSalary", "Purchased")]
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
classifier <- rpart(Purchased ~., 
                    data = set,
                    method = "class") 

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, 
                  newdata = grid_set, 
                  type = "class")

plot(set[, -3],
     main = 'Decision Trees (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


### Creating the testing set
set = testing.set[,c("Age", "EstimatedSalary", "Purchased")]
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, 
                  newdata = grid_set, 
                  type = "class")

plot(set[, -3],
     main = 'Decision Trees (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# Plotting the decision tree
plot(classifier)
text(classifier, cex=0.5)
