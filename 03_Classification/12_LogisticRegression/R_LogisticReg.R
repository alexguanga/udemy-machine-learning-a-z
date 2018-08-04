# Logistic Regression

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
training.set$Gender <- as.numeric(training.set$Gender)
testing.set$Gender <- as.numeric(testing.set$Gender)

  
# Feature Scaling
testing.set[, c("Age","EstimatedSalary")] <-  
  scale(subset(testing.set, select = c("Age", "EstimatedSalary")))

training.set[, c("Age","EstimatedSalary")] <- 
  scale(subset(training.set, select = c("Age", "EstimatedSalary")))


####
# Fitting the logistic regression
####
classifier <- glm(Purchased ~., family = binomial, data = training.set)

# Predicting the results
prob.pred <- predict(classifier, type = 'response', newdata = testing.set[ ,!(colnames(testing.set) == "Purchased")])
y.pred <- ifelse(prob.pred>0.5, 1, 0)

# Make the confusion matrix
confusion.matrix <- table(testing.set$Purchased, y.pred)


####
# Visualizing the results
####


library(ElemStatLearn)
set = training.set[,c("Age", "EstimatedSalary", "Purchased")]
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

# For visuals, I need to make this a 2-Variable Regression
classifier <- glm(Purchased ~., family = binomial, 
                  data = set)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


set = testing.set[,c("Age", "EstimatedSalary", "Purchased")]
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
  
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
  
  
  