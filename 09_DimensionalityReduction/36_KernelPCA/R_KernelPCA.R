# Kernel PCA

# Importing the dataset
df <- read.csv("./archive/Social_Network_Ads.csv")
df <- df[,c("Age", "EstimatedSalary", "Purchased")]

# Splitting the dataset into training and testing
library(caTools)
set.seed(123)
split <- sample.split(df$Purchased, SplitRatio = 0.8)

training.set <- subset(df, split == TRUE)
testing.set <- subset(df, split == FALSE)

# Checking the datatype of the variables
sapply(testing.set, class)

# Feature Scaling
testing.set[, c("Age","EstimatedSalary")] <-  
  scale(subset(testing.set, select = c("Age", "EstimatedSalary")))

training.set[, c("Age","EstimatedSalary")] <- 
  scale(subset(training.set, select = c("Age", "EstimatedSalary")))


# Applying Kernel PCA
library(kernlab)
kpca <- kpca(~., data = training.set[ ,!(colnames(testing.set) == "Purchased")], kernel = 'rbfdot', 
             features = 2)
training.set.pca <- as.data.frame(predict(kpca, training.set))

# adding the depedent variable back to the kernel pca
training.set.pca$Purchased <- training.set$Purchased

# Performing the same steps for the testing set
testing.set.pca <- as.data.frame(predict(kpca, testing.set))
testing.set.pca$Purchased <- testing.set$Purchased


####
# Fitting the logistic regression
####
classifier <- glm(Purchased ~., family = binomial, data = training.set.pca)

# Predicting the results
prob.pred <- predict(classifier, type = 'response', newdata = testing.set.pca[ ,!(colnames(testing.set.pca) == "Purchased")])
y.pred <- ifelse(prob.pred>0.5, 1, 0)

# Make the confusion matrix
confusion.matrix <- table(testing.set.pca$Purchased, y.pred)


####
# Visualizing the results
####


library(ElemStatLearn)
set = training.set.pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# Testing results
set = testing.set.pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


