# Multiple Linear Regression
df <- read.csv("./50_Startups.csv") # recall that the dataset is one level above the "Section 2" directory

# organize the categorical data
df$State <- factor(df$State, 
                   levels = c('New York','California','Florida'),
                   labels = c(1,2,3))

# Getting the columns names
colnames(df)

# Splitting the dataset
library(caTools)
set.seed(123)
split <- sample.split(df$Profit, SplitRatio = 0.8)

training_set <- subset(df, split == TRUE)
testing_set <- subset(df, split == FALSE)

# Fitting the Mult. Regression to the Training set
# linreg <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State )
# A better way to write it
linreg <- lm(Profit ~ ., data = training_set)

# Predicting the Test set
y_pred <- predict(linreg, newdata = testing_set)


# Backward Elimination Building
linreg <- lm(Profit ~ R.D.Spend + Adminstration + Marketing.Spend + State, data = dataset)
summary(linreg)

optimal.linreg <- step(linreg, direction = "backward", trace=FALSE )
summary(optimal.linreg)

# Another way of including a backward eliminaiton function
backwardElimination <- function(x, sl) {
  numVars = length(x)
  
  # A for loop that will eliminate one by one, pvals greater than 0.05
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    # Checks for the highest pvalue
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      # finds the value that has the largest pvalue
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      # removes it from the dataset
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
backwardElimination(training_set, SL)


