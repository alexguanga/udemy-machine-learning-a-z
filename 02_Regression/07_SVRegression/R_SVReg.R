# Support Vector Regression

# Importing the dataset
df <- read.csv("./archive/Position_Salaries.csv")
df <- df[c('Level', 'Salary')]

# Creating the SVR model
# eps is for regression
library(e1071)
regressor <- svm(Salary ~., data=df, type="eps-regression")

# Predicting the results
y.pred <- predict(regressor, data.frame(Level=6.5)) # The model predicted of 177861.1

# Visualizing the SVR Plot
library(ggplot2)
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary),
             color = 'red') + 
  geom_line(aes(x = df$Level, y = predict(regressor, newdata = df)),
            color = 'blue') +
  ggtitle("Truth or Bluff (SVRegression)") + 
  xlab('Level') +
  ylab('Salary')