# Decision Tree Regressor

# Importing the dataset
df <- read.csv("./archive/Position_Salaries.csv")
df <- df[c('Level', 'Salary')]

# Creating the Decision Tree Regression
library(rpart)
regressor <- rpart(Salary ~., data=df)

# Predicting the results
y.pred <- predict(regressor, data.frame(Level=6.5)) # The model predicted of 249500


# Visualizing the Decision Tree Regression Plot
library(ggplot2)
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary),
             color = 'red') + 
  geom_line(aes(x = df$Level, y = predict(regressor, newdata = df)),
            color = 'blue') +
  ggtitle("Truth or Bluff (Decision Tree Regressor)") + 
  xlab('Level') +
  ylab('Salary')

# Noticed how the splits are not correct because of the number of splits
# We are not splitting the data enough to understand the data
# Moreover, this is not caused by distance because we are not using distance!


# Creating the Decision Tree Regression with splits
regressor <- rpart(Salary ~., data = df, control = rpart.control(minsplit = 1))

# Visualizing the Decision Tree Regression Plot
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary),
             color = 'red') + 
  geom_line(aes(x = df$Level, y = predict(regressor, newdata = df)),
            color = 'blue') +
  ggtitle("Truth or Bluff (Decision Tree Regressor)") + 
  xlab('Level') +
  ylab('Salary')

# This a red FLAG! The model is improved bc we are splitting. 
# But this cannot be the shape of a decision tree regressor
# The decison tree regressor is not continuous
# Thus, it makes it difficult to depict accurate decisions (this is shown by the non-horizontonal lines)


# Need to improve the resolution of the graph
x_grid <- seq(min(df$Level), max(df$Level), 0.01)
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary),
             color = 'red') + 
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            color = 'blue') +
  ggtitle("Truth or Bluff (Decision Tree Regressor)") + 
  xlab('Level') +
  ylab('Salary')

# Noticed that when we use smaller intervals, we get a better resolution of the graph

# This means that we split the data at the horizontal lines
# Thus, the horizontal lines is the predicted value that the model will predict for these X values
# Moreover, these are the averages of all the points in that split!
