# Random Forest Regressor

# Importing the dataset
df <- read.csv("./archive/Position_Salaries.csv")
df <- df[c('Level', 'Salary')]

# Creating the Decision Tree Regression
set.seed(1234)
library(randomForest)
regressor <- randomForest(x = subset(df, select=c("Level")), # This provides a dataframe
                          y = df$Salary, 
                          ntree = 500) # This provides a vector

# Predicting the results
y.pred <- predict(regressor, data.frame(Level=6.5)) 
# The model prediction is 141733.3 for 10 trees
# The model prediction is 163545 for 100 trees
# The model prediction is 160457.7 for 500 trees


# Just like the decision tree, we need to increase the resolution to have a more accurate 
# visual of how the random forest is making its predictions

# Need to improve the resolution of the graph
library(ggplot2)
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

# Random Forest is splitting the data into more subsets
# Hence, we have smaller intervals
