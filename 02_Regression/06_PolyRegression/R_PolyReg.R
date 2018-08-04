# Polynomial Regression

# Importing the dataset
df <- read.csv("./Position_Salaries.csv")
df <- df[c('Level', 'Salary')]

# We do not need to split the data
#
# Splitting the dataset
# library(caTools)
# set.seed(123)
# split <- sample.split(df$Profit, SplitRatio = 0.8)
# 
# training_set <- subset(df, split == TRUE)
# testing_set <- subset(df, split == FALSE)


# Fitting the Poly and Linear Regression to the Training set
linreg.1 <- lm(Salary ~ ., data = df)
summary(linreg.1)

# Fitting the Poly Regression
df$Level2 <- df$Level^2
df$Level3 <- df$Level^3
polyreg <- lm(Salary ~ ., data = df)
summary(polyreg)

# Visualizing the plot
# linear regression
library(ggplot2)
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary),
             color = 'red') + 
  geom_line(aes(x = df$Level, y = predict(linreg.1, newdata = df)),
            color = 'blue') +
  ggtitle("Truth or Bluff (Linear Regression)") + 
  xlab('Level') +
  ylab('Salary')

# poly regression
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary),
             color = 'red') + 
  geom_line(aes(x = df$Level, y = predict(polyreg, newdata = df)),
            color = 'blue') +
  ggtitle("Truth or Bluff (Poly Regression)") + 
  xlab('Level') +
  ylab('Salary')


# Predicting info. with a linear regression
y.pred <- predict(linreg.1, data.frame(Level=6.5))

# Predicting info. with a poly regression
y.pred <- predict(polyreg, data.frame(Level=6.5,
                                      Level2=6.5^2,
                                      Level3=6.5^3))
### A good to create more prediction for a smooth line
x.grid <- seq(min(df$Level, max(df$Level)), 0.1)


