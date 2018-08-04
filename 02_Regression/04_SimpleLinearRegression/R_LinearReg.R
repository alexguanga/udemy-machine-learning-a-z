# Importing the dataset
df <- read.csv("./Salary_Data.csv") # recall that the dataset is one level above the "Section 2" directory

library(caTools)
set.seed(123)
split <- sample.split(df$Salary, SplitRatio = 0.8)

training_set <- subset(df, split == TRUE)
testing_set <- subset(df, split == FALSE)

linearMod <- lm(Salary ~ YearsExperience, data=training_set)  # build linear regression model on full data
print(linearMod)

# More info on the model
summary(linearMod)

# Predicting the testing set
result <-  predict(linearMod, newdata=testing_set)

# Visualizing the graph
# the uniquye thing here in ggplot is that we need to add the features of the graph separetly
# point means scatter in python, line means plot in python

library(ggplot2)

ggplot() + 
  geom_point(aes(x=training_set$YearsExperience, y=training_set$Salary), 
             color='red') +
  geom_line(aes(x=training_set$YearsExperience, y=predict(linearMod, newdata=training_set)),
            color='blue') +
  ggtitle("Salary vs Years Experience (training set)") +
  xlab('Years of Experience') +
  ylab('Salary')

# the line does not matter bc at the of the day, the model is build and it will simply 
# get the point throughout the graph

ggplot() + 
  geom_point(aes(x=testing_set$YearsExperience, y=testing_set$Salary), 
             color='red') +
  geom_line(aes(x=training_set$YearsExperience, y=predict(linearMod, newdata=training_set)),
            color='blue') +
  ggtitle("Salary vs Years Experience (training set)") +
  xlab('Years of Experience') +
  ylab('Salary')


