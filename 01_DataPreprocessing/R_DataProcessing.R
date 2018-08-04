##################
# Lecture 13: Data Preprocessing
##################

# Importing the dataset
df <- read.csv("../Data.csv") # recall that the dataset is one level above the "Section 2" directory

# Using mean to fill NaN Values, 
df$Age <- ifelse(is.na(df$Age), ave(df$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                 df$Age)
df$Salary <- ifelse(is.na(df$Salary), ave(df$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                 df$Salary)

##################
# Lecture 14: Categorical Data
##################
df$Country <- factor(df$Country,
                     levels = c('Spain','France','Germany'),
                     labels = c(1,2,3))

df$Purchased <- factor(df$Purchased,
                     levels = c('Yes','No'),
                     labels = c(1,0))
##################
# Lecture 15: Splitting the Dataset into the Training Set and Test Set
##################
# install.packages('caTools')

library(caTools)
set.seed(123)
split <- sample.split(df$Purchased, SplitRatio = 0.8)

training_set <- subset(df, split == TRUE)
testing_set <- subset(df, split == FALSE)


##################
# Lecture 16: Feature Scaling
##################

# Notice how the process is much quicker in R
# Also, the sets are divided in two sets rather than 4 (splitting the y from Xs)
# Unlike in Python, we are going to include the categorical columns or else
# the scaling will not work

training_set[, 2:3] <- scale(training_set[, 2:3])
training_set[, 2:3] <- scale(training_set[, 2:3])

# Shortcut to comment multi-line: command+shift+c


