# Apriori 

# Data Preprocessing
# Loading the dataset
df <- read.csv('./archive/Market_Basket_Optimisation.csv', header = FALSE)

# In machine learning, there's a term called Sparacity
# which means, a lot of zeros

# 1 means that the value is true for that customer
# 0 means that the value is false for that customer

library(arules)

# Creating the sparse matrix (filled with 0 and 1)
# We cannot have any duplicates in a sparse matrix
df <- read.transactions('./archive/Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)

# df outputs the amount of duplicates (first row)
# df outputs the total occurences of the duplicates (second row)

summary(df)
# Info on the summary
# Density means that percentage of non-zero values (3% of our dataset is non-zero values)

# Creating a frequency plot for the top items (top 10)
itemFrequencyPlot(df, topN = 10)


#########
# Training Apriori in the dataset
#########

# There are a lot of products with a small support (low frequency in our data)
rules <- apriori(data = df, parameter = list(support = 0.003, confidence = 0.8))

# Estimating the support
# The argument is that we would want products that are purchased 3-4 times a day. 
# Since the dataset is for 7 days, we would to divide by the total transactions (7500)

# Estimating the confidence
# We are going to use the default value and then tweak it

# For the first rules, there's no rules because of the high confidence

# Changing the rules (281 rules were creating by changing the confidence)
rules <- apriori(data = df, parameter = list(support = 0.003, confidence = 0.8/2))

# Visualizing the results
inspect(sort(rules, by = 'lift')[1:10])

# Redoing the apriori bc we some values have high support which is making some of the rules irrelavant
rules <- apriori(data = df, parameter = list(support = 0.003, confidence = 0.8/4))

# Redoing the apriori by changing the support (using a desired of 4 transcation per day)
rules <- apriori(data = df, parameter = list(support = 0.004, confidence = 0.8/4))




