# Eclat 

# Data Preprocessing
# Loading the dataset
# Creating the sparse matrix (filled with 0 and 1)

library(arules)
df <- read.csv('./archive/Market_Basket_Optimisation.csv', header = FALSE)
df <- read.transactions('./archive/Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
# df outputs the amount of duplicates (first row)
# df outputs the total occurences of the duplicates (second row)

# Density means that percentage of non-zero values (3% of our dataset is non-zero values)
summary(df)

# Creating a frequency plot for the top items (top 10)
itemFrequencyPlot(df, topN = 10)


#########
# Training Eclat in the dataset
#########

# There are a lot of products with a small support (low frequency in our data)
# Eclat: Uses # of sets, Aprior: Uses # of rules
rules <- eclat(data = df, parameter = list(support = 0.003, minlen=2))

# Estimating the support: We want products that were purchased 3-4 times a day. 
# The dataset is for 7 days, thus, we divide by the weekly transactions (7500)

# Visualizing the results
inspect(sort(rules, by = 'support')[1:10])



