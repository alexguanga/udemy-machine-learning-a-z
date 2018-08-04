# K-Means Clustering

# Importing the dataset
df <- read.csv('./archive/Mall_Customers.csv')

# Using the values 
Xs <- subset(df, select=c("Annual.Income..k..", "Spending.Score..1.100."))

# Implementing the elbow method to find the optimal number of clusters
set.seed(6)
wcss <- vector() # creating a vector (will store the within clusters sum of squares)

for (i in 1:20) wcss[i] <- sum(kmeans(Xs, i)$withinss)

plot(1:20, wcss, type = 'b', main = paste('Clusters of Clients'), xlab = '# of Clusters', ylab = 'WCSS')

# The optimal number of clusters if 5!

# Applying kmeans to the mall dataset
set.seed(29)
kmeans <- kmeans(Xs, 5, iter.max = 300, nstart = 20)

library(cluster)
clusplot(Xs, 
         kmeans$cluster, 
         lines=0,
         shade = TRUE,
         color = TRUE, 
         labels = 2,
         plotchar = FALSE, 
         span = TRUE, 
         main = paste('Clusters of Clients'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')