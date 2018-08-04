# Hierachical Clustering

# Importing the dataset
df <- read.csv('./archive/Mall_Customers.csv')

# Using the values 
Xs <- subset(df, select=c("Annual.Income..k..", "Spending.Score..1.100."))

# Implementing the elbow method to find the optimal number of clusters
set.seed(6)

# Using the dendogram to find the optimal number of clusters
dendogram <- hclust(dist(Xs, method = 'euclidean'), method = 'ward.D')

# Visualizing the clusters
plot(dendogram, 
     main = paste('Dendogram'),
     xlab = 'Customers', 
     ylab = 'Euclidean Customers')

# Fitting the dendogram to the mall dataset
hc <- hclust(dist(Xs, method = 'euclidean'), method = 'ward.D')
y.hc <- cutree(hc, k = 5)

# Visualizing the clusters
library(cluster)
clusplot(Xs, 
         y.hc, 
         lines=0,
         shade = TRUE,
         color = TRUE, 
         labels = 2,
         plotchar = FALSE, 
         span = TRUE, 
         main = paste('Clusters of Clients'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


