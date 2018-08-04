# Natural Language Processing

# Importing the dataset
df.original <- read.delim("./archive/Restaurant_Reviews.tsv", quote = '', stringsAsFactors = FALSE)

# Cleaning the texts
library(tm)
library(SnowballC)

###-----------------------------------------------------------------
# Preprocessing the text
###-----------------------------------------------------------------

corpus <- VCorpus(VectorSource(df.original$Review))

# Changing all the review to lowercase (avoding idential words)
corpus <- tm_map(corpus, content_transformer(tolower))

# Removing numbers that are in the dataset
# However, I do not agree to delete them (they might provide some value, like expensive or not)
corpus <- tm_map(corpus, removeNumbers)

# Removing any punctuations
corpus <- tm_map(corpus, removePunctuation)

# Removing non-relevants words (eg. like, this, that)
corpus <- tm_map(corpus, removeWords, stopwords())

# Performing "stemming", to get the root of the word
# loved, love, loving will have the same root
corpus <- tm_map(corpus, stemDocument)

# Removing any extra spaces
corpus <- tm_map(corpus, stripWhitespace)


###-----------------------------------------------------------------
# Creating our sparse matrix
###-----------------------------------------------------------------
dtm <- DocumentTermMatrix(corpus)

# Extracting the most frequent words
# Words that appear once will not be included in the sparse martix (not enough info)

# Sparsity is 100%, we should be able to decrease it using removing irrelavant words!
dtm <- removeSparseTerms(dtm, 0.999) # Keeping 99.9% of the most frequent words!

df <- as.data.frame(as.matrix(dtm))
df$Liked <- df.original$Liked


# Function to extract important metrics
measurePrecisionRecall <- function(actual_labels, predict){
  confusion.matrix <- table(actual_labels, predict)
  tn <- table(actual_labels, predict)[1]
  fn <- table(actual_labels, predict)[2]
  fp <- table(actual_labels, predict)[3]
  tp <- table(actual_labels, predict)[4]
  total <- tn+fn+fp+tp
  
  accuracy <- (tn+tp)/total
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  f1.score <- 2*precision*recall/(precision+recall)
  
  cat("Accuracy", accuracy, '\n')
  cat("Precision", precision, '\n')
  cat("Recall", recall, '\n')
  cat("F1 Score", f1.score)
}



###-----------------------------------------------------------------
# Preprocessing the dataset
###-----------------------------------------------------------------


# Encoding the target feature as a factor
df$Liked <- factor(df$Liked, levels = c(0,1))

######################################################
# Splitting the dataset into training and testing
######################################################

library(caTools)
set.seed(123)
split <- sample.split(df$Liked, SplitRatio = 0.8)
training.set <- subset(df, split == TRUE)
testing.set <- subset(df, split == FALSE)


###-----------------------------------------------------------------
# Random Forest
###-----------------------------------------------------------------

library(randomForest)
classifier <- randomForest(x = training.set[ ,!(colnames(testing.set) == "Liked")],
                           y = training.set$Liked,
                           ntree = 10)

# Predicting the results
y.pred <- predict(classifier, 
                  newdata = testing.set[ ,!(colnames(testing.set) == "Liked")],
                  type = "class")

# Make the confusion matrix
confusion.matrix <- table(testing.set$Liked, y.pred)
measurePrecisionRecall(y.pred, testing.set$Liked)

# Accuracy 0.745 
# Precision 0.7692308 
# Recall 0.7 
# F1 Score 0.7329843

###-----------------------------------------------------------------
# Naive Bayes
###-----------------------------------------------------------------

library(e1071)
classifier <- naiveBayes(x = training.set[, !(colnames(training.set) == "Liked")],
                         y = training.set[, (colnames(training.set) == "Liked")])

# Predicting the results
y.pred <- predict(classifier, newdata = testing.set[,!(colnames(testing.set) == "Liked")])

# Make the confusion matrix
confusion.matrix <- table(testing.set$Liked, y.pred)
measurePrecisionRecall(y.pred, testing.set$Liked)

# Accuracy 0.505 
# Precision 0.5026178 
# Recall 0.96 
# F1 Score 0.6597938


###-----------------------------------------------------------------
# SVM
###-----------------------------------------------------------------

classifier <- svm(Liked ~.,
                  data = training.set,
                  type = 'C-classification',
                  kernel = 'linear')

# Predicting the results
y.pred <- predict(classifier, newdata = testing.set[,!(colnames(testing.set) == "Liked")])

# Make the confusion matrix
confusion.matrix <- table(testing.set$Liked, y.pred)
measurePrecisionRecall(y.pred, testing.set$Liked)

# Accuracy 0.795 
# Precision 0.7864078 
# Recall 0.81 
# F1 Score 0.7980296


###-----------------------------------------------------------------
# Kernel SVM
###-----------------------------------------------------------------

kernels <- c('polynomial', 'radial', 'sigmoid')
for (kernel in kernels){
  classifier <- svm(Liked ~.,
                    data = training.set,
                    type = 'C-classification',
                    kernel = kernel,
                    scale = FALSE)
  
  # Predicting the results
  y.pred <- predict(classifier, newdata = testing.set[, !(colnames(testing.set) == "Liked")])
  
  # Make the confusion matrix
  confusion.matrix <- table(testing.set$Liked, y.pred)
  print(kernel)
  measurePrecisionRecall(y.pred, testing.set$Liked)
}

# Models are barely over 50%


###-----------------------------------------------------------------
# Logistic Regression
###-----------------------------------------------------------------

classifier <- glm(Liked ~., family = binomial, data = training.set)

# Predicting the results
prob.pred <- predict(classifier, 
                     type = 'response', 
                     newdata = testing.set[ ,!(colnames(testing.set) == "Liked")])

y.pred <- ifelse(prob.pred>0.5, 1, 0)

# Make the confusion matrix
confusion.matrix <- table(testing.set$Liked, y.pred)
measurePrecisionRecall(y.pred, testing.set$Liked)

# Accuracy 0.55 
# Precision 0.5446429 
# Recall 0.61 
# F1 Score 0.5754717


###-----------------------------------------------------------------
# KNN
###-----------------------------------------------------------------

library("class")
y.pred <- knn(train = training.set[,!colnames(training.set) == "Liked"],
              test = testing.set[,!colnames(testing.set) == "Liked"],
              cl = training.set[, "Liked"], 
              k = 5)

# Make the confusion matrix
confusion.matrix <- table(testing.set$Liked, y.pred)
measurePrecisionRecall(y.pred, testing.set$Liked)

# Accuracy 0.65 
# Precision 0.6829268 
# Recall 0.56 
# F1 Score 0.6153846


###-----------------------------------------------------------------
# Decision Trees
###-----------------------------------------------------------------

library(rpart)
classifier <- rpart(formula = Liked ~., 
                    data = training.set,
                    method = "class")

# Predicting the results
y.pred <- predict(classifier, 
                  newdata = testing.set[ ,!(colnames(testing.set) == "Liked")],
                  type = "class")

# Make the confusion matrix
confusion.matrix <- table(testing.set$Liked, y.pred)
measurePrecisionRecall(y.pred, testing.set$Liked)

# Accuracy 0.71 
# Precision 0.7916667 
# Recall 0.57 
# F1 Score 0.6627907


### Models to Decide On
# Decision Trees, Support Vector Machines (linear kernel), Random Forest



