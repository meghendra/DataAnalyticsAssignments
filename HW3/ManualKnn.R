setwd('Downloads/GoogleDrive/DAFall16/HW3/')

## utility functions used in the rest of the code
# normalize the data with min max
normalize <- function(x) {
  nx <- ((x - min(x)) / (max(x) - min(x)))
  return(nx)
}
# normalize the data with z-score
zScoreNormalize <- function(x) {
  nx <- (x - mean(x)) / sd(x)
  return(nx)
}
# find euclidean distance between test and training data
distance <- function(x, y) {
  dxy <- sqrt(sum(( x - y )^2))
  return(dxy)
}
# find k nearest neighbors by distance
getKnnByDistance <- function(dxy, k) {
  knnbd <- order(dxy) [1:k]
  return (knnbd)
} 
# find weights for the votes of k nearest neighbors by distance
getKnnWeightsByDistance <- function(dxy, k) {
  knnbd <- order(dxy) [1:k]
  knnwbd <- 1/(dxy[knnbd])^2
  return (knnwbd)
} 
# find the majority vote / weighted majority vote for each label
findVotes <- function(neighborIds, weightVector=rep(1, length(neighborIds))) {
  result <- matrix(0, nrow=2, ncol = 3)
  knnLabels <- train_labels[neighborIds,1]
  for (i in 1 : length(knnLabels))
  {
    if (knnLabels[i] == 0) {
      result[1,] <- c(0, (sum (1, result[1,2])) * weightVector[i], 0)
    }
    if (knnLabels[i] == 1) {
      result[2,] <- c(1, (sum (1, result[2,2])) * weightVector[i], 0)
    }
  }
  result[,3] <- result[,2] / sum(result[,2])
  return(result)
}

#load data 
train <- read.csv("train.csv")
test <- read.csv("test.csv")
head(train)
head(test)
# test.csv has an additional column ID which is useless hence discarded
## clean and normalize the data by min-max normalization
# train_n <- as.data.frame(lapply(train[,c(1,2,3)], normalize))
# test_n <- as.data.frame(lapply(test[,c(2,3,4)], normalize))
# train_labels <- as.data.frame(train[,c(4)])
# test_labels <- as.data.frame(test[,c(5)])

## OR
## normalize the data using z-score normalization
# train_n <- as.data.frame(lapply(train[,c(1,2,3)], zScoreNormalize))
# test_n <- as.data.frame(lapply(test[,c(2,3,4)], zScoreNormalize))
# train_labels <- as.data.frame(train[,c(4)])
# test_labels <- as.data.frame(test[,c(5)])

# OR
## dont normalize the data
train_n <- as.data.frame(train[,c(1,2,3)])
test_n <- as.data.frame(test[,c(2,3,4)])
train_labels <- as.data.frame(train[,c(4)])
test_labels <- as.data.frame(test[,c(5)])

## compute distance matrix d
d <- matrix(NA, nrow = nrow(test_n), ncol = nrow(train_n))
for (i in 1:nrow(test_n)) {
  d[i,] = apply(train_n[,1:3], 1, distance, x = test_n[i,1:3])
}
d <- t(d)
##############################################
## For unweighted kNN with k=3
## Compute ids of k-NN with k = 3
knns <- apply(d, 2, getKnnByDistance, k=3)
predictedLabelAggr <- vector()
predictedProbAggr <- vector()
for (i in 1:ncol(knns)){
  votes <- findVotes(knns[,i])
  predictedLabelAggr[i] <- votes[which.max(votes[,3]), 1]
  predictedProbAggr[i] <- votes[which.max(votes[,3]), 3]
}
result <- cbind(predictedLabelAggr, predictedProbAggr)
# write confusion matrix
table(predictedLabelAggr, test_labels[,1])

############################################### 
## For weighted k-NN with k = 3 and w=(1/d^2)
## determine the weightvectors of the kNNs
weightVectors <- apply(d, 2, getKnnWeightsByDistance, k=3)
predictedLabelAggr <- vector()
predictedProbAggr <- vector()
for (i in 1:ncol(knns)){
  votes <- findVotes(knns[,i], weightVector = weightVectors[,i])
  predictedLabelAggr[i] <- votes[which.max(votes[,3]), 1]
  predictedProbAggr[i] <- votes[which.max(votes[,3]), 3]
}
resultWeighted <- cbind(predictedLabelAggr, predictedProbAggr)
# write confusion matrix
table(predictedLabelAggr, test_labels[,1])
