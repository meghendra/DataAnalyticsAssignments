train_orig <- read.csv('train.csv')
test_orig <- read.csv('test.csv')

## useful functions

#normalize <- function(x) (x-mean(x))/sd(x)
normalize <- function(x) (x-min(x))/(max(x) - min(x))

eu_dist <- function(x, y) sqrt(sum((x-y)^2))
neighbors <- function(x, k) order(x)[1:k]
predict.class <- function(neighbor_id, weight=rep(1, length(neighbor_id)), class=c(0,1)){
  classes <- train[neighbor_id,4]
  a <- matrix(NA, nrow=length(class), ncol = 3)
  colnames(a) <- c('class', 'freq', 'prob')
  for (i in 1:length(class)){
    a[i,] <- c(class[i], (sum(classes==class[i]))*weight[i], 0)
  }
  a[,3] <- a[,2]/sum(a[,2])
  return(a)
}

## normalize the data

train <- cbind(apply(train_orig[,1:3], 2, normalize), train_orig[,4])
test <- cbind(apply(test_orig[,2:4], 2, normalize), test_orig[,5])
## compute distance matrix

distMat <- matrix(NA, nrow = nrow(test), ncol = nrow(train))
for (i in 1:nrow(test)){
  distMat[i,] = apply(train[,1:3], 1, eu_dist, x = test[i,1:3])
}
distMat <- t(distMat)


## 3-NN unweighted
neighbors_3 <- apply(distMat, 2, neighbors, k=3)
prediction_3.class <- vector()
prediction_3.prob <- vector()

for (i in 1:nrow(test)){
  aa <- predict.class(neighbors_3[,i])
  prediction_3.class[i] <- aa[which.max(aa[,3]), 1]
  prediction_3.prob[i] <- aa[which.max(aa[,3]), 3]
}
prediction_3_uw <- cbind(prediction_3.class, prediction_3.prob)


## 3-NN weighted
prediction_3.class <- vector()
prediction_3.prob <- vector()

for (i in 1:nrow(test)){
  weight_vec <- 1/distMat[neighbors_3[,i], i]^2
  aa <- predict.class(neighbors_3[,i], weight=weight_vec)
  prediction_3.class[i] <- aa[which.max(aa[,3]), 1]
  prediction_3.prob[i] <- aa[which.max(aa[,3]), 3]
}
prediction_3_w <- cbind(prediction_3.class, prediction_3.prob)