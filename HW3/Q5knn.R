setwd('Downloads/GoogleDrive/DAFall16/HW3/')

#knn
train <- read.csv("train.csv")
test <- read.csv("test.csv")
head(train)
head(test)
normalize <- function(x) {
  return ( (x - min(x)) / (max(x) - min(x)))
}
train_n <- as.data.frame(lapply(train[,c(1,2,3)], normalize))
# test.csv has an additional column ID which is 
# useless hence discarded
test_n <- as.data.frame(lapply(test[,c(2,3,4)], normalize))
train_labels <- as.data.frame(train[,c(4)])
test_labels <- as.data.frame(test[,c(5)])
require(class)
m1 <- knn(train=train_n, test=test_n, cl=train_labels[,1], k=3, prob = TRUE)
m1
# plot the contingency table 
table(test_labels[,1],m1)
