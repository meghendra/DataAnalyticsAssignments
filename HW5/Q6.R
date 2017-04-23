library(xlsx)
library(DAAG)
library(boot)
library(caret)

#Read the data file into R
setwd("/Users/megh/Downloads/GoogleDrive/DAFall16/HW5/")
enb2012 <- read.xlsx("ENB2012_data.xlsx",sheetIndex = 1)
enb2012_data <- enb2012[1:768,]

#Part (a)
#Make a linear regression model object
linearModel <- lm(Y~X1+X2+X3+X4+X5+X6+X7, data=enb2012_data)

#Perform backward elimination using AIC
selectedVars <- step(linearModel, direction = "backward",trace = 1)

#Print coefficients for each step of elimination 
#based on output of backward elimination
lm(Y~X1+X2+X3+X4+X5+X6+X7, data=enb2012_data)
lm(Y~X1+X2+X3+X4+X6+X7, data=enb2012_data)
lm(Y~X1+X2+X4+X6+X7, data=enb2012_data)

#Make predictions the model using 5-fold cross validation
fitLinearRegression <- cv.lm(data = enb2012_data, selectedVars, m = 5)

#Convert model output to binary for creating confusion matrix 
#and computing accuracy.
fitLinearPredicted <- fitLinearRegression$cvpred > 0.5

#Part (b)
#Make a logistic regression model object
logisticModel <- glm(Y~X1+X2+X3+X4+X5+X6+X7, data=enb2012_data, 
                     family='binomial')

#Print coefficients for the logistic regression model
logisticModel$coefficients

#Make predictions using 5-fold cross validation
fitLogisticRegression <- cv.binary(logisticModel, nfolds = 5)

#Convert model output to binary for creating confusion matrix 
#and computing accuracy.
fitLogisticPredicted <- fitLogisticRegression$cvhat >= 0.5

#Part (c)
#Print confusion matrices for the linear and logistic 
#regression predictions
d<-table(enb2012_data$Y, fitLinearPredicted)
d
d1<-table(enb2012_data$Y, fitLogisticPredicted)
d1

#Print accuracies for the linear and logistic 
#regression predictions
sum(diag(d))/sum(d)
sum(diag(d1))/sum(d1)
