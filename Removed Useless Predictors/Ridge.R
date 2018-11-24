#install.packages('glmnet')
#install.packages('ISLR')
#install.packages('plotmo')

rm(list = ls())

setwd('/Users/cassie/Desktop/STA314-Comp/')

training = read.csv('/Users/cassie/Desktop/STA314-Comp/trainingdata.csv')
testing = read.csv('/Users/cassie/Desktop/STA314-Comp/test_predictors.csv')

attach(training)
attach(testing)

library(plotmo) # nicer plotting of glmnet output
library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set 'Hitters'

#attach(Hitters) # works since we loaded the package ISLR

#------------------------------------------------------------
# this part is from lecture 9
# note: some values of slary are NA, ie unknown
#Hitters = na.omit(Hitters) # remove missing values
#x
x = model.matrix(y ~ . ,data=training )
#x
# bring data in format that glmnet can deal with
# [,-1] is to remove intercept
# same as lecture 8

y = training$y

grid = 10^seq(10,-2,length = 100) 
# produce a grid of lambda values
# those lambda values will be used for  lasso and ridge


# ----------------------------------------------------
# now look at ridge
# glmnet with alpha = 0 is ridge regression

ridge.mod = glmnet(x,y,alpha = 0, lambda =grid) # alpha = 0 is ridge regression
dim(coef(ridge.mod))


# next, we use validation set approach to see performance of different methods
# in terms of prediction on the Hitters data set

set.seed(1)

# random sample from numbers 1:nrow(x)
# this will correspond to the training observations
#train = sample(1:nrow(x),nrow(x)/2) 
#test = (-train)
#test
#ytest = y[test]
#ytest

train = sample(1:nrow(x),nrow(x)) 
test = sample(1:nrow(testing), nrow(testing))
test
ytest = y[test]
ytest


# cross validation for ridge
cv.ri = cv.glmnet(x[train,],y[train],alpha =0)
plot(cv.ri)
#names(cv.ri)
best.ri = cv.ri$lambda.min
best.ri
fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)

#train = sample(1:nrow(x),nrow(x)) 
#fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)
pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
mean((pred.ri-ytest)^2)  # error of ridge

length(pred.ri)

#Might Need to Resubmit.. Not sure yet.. 
out = glmnet(x, y, alpha =0)
final_ridge.pr = predict(out, type= "coefficients", s= best.ri)[1:20,]
mean((final_ridge.pr-ytest)^2)  # error of ridge
length(final_ridge.pr)


#RIDGE IS BETTER THAN LASSO

#Resubmission
da.sample = data.frame(cbind(1:500,  pred.ri))
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "Submission.csv", row.names = FALSE)


