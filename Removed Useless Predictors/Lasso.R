#install.packages('glmnet')
#install.packages('ISLR')
#install.packages('plotmo')

rm(list = ls())

setwd('/Users/cassie/Desktop/STA314-Comp/')

training = read.csv('/Users/cassie/Desktop/STA314-Comp/trainingdata.csv')
testing = read.csv('/Users/cassie/Desktop/STA314-Comp/test_predictors.csv')


#fix(training)
attach(training)
attach(testing)

library(plotmo) # nicer plotting of glmnet output
library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set 'Hitters'

#------------------------------------------------------------
x = model.matrix(y ~ . , data=training )
y = training$y

grid = 10^seq(10,-2,length = 100) 
# produce a grid of lambda values
# those lambda values will be used for  lasso and ridge

# glmnet with alpha = 1 is lasso
# note: by default, glmnet standardizes predictors
# we don't need to worry about doing that
lasso.mod = glmnet(x,y,alpha = 1, lambda = grid)
plot(lasso.mod, label = TRUE)
# note: plot is against L1 norm (sum of absolute values)
# of estimated b^L(lambda)

# nicer plot with external package
# the function glm_plot is from the library plotmo
plot_glmnet(lasso.mod)

# next look at some of the coefficients
coef(lasso.mod)[,100] # this is the version with small ampunt of penalization
coef(lasso.mod)[,80] # more penalization, some coefficients are set to zero
coef(lasso.mod)[,70] # even more penalization, more coefficients set to zero
coef(lasso.mod)[,1]


# ----------------------------------------------------
# next, we use validation set approach to see performance of different methods
# in terms of prediction on the Training data set

set.seed(1)
train = sample(1:nrow(x),nrow(x)) 
test = sample(1:nrow(testing), nrow(testing))
ytest = y[test]

# cross validation for ridge
cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso
plot(cv.la)
# first extract the lambda values obtained by cross-validation
best.la = cv.la$lambda.min
best.la

fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)
pred.la = predict(fit.la, s = best.la, newx = x[test,])
mean((pred.la-ytest)^2)  # error of lasso
length(pred.la)

# compare with results obtained from one standard error rule
#la.1se = cv.la$lambda.1se
#pred.la = predict(fit.la, s = la.1se, newx = x[test,])
#mean((pred.la-ytest)^2)  # error of lasso


#Submission
da.sample = data.frame(cbind(1:500,  pred.la))
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "Submission.csv", row.names = FALSE)

