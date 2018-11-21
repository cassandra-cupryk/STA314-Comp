# clear workspace
rm(list = ls())

library(caret) #for k-nn regression
library(class) #for k-nn regression
library(leaps) # this contains the function regsubsets
library(MASS)
library(ISLR)

# for additional details, see chapter 6.5 of the textbook

training = read.csv('/Users/cassie/Desktop/STA314-Comp/trainingdata.csv')
fix(training)
attach(training)


lm(y~.,data=training)

#-----------------------------------------------------------------------
# now look at different ways of selecting models
#Best subset selection WAS NOT RECOMMENDED
# first: best subset selection
regfit.best = regsubsets(y~.,data = training)
# performs exhaustive search
summary(regfit.best)

# note: by default only search up to 8 predictors

regfit.best = regsubsets(y~.,data = training,nvmax = 11)
summary(regfit.best)

# to extract coefficients from one particular model, 
# for example model with 4 predictors
coef(regfit.best,4)

#-----------------------------------------------------------------------
# look at forward stepwise
regfit.forward = regsubsets(y~.,data = training,method='forward')
# performs forward selection
summary(regfit.forward)

# note: for example for k = 4 we get a different model compared
# to the result from best subset selection.
coef(regfit.forward,4)
#X1, X12, X23, X25
coef(regfit.forward,5)
#X1, X8, X12, X23, X25

#-----------------------------------------------------------------------
# look at backward stepwise
regfit.backward = regsubsets(y~.,data = training,method='backward')
# performs backward selection
summary(regfit.backward)

# note: we again get different models compared
# to forward stepwise and best subset

coef(regfit.backward,4)
#X1, X12, X23, X25
coef(regfit.backward,5)
#X1, X8, X12, X23, X25

#-----------------------------------------------------------------------
# now look at methods using adjusted R^2, C_p, BIC 

# plot adjusted R^2, Cp, BIC against number of predictors
# use nvmax argument to set maximal number of variables considered
regfit.best = regsubsets(y~.,data = training,nvmax = 28)
# extract values for Cp, BIC, adjusted R^2 from the object regfit.best
adjr2 = summary(regfit.best)$adjr2
cp = summary(regfit.best)$cp
bic = summary(regfit.best)$bic

#selecting the models and compute
#want a bigger adjusted r^2, and smaller values of cp and bic
which.max(adjr2)
which.min(cp)
which.min(bic)




 #Submission - GOTTA ASK
 da.sample = data.frame(cbind(1:500,  prlmMultiUseful))
 names(da.sample) = c('id', 'y')
 write.csv(da.sample, file = "Submission.csv", row.names = FALSE)
 
 
 
 
 