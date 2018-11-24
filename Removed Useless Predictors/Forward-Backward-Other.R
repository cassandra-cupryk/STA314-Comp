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

regfit.best = regsubsets(y~.,data = training,nvmax = 10)
summary(regfit.best)


# to extract coefficients from one particular model, 
# for example model with 4 predictors
coef(regfit.best,10)
#plot(regfit.best, scale="bic")


#-----------------------------------------------------------------------
# look at forward stepwise
regfit.forward = regsubsets(y~.,data = training,method='forward')
# performs forward selection
summary(regfit.forward)

# note: for example for k = 4 we get a different model compared
# to the result from best subset selection.
coef(regfit.forward,8)

#best number of predictors is 8
#X1, X2, X4, X5, X8, X12, X23, X25

#-----------------------------------------------------------------------
# look at backward stepwise
regfit.backward = regsubsets(y~.,data = training,method='backward')
# performs backward selection
summary(regfit.backward)

# note: we again get different models compared
# to forward stepwise and best subset

coef(regfit.backward,8)
#best number of predictors is 8
#X1, X2, X3, X4, X8, X12, X23, X25

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

#red dots relates to the model selected

# plot the results
# red points will indictae the number of predictors selected by each
# Cp, BIC, Adjusted R^2
par(mfrow = c(1,3))
plot(adjr2,type='l',xlab='number of predictors',ylab = 'Adjusted R^2')
points(adjr2)
points(which.max(adjr2),adjr2[which.max(adjr2)],col = 'red')
plot(cp,type='l',xlab='number of predictors',ylab = 'Cp')
points(cp)
points(which.min(cp),cp[which.min(cp)],col = 'red')
plot(bic,type='l',xlab='number of predictors',ylab = 'BIC')
points(bic)
points(which.min(bic),bic[which.min(bic)],col = 'red')

coef(regfit.best, 10)
#BIC agrees that the best number of predictors is 10
#X1, X2, X3, X4, X8, X12, X13, X23, X24, X25

 #Submission
 da.sample = data.frame(cbind(1:500,  prlmMultiUseful))
 names(da.sample) = c('id', 'y')
 write.csv(da.sample, file = "Submission.csv", row.names = FALSE)
 
 
 
 
 