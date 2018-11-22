# clear workspace
rm(list = ls())


#install.packages('glmnet')
#install.packages('ISLR')
install.packages('pls')

library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set Hitters
library(pls)    # contains functions for fitting PLS and PCR

# this part is same as last lecture, preparing data
trainingdata = read.csv('/Users/cassie/Desktop/STA314-Comp/trainingdata.csv')
training = na.omit(trainingdata)
attach(training)


#Hitters = na.omit(Hitters) # remove missing values
#attach(Hitters)

y = training$y

set.seed(1)

train = sample(1:nrow(x),nrow(x)/2) 
test = (-train)


test = (-train)
ytest = y[test]
x = model.matrix(Salary ~ . ,Hitters )[,-1]  
# bring data in format that glmnet can deal with
# [,-1] is to remove intercept


set.seed(1)
# function pcr performs principal components regression
# subset = train means that only data from subset
# with that index are used for fitting
# scale = TRUE means that all predictors are scaled
# to have sample standard deviation 1
# validation = CV means that cross-validation is also run


pcr.fit = pcr(Salary~., data=Hitters ,subset = train ,scale =TRUE ,validation ="CV")

# to select number of components via CV by looking at graph
# ignore the row adjCV
validationplot(pcr.fit, val.type = "MSEP")
# also heplful:
summary(pcr.fit)

# to get best number of components automaticaslly run crossval function
# MSE (Mean Square Error)
MSEP(pcr.fit)$val[2,1,]
which.min(MSEP(pcr.fit)$val[2,1,]) - 1
# number of components is 7


# select number of components based on 1 standard error rule
selectNcomp(pcr.fit, method = "onesigma", plot = TRUE)
# Blue dotted line is produced by 1 standard error rule

# best number of components is 7 via stadard cv
# is 2 based on 1se for cv
# look at test error with this particular number of components 
pred.pcr = predict(pcr.fit ,x[test ,], ncomp= 7)
mean((pred.pcr-Salary[test])^2)  # estimated prediction error


#-----------------------------------------------------------------------
# next look at PLS
#-----------------------------------------------------------------------

set.seed(1)
# function plsr performs partial least squares regression
# substitue pcr with plsr
pls.fit = plsr(Salary~., data=Hitters ,subset = train ,scale =TRUE ,validation ="CV")
# to select number of components via CV, use following function
validationplot(pls.fit, val.type = "MSEP")

#select a smaller amount of directions
# adapt to data more

# best number of components is 2 in this case
# look at test error with this particular number of components 
pred.pls = predict(pls.fit ,x[test ,], ncomp= 2)

mean((pred.pls-Salary[test])^2)  # estimated prediction error
mean((pred.pcr-Salary[test])^2)  # estimated prediction error

# EXERCISE: adjust the code from PCR
# to get number of components selected by CV 
# for PLS automatically


#----------------------------------------------------------------------
# Next, repeat above a couple of times 
# without setting seed to see hw much results change
#----------------------------------------------------------------------


#----------------------

R = 100

t.ri = numeric(R)
t.la = numeric(R)
t.nu = numeric(R)
t.ls = numeric(R)
t.pcr = numeric(R)
t.pls = numeric(R)

set.seed(1)

for(r in 1:R){

train = sample(1:nrow(x),nrow(x)/2) 

#train = sample(1:nrow(x),70)

# random sample from numbers 1:nrow(x)
# this will correspond to the training observations
test = (-train)
ytest = y[test]

fit.ri = glmnet(x[train,],y[train],alpha =0)
fit.la = glmnet(x[train,],y[train],alpha =1)

pcr.fit = pcr(Salary~., data=Hitters ,subset = train ,scale = TRUE ,validation ="CV")
pls.fit = plsr(Salary~., data=Hitters ,subset = train ,scale = TRUE ,validation ="CV")

cv.ri = cv.glmnet(x[train,],y[train],alpha =0) # cross validation for ridge
cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso


best.ri = cv.ri$lambda.min  # best lambda value for ridge
best.la = cv.la$lambda.min  # best lambda value for lasso

best.pcr = which.min(MSEP(pcr.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PCR
best.pls = which.min(MSEP(pls.fit)$val[2,1,]) - 1

#best.pcr = wimi_dimred(pcr.fit,y[train]) # number of components for PCR
#best.pls = wimi_dimred(pcr.fit,y[train]) # number of components for PLS


pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
pred.la = predict(fit.la, s = best.la, newx = x[test,])
lim = lm(y[train]~., data = data.frame(x[train,]))
pred.ls = predict(lim, newdata = data.frame(x[test,]))
pred.pcr = predict(pcr.fit ,x[test ,], ncomp = best.pcr)
pred.pls = predict(pls.fit ,x[test ,], ncomp = best.pls)

t.ri[r] = mean((pred.ri-ytest)^2)  # error of ridge
t.la[r] = mean((pred.la-ytest)^2)  # error of lasso
t.nu[r] = mean((mean(y[train])-ytest)^2)
t.ls[r] = mean((pred.ls-ytest)^2)
t.pcr[r] = mean((pred.pcr-ytest)^2)
t.pls[r] = mean((pred.pls-ytest)^2)

print(r)

}

nam = c('ridge', 'lasso', 'PCR', 'PLS', 'full lm')
boxplot(t.ri,t.la,t.pcr,t.pls,t.ls,names =  nam)

#-----------------------------------------------------------------------

# now the same for smaller size of training set

R = 50

t.ri = numeric(R)
t.la = numeric(R)
t.nu = numeric(R)
t.ls = numeric(R)
t.pcr = numeric(R)
t.pls = numeric(R)

set.seed(1)

for(r in 1:R){

train = sample(1:nrow(x),70) 

#train = sample(1:nrow(x),70)

# random sample from numbers 1:nrow(x)
# this will correspond to the training observations
test = (-train)
ytest = y[test]

fit.ri = glmnet(x[train,],y[train],alpha =0)
fit.la = glmnet(x[train,],y[train],alpha =1)

pcr.fit = pcr(Salary~., data=Hitters ,subset = train ,scale = TRUE ,validation ="CV")
pls.fit = plsr(Salary~., data=Hitters ,subset = train ,scale = TRUE ,validation ="CV")

cv.ri = cv.glmnet(x[train,],y[train],alpha =0) # cross validation for ridge
cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso


best.ri = cv.ri$lambda.min  # best lambda value for ridge
best.la = cv.la$lambda.min  # best lambda value for lasso
best.pcr = which.min(MSEP(pcr.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PCR
best.pls = which.min(MSEP(pls.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PLS


pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
pred.la = predict(fit.la, s = best.la, newx = x[test,])
lim = lm(y[train]~., data = data.frame(x[train,]))
pred.ls = predict(lim, newdata = data.frame(x[test,]))
pred.pcr = predict(pcr.fit ,x[test ,], ncomp = best.pcr)
pred.pls = predict(pls.fit ,x[test ,], ncomp = best.pls)

t.ri[r] = mean((pred.ri-ytest)^2)  # error of ridge
t.la[r] = mean((pred.la-ytest)^2)  # error of lasso
t.nu[r] = mean((mean(y[train])-ytest)^2)
t.ls[r] = mean((pred.ls-ytest)^2)
t.pcr[r] = mean((pred.pcr-ytest)^2)
t.pls[r] = mean((pred.pls-ytest)^2)

print(r)

}

nam = c('ridge', 'lasso', 'PCR', 'PLS', 'full lm')
boxplot(t.ri,t.la,t.pcr,t.pls,t.ls,names =  nam)






























