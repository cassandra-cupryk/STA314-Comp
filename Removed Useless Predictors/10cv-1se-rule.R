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
library(class)
library(caret)

x = model.matrix(y ~ . , data=training )
y = training$y

do_10cv_1se_knn = function(X,Y,krange){
  n = length(Y) # smaple size

  # permute index set
  permidx = sample(1:n,n)
  X = as.matrix(X)[permidx,]
  Y = Y[permidx]

  # size of fold
  foldsize = floor(n/10)

  # for saving errors
  cv_error = array(0,length(krange))
  # for saving standard errors
  cv_se = array(0,length(krange))

  for(k in 1:length(krange)){
    K = krange[k]
    # the inner loop will be over the folds

    fold_err = array(0,10) # will contain error from each fold
    # used for 1 se rule

    for(j in 1:10){
     # for fold 1-9 do same as last time
     if(j < 10){
       testidx = foldsize*(j-1) + (1:foldsize)
     # take care with last fold, it might be larger
     # reason: n/10 might not be an integer
     }else{
       testidx = (foldsize*(j-1)):n
     }

     fit = knnreg(as.matrix(X)[-testidx,,drop=FALSE],Y[-testidx],k=K)
     pr = predict(fit, newdata = as.matrix(X)[testidx,,drop=FALSE])
     fold_err[j] = mean((Y[testidx] - pr)^2)

    } # end loop over folds
  cv_error[k] = mean(fold_err)
  cv_se[k] = sd(fold_err)
  } # end loop over k

  # create a list with two elements
  res_list = list(cv_error,cv_se)

  # give names to the elements in list
  names(res_list) = c('cv_error','cv_se')

  # the next line will outout the result
  return(res_list)
}


re = do_10cv_1se_knn(x,y,1:50)
re

plot(re$cv_error)
for(k in 1:50){
  lines(c(k,k),c(re$cv_error[k]-re$cv_se[k]*10^{-1/2},re$cv_error[k]+re$cv_se[k]*10^{-1/2}))
}

abline(a = min(re$cv_error) + re$cv_se[which.min(re$cv_error)]*10^(-1/2), b = 0, col='red')

mi.err = min(re$cv_error)
# the following line needs to be corected
# this is the old version
max((1:50)[which(re$cv_error < mi.err + re$cv_se*10^{-1/2})])

# the following two lines give the correct version
mi.sd = re$cv_se[which.min(re$cv_error)]
max((1:50)[which(re$cv_error < mi.err + mi.sd*10^{-1/2})])