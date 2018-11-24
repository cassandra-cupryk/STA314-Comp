#----------------------------------------------------------
# Take a look at Advertising data
# first try validation set approach
#----------------------------------------------------------

rm(list = ls())

library(caret)

# read in data
# setwd('C:/Users/stas/Desktop') # change to a different directory
res = read.csv(file = 'Advertising.csv') # read a file from the working directory
attach(res)

#----------------------------------------------------------
# load function for cross-validation
#----------------------------------------------------------

do_10cv_knn = function(X,Y,krange){
  n = length(Y) # smaple size

  # permute index set
  permidx = sample(1:n,n)
  X = as.matrix(X)[permidx,]
  Y = Y[permidx]

  # size of fold
  foldsize = floor(n/10)

  # for saving errors
  valset_error = array(0,length(krange))

  for(k in 1:length(krange)){
    K = krange[k]
    # the inner loop will be over the folds
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
     valset_error[k] = valset_error[k] + mean((Y[testidx] - pr)^2)
    } # end loop over folds
  } # end loop over k

  # the next line will output the result
  return(valset_error)
}

valset_knn = function(X,Y,krange){
  n = length(Y)
  l.tr = floor(n/2)
  l.val = n - l.tr
  train_index = sample(1:n,l.tr) # randomly sample l.tr points from 1,...,n
  # this will correspond to the training set

  # create array where results of validation error will be stored
  valset_error = array(0,length(krange))

  # loop over values of K, foer ach store error on validation set
  for(k in 1:length(krange)){
     # only use data with index in train_index to fit the model
     K = krange[k]
     fit = knnreg(as.matrix(X[train_index]),Y[train_index],k=K)

     # now use fitted model to predict data which are not in train_index
     # recall from lecture 0: a[-b] gives entris of a which are not indexed by b
     pr = predict(fit, newdata = data.frame(X = X[-train_index]))

     # compute and store the validation error
     valset_error[k] = mean((Y[-train_index] - pr)^2)
  }

  return(valset_error) # this will be the output of the function
}


#----------------------------------------------------------
# consider advertising data
#----------------------------------------------------------


# make results reproducible

set.seed(1)

# sample a test set
test_index = sample(1:200,40)

# run k-nn on the training set
# this includes selecting k via cross-validation

# first use TV as predictor
# select K
K_10CV_TV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
# fit model with selected K
fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_10CV_TV)
# predict data in training set
pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
# compute error
TV_error = mean((sales[test_index] - pr_TV)^2)
TV_error

# next do the same with radio as predictor
K_10CV_radio = which.min(do_10cv_knn(radio[-test_index],sales[-test_index],krange=1:100))
fit_radio = knnreg(as.matrix(radio[-test_index]),sales[-test_index],k = K_10CV_radio)
pr_radio = predict(fit_radio, newdata = data.frame(radio  = radio[test_index]))
radio_error = mean((sales[test_index] - pr_radio)^2)

# next do the same with News as predictor
K_10CV_News = which.min(do_10cv_knn(newspaper[-test_index],sales[-test_index],krange=1:100))
fit_News = knnreg(as.matrix(newspaper[-test_index]),sales[-test_index],k = K_10CV_News)
pr_News = predict(fit_News, newdata = data.frame(newspaper  = newspaper[test_index]))
News_error = mean((sales[test_index] - pr_News)^2)
News_error

# compare all of this with just using the mean
pr_mean = mean(sales[-test_index])
mean_error = mean((sales[test_index] - pr_mean)^2)

mean_error

#----------------------------------------------------------
# now repeat above procedure several times
#----------------------------------------------------------

set.seed(1)

repi = 25
TV_error = array(0,repi)
radio_error = array(0,repi)
News_error = array(0,repi)


for(r in 1:repi){

  test_index = sample(1:200,40)

  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selradio = which.min(do_10cv_knn(radio[-test_index],sales[-test_index],krange=1:100))
  K_selNews = which.min(do_10cv_knn(newspaper[-test_index],sales[-test_index],krange=1:100))

  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error[r] = mean((sales[test_index] - pr_TV)^2)

  fit_radio = knnreg(as.matrix(radio[-test_index]),sales[-test_index],k = K_selradio)
  pr_radio = predict(fit_radio, newdata = data.frame(radio  = radio[test_index]))
  radio_error[r] = mean((sales[test_index] - pr_radio)^2)

  fit_News = knnreg(as.matrix(newspaper[-test_index]),sales[-test_index],k = K_selNews)
  pr_News = predict(fit_News, newdata = data.frame(newspaper  = newspaper[test_index]))
  News_error[r] = mean((sales[test_index] - pr_News)^2)

  print(r)

}

boxplot(TV_error,radio_error,News_error,main = 'Test Error',names = c('TV','radio','newspaper'),ylim = c(0,max(News_error)))




#----------------------------------------------------------
# now do the same but with models including several predictors
#----------------------------------------------------------

set.seed(1)
# scaling for K-nn
#create dataframe that has radio, TV, newspaper
#Standardize it to have mean 0 and standard deviation
TvRa = scale(data.frame(radio,TV))
TvRaNe = scale(data.frame(radio,TV,newspaper))

repi = 25
TV_error = array(0,repi)
TvRa_error = array(0,repi)
TvRaNe_error = array(0,repi)

for(r in 1:repi){

  test_index = sample(1:200,40)

  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selTvRa = which.min(do_10cv_knn(TvRa[-test_index,],sales[-test_index],krange=1:100))
  K_selTvRaNe = which.min(do_10cv_knn(TvRaNe[-test_index,],sales[-test_index],krange=1:100))

  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error[r] = mean((sales[test_index] - pr_TV)^2)

  fit_TvRa = knnreg(TvRa[-test_index,],sales[-test_index],k = K_selTvRa)
  pr_TvRa = predict(fit_TvRa, newdata = TvRa[test_index,])
  TvRa_error[r] = mean((sales[test_index] - pr_TvRa)^2)

  fit_TvRaNe = knnreg(TvRaNe[-test_index,],sales[-test_index],k = K_selTvRaNe)
  pr_TvRaNe = predict(fit_TvRaNe, newdata = TvRaNe[test_index,])
  TvRaNe_error[r] = mean((sales[test_index] - pr_TvRaNe)^2)

  print(r)

}

boxplot(TV_error,TvRa_error,TvRaNe_error,main = 'Test Error',names = c('TV','TV+RA','TV+RA+NE'))

boxplot(TvRa_error,TvRaNe_error,names=c('Ta+RA','TV+Ra+News'))

#including all predictors may be worse.
#TV+Ra+News: made things worse
#median of 1.5
#does not reduce the bias, but could increase the variance


#------------------------------------------------------
# Now look at examples with increasing amount of noise
#------------------------------------------------------

set.seed(1)

# this vector contains ndifferent numbers of
# 'noise predictors' which are added to model
dvec = c(1,5,10,15,20)
l.d = length(dvec)

# initialize
error_knn = array(0,c(repi,l.d))

for(r in 1:repi){

  test_index = sample(1:200,40)

  # this 'inner loop' is over different amount of noise variables
  for(d in 1:l.d){
    # this matrix will contain additional noise variables
    NoiseMat = 200*matrix(runif(200*dvec[d]),ncol = dvec[d], nrow = 200)

    # this is a matrix with the new predictors
    # cbind creates a new matrix by binding columns together
    Pr_new = cbind(TV,radio,NoiseMat)

    # use scaled version for k-nn
    Pr_new_knn = scale(cbind(TV,radio,NoiseMat))

    # create a data frame that contains the values for Sales, Tv, Radio
    # and the new noise predictors
    daf = data.frame(sales, TV, radio, NoiseMat)

    # cross-validation for k-nn
    K_sel = which.min(do_10cv_knn(Pr_new_knn[-test_index,],sales[-test_index],krange=1:100))

    # computing error for k-nn
    fit = knnreg(Pr_new_knn[-test_index,],sales[-test_index],k = K_sel)
    pr = predict(fit, newdata = data.frame(Pr_new_knn  = Pr_new_knn[test_index,]))
    # save error for linear model
    error_knn[r,d] = mean((sales[test_index] - pr)^2)

  }

print(r)

}

# look at boxplot for k-nn
nam = dvec
boxplot(error_knn,names = nam,main = 'X-axis: number of noise variables')
