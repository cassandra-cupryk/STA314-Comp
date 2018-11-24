library(leaps) # this contians the function regsubsets
library(MASS)
library(ISLR) # this contains Credit data set

rm(list = ls())

# load the credit data set
training = read.csv('/Users/cassie/Desktop/STA314-Comp/trainingdata.csv')
attach(training)

#-------------------------------------------------------
# next we look at cross-validation combined with best subset selection
#-------------------------------------------------------

# new function for predicting
# this function is written specifically to predict
# using the output from regsubsets
# regsubsets does not have predicton function 
predict.regsubsets = function(object, newdata, id) {
        coefi  <-  coef(object, id)
        xvars  <-  names(coefi)[-1]
        coefi[1] + as.matrix(newdata[,xvars]) %*% coefi[-1]
}

# new function for doing cross-validation
# dmax is maximal number of variables considered
# similar structure to do_10cv for k-nn, but with some differences

# input
# X: matrix with predictors
# Y: response
# dmax: maximal number of predictors considered
# this function will use best subset selection

do_10cv_bestsubset = function(X,Y,dmax){

  n = length(Y) # sample size

  # permute index set
  permidx = sample(1:n,n)

  # create data frame icluding predictors and reponse
  # permute to randomize for cross-validation
  daf = data.frame(Y,X)
  daf = daf[permidx,]

  # size of fold  
  foldsize = floor(n/10)
  
  # for saving errors
  # note: array, 
  # columns will corresond to folds (here: 10)
  # rows will correspond to numbers of predictors 
  valset_error = array(0,c(10,dmax))

  # outer loop is over the folds
  # this way around to speed things up
  # outer loop over d as in k-nn also possible but takes longer

  for(j in 1:10){

     #---------------------------
     # this is same as for k-nn
     # for fold 1-9 do same as last time
     if(j < 10){
       testidx = foldsize*(j-1) + (1:foldsize)
     # take care with last fold, it might be larger 
     # reason: n/10 might not be an integer 
     }else{
       testidx = (foldsize*(j-1)):n
     }
     #---------------------------
     
     regfit.best = regsubsets(Y~., data = daf[-testidx,], nvmax=dmax)

     # now do the loop over d
     # note: regsubsets needs to be called only once
     for(d in 1:dmax){ 
       # make predictions on test data
       pr = predict.regsubsets(regfit.best,newdata = daf[testidx,], id = d)
       # save predictions with model size d and j'th fold
       valset_error[j,d] = mean((daf$Y[testidx] - pr)^2)
     }
  }
  # note: this function returns the whole vector of errors
  # this will be useful later for 'one standard error rule'
  return(valset_error)
}




# -----------------------------------------------------
# now use functions above to do data analyis
# we use Credit data set

# this is useful for dealing with qualitative
# predictors. Those are automatically turned into dummies
# this is important for functions 
# that don't work with qualitative predictors directly
X = model.matrix(y~.,data = training)

fix(X)

# first column is intercept
# remove it
X = model.matrix(y~.,data = training)[,-1]
Y = y

set.seed(1)

cv_error = do_10cv_bestsubset(X,Y,dmax = 28)
which.min(colMeans(cv_error))
# cross-validation selects a model with 6 predictors
# take a look at the graph
plot(1:28, colMeans(cv_error),xlab = 'd', ylab = 'CV error')

# don't see too much, remove first couple of points
plot(7:28, colMeans(cv_error)[7:28],xlab = 'd', ylab = 'CV error')

#----------------------------------------------------------
# next: one standard error rule
# compute standard deviation for each value d
sdmat = apply(cv_error,2,sd)

# apply is a very useful function
# syntax:
# first argumentis an array of arbitrary dimension 
# apply the function specified in third argument
# keeping the dimensions of second argument fixed


cv = colMeans(cv_error) 
wimi = which.min(cv)
p = 28  # we have 28 predictors
# this vector contains TRUE in inequality
# from third step in description in lectures holds
cv < cv[wimi] + sdmat[wimi]/10^.5 

(1:p)[cv < cv[wimi] + sdmat[wimi]/10^.5]
# find smallest d for which above is true
cv_sd = min((1:p)[cv < cv[wimi] + sdmat[wimi]/10^.5])



#---------------------------------------------------------
# look at plot of CV error with 1 sd bars

# add and substract standard deviation
ub = colMeans(cv_error)+sdmat/10^.5
lb = colMeans(cv_error)-sdmat/10^.5

plot(7:28, ylim = c(min(lb[7:28]),max(ub[7:28])), colMeans(cv_error)[7:28],xlab = 'd', ylab = 'CV error')

for(j in 7:28){
  lines(c(j,j),c(lb[j],ub[j]),lwd=2)
}

#BEST NUMBER OF PREDICTORS IS 10




