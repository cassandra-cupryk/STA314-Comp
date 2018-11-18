# clear workspace
rm(list = ls())

#load libraries
library(caret) #for k-nn regression
library(class) #for k-nn regression

 #getwd() # show current working directory. this is where on the computer
 # files are saved by default and where R looks ofr files
setwd('/Users/cassie/Desktop/STA314 - Competition/') # change to a different directory
# pick the right directory on your computer
 mydata = read.csv(file = 'trainingdata.csv') # read a file from the working directory

 fix(mydata) # take a look at data in table form
 names(mydata) # names of entries in res
 attach(mydata) # make elements of res available in workspace
 
 y = mydata$y # values of response variable
 

 #--------------------------------------------------------------
 # Data Analysis: loading external packages and using knn-regression
 #--------------------------------------------------------------
 # knnreg(x,y,k=K) uses the rows of the matrix or data frame x as predictors for
 # performing k-nn regression with response y. K is the number of neighbours used
 # note: first entry must be matrix or data frame
 # knnreg(TV,Sales,k=5) does not work

 knnfit = knnreg(as.matrix(X1),y,k=5) # this function does a pre-prossesing step
 # it returns an object of class knnreg
 # this object can be further processed by the function predict

 prKnn = predict(knnfit, newdata = data.frame(X1 = as.matrix(1:500)))

 # the input of the function predict:
 # first argument: an object from a function in R, here knnreg
 # second argument: the values of regressors for which we would like to compute
 # the prediction
 # the output are values of the k-nn regression function at values of newdata

 # in the example above, we compute the value of the knn regression function at the values
 # 1:300
 # so the output will be a vector of length 300
 length(prKnn)
 # now plot the results
 plot(X1, y) # first plot original data

 lines(1:500,prKnn, col='red', lwd = 2) # now add the values of the prediction function
 # lines(x,y) connects the dots (x_1,y_1),...,(x_n,y_n) by lines
 # the argument col='red' means that the line will be red
 # the argment lwd = 2 makes the line thiker

 # next let us reproduce the second plot from lectures
 # you should be able to understand the following code based on what we saw so far

 par(mfrow = c(2,2))

 fit5 = knnreg(as.matrix(X1),as.matrix(y),k=25)
 pr5 = predict(fit5,data.frame(sales=as.matrix(1:500)))

 fit25 = knnreg(as.matrix(X1),as.matrix(y),k=100)
 pr25 = predict(fit25,data.frame(sales=as.matrix(1:500)))

 fit50 = knnreg(as.matrix(X1),as.matrix(y),k=300)
 pr50 = predict(fit50,data.frame(sales=as.matrix(1:500)))

 fit100 = knnreg(as.matrix(X1),as.matrix(y),k=499)
 pr100 = predict(fit100,data.frame(sales=as.matrix(1:500)))


 plot(X1,y, main = 'K = 25')
 lines(1:500,pr5, lwd=2, col = 2)

 plot(X1,y, main = 'K = 100')
 lines(1:500,pr25, lwd=2, col = 2)

 plot(X1,y, main = 'K = 300')
 lines(1:500,pr50, lwd=2, col = 2)

 plot(X1,y, main = 'K = 499')
 lines(1:500,pr100, lwd=2, col = 2)

 #Multiple Linear Regression
 
 
 # note: we could (and should) also have removed X in the very beginning
 # one way to do that
 names(mydata)
 dim(mydata)
 mydataNew = mydata[,-1]
 names(mydataNew)
 
 lmMulti = lm(y ~ ., data = mydataNew)
 lmMulti$coefficients
 summary(lmMulti)
 prlmMulti = predict(lmMulti)
 
 
 #Removed predictors that have a p-value greater than 0.05
 lmMultiUseful = lm(y ~ X1+X2+X3+X4+X8+X12+X13+X23+X24+X25, data = mydataNew)
 summary(lmMultiUseful)
 prlmMultiUseful = predict(lmMultiUseful)
 
 
 #Submission - GOTTA ASK
 da.sample = data.frame(cbind(1:500, y.pr))
 names(da.sample) = c('id', 'y')
 write.csv(da.sample, file = "Submission.csv", row.names = FALSE)
 
 
 
 
 