# clear workspace
rm(list = ls())

#load libraries
library(caret) #for k-nn regression
library(class) #for k-nn regression

 #getwd() # show current working directory. this is where on the computer
 # files are saved by default and where R looks ofr files
setwd('/Users/cassie/Desktop/STA314-Comp/') # change to a different directory
# pick the right directory on your computer
 mydataNew = read.csv(file = 'trainingdata.csv') # read a file from the working directory

 fix(mydataNew) # take a look at data in table form
 names(mydataNew) # names of entries in res
 attach(mydataNew) # make elements of res available in workspace
 
 #y = mydata$y # values of response variable
 
 #Multiple Linear Regression
 # note: we could (and should) also have removed X in the very beginning
 # one way to do that
 
 
 #DO YOU NEED TO DO THIS???
 #names(mydata)
 #dim(mydata)
 #mydataNew = mydata[,-1]
 #names(mydataNew)
 
 #General MultiLinear Regression
 lmMulti = lm(y ~ ., data = mydataNew)
 prlmMulti = predict(lmMulti)
 lmMulti$coefficients
 summary(lmMulti)
 #Residual Standard Error = 0.8254

 
 #Removed predictors that have a p-value greater than 0.05
 lmMultiUseful = lm(y ~ X1+X2+X3+X4+X8+X12+X13+X23+X24+X25, data = mydataNew)
 summary(lmMultiUseful)
 #Residual Standard Error = 0.827
 
 prlmMultiUseful = predict(lmMultiUseful)
 plot(prlmMultiUseful, y - prlmMultiUseful,ylim = c(-5,5), xlab = 'predicted values', ylab = 'residuals', main = 'Useful Predictors from Multi')
 length(prlmMultiUseful)
 
 #MultiLinear Regression with Interaction
 lmMultiInteraction = lm(y ~ (X1+X2+X3+X4+X8+X12+X13+X23+X24+X25)^2, data = mydataNew)
 summary(lmMultiInteraction)
 #Residual Standard Error = 0.7809
 
 #2.37392
 
 lmMultiInteractionUseful = lm(y ~ X1+X2+X3+X4+X8+X12+X13+X23+X24+X25+(X1:X25)+(X2:X8)+(X2:X12)+(X3:X8)+(X3:X12)+(X4:X8)+(X8:X25)+(X12:X13), data = mydataNew)
 summary(lmMultiInteractionUseful)
 #Residual Standard Error = 0.7774
 prlmMultiInteractionUseful = predict(lmMultiInteractionUseful)
 plot(prlmMultiInteractionUseful, y - prlmMultiInteractionUseful,ylim = c(-5,5), xlab = 'predicted values', ylab = 'residuals', main = 'Multi Interaction with Useful Predictors')

 #2.37572 - Probably Overfits
 
 
 #Submission - GOTTA ASK
 da.sample = data.frame(cbind(1:500,   prlmMultiInteractionUseful))
 names(da.sample) = c('id', 'y')
 write.csv(da.sample, file = "MultilinearSubmission.csv", row.names = FALSE)

  
 
 
 
 