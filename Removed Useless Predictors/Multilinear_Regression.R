# clear workspace
rm(list = ls())

#load libraries
library(caret) #for k-nn regression
library(class) #for k-nn regression
library(car)

 #getwd() # show current working directory. this is where on the computer
 # files are saved by default and where R looks ofr files
setwd('/Users/cassie/Desktop/STA314-Comp/') # change to a different directory
# pick the right directory on your computer
training = read.csv(file = 'trainingdata.csv') # read a file from the working directory

 attach(training) # make elements of res available in workspace
 
 #General MultiLinear Regression
 lm.fit = lm(y ~ ., data = training)
 pr.lmfit = predict(lm.fit)
 pr.lmfit
 summary(lm.fit)
 vif(lm.fit)
 #Residual Standard Error = 0.8254
 
 
#Removed the variables with high variances, did not work
 lm.RemoveHighVar = lm(y ~ . - X1 - X2- X3-X4-X5-X6-X7, data=training)
summary(lm.RemoveHighVar)
#Residual Standard Error = 1.071

#Removed the variables with high variances, did not work
lm.LowVar = lm(y ~ X8 + X12+ X23+X24+X25, data=training)
summary(lm.LowVar)
#Residual Standard Error = 1.074
 
 # lm1.fit shows that a linear model may be be better
 lm1.fit = lm(y ~ I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2) + I(X5^2) + 
                I(X6^2) + I(X7^2) +I(X8^2) + I(X9^2) + I(X10^2) + 
                I(X11^2) + I(X12^2) + I(X13^2) + I(X14^2) + I(X15^2)
              + I(X16^2) + I(X17^2) +I(X18^2) + I(X19^2) + I(X20^2) 
              + I(X21^2) + I(X22^2) + I(X23^2) + I(X24^2) + I(X25^2)
              + I(X26^2) + I(X27^2) + I(X28^2), data = training) 
 summary(lm1.fit)
 #Residual Standard Error = 1.532
 
 lm2.fit = lm(y ~ I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2) + I(X12^2) + I(X28^2), data = training) 
 summary(lm2.fit)
 #Residual Standard Error = 1.525
 
 
 #Removed predictors that have a p-value greater than 0.05
 lmMultiUseful = lm(y ~ X1+X2+X3+X4+X8+X12+X13+X23+X24+X25, data = training)
 summary(lmMultiUseful)
 #Residual Standard Error = 0.827
 
 prlmMultiUseful = predict(lmMultiUseful)
 plot(prlmMultiUseful, y - prlmMultiUseful,ylim = c(-5,5), xlab = 'predicted values', ylab = 'residuals', main = 'Useful Predictors from Multi')
 length(prlmMultiUseful)
 
 
 #Not an improvement
 lmLOOCV = lm(y~ (X1^2) + (X2^2) + (X3^2) + (X4^2) + (X8^2) + (X23^2) + (X25^2) , data = training)
 summary(lmLOOCV)
 
 
 
 #MultiLinear Regression with Interaction
 lmMultiInteraction = lm(y ~ (X1+X2+X3+X4+X8+X12+X13+X23+X24+X25)^2, data = training)
 summary(lmMultiInteraction)
 #Residual Standard Error = 0.7809
 
 #2.37392
 
 lmMultiInteractionUseful = lm(y ~ X1+X2+X3+X4+X8+X12+X13+X23+X24+X25+(X1:X25)+(X2:X8)+(X2:X12)+(X3:X8)+(X3:X12)+(X4:X8)+(X8:X25)+(X12:X13), data = training)
 summary(lmMultiInteractionUseful)
 #Residual Standard Error = 0.7774
 prlmMultiInteractionUseful = predict(lmMultiInteractionUseful)
 plot(prlmMultiInteractionUseful, y - prlmMultiInteractionUseful,ylim = c(-5,5), xlab = 'predicted values', ylab = 'residuals', main = 'Multi Interaction with Useful Predictors')

 #2.37572 - Probably Overfits
 
 
 #Submission 
 da.sample = data.frame(cbind(1:500, prlmMulti))
 names(da.sample) = c('id', 'y')
 write.csv(da.sample, file = "Submission.csv", row.names = FALSE)

  
 
 
 
 