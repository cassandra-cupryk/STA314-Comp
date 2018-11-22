# install.packages('gam')
# install.packages('ISLR')

library(ISLR)
attach(Wage)
library(splines)
library(gam)

# example: GAM with natural splines for year and age, both degree 4
# this can be done by simply using the lm() function
gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

# prepare plotting region
# plot.gam will produce three plots for the three predictors
# note: we need plot.gam, not just plot
# the gam package should be loaded to use this
par(mfrow=c(1,3))
plot.gam(gam1, se=TRUE, col="red")

# now use gam package
# example: GAM with smoothing splines for year and age
# use dummy for education
# note: can not use linear model

gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")

# outcome not too different from previous model

# example: GAM with local regressionfor age and smoothing sline for year
# note: can not use linear model

gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.lo, se=TRUE, col="green")

# look at the output using 'sumary'
# compare lectures for interpretation
summary(gam.lo)

# next consider 3 different models and use anova to compare
# see lectures for explanation of output
gam.m1=gam(wage ~ s(age,df=5)+education,data=Wage)
gam.m2=gam(wage ~ year +s(age,df=5)+education,data=Wage)
gam.m3=gam(wage ~ s(year,df=4)+s(age,df=5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3)

#-----------------------------------------------------------------------
# Extended ANOVA models
# load Advertising data

setwd('/Users/cassie/Desktop')
training = read.csv('/Users/cassie/Desktop/STA314-Comp/trainingdata.csv')
attach(training)

#setwd('C:/Users/stas/Desktop') # change to a different directory
#Adv = read.csv(file = 'Advertising.csv') # read a file from the working directory
#attach(Adv)


# load a package called akima
# will be used for plotting
#install.packages('akima')
library(akima)

gam1 = gam(y ~ X1 + X2 + X3 + X4 + X8 + X12 + X23 + X25 + s(X1,df=5), data = training)

#gam1 = gam(Sales ~ Radio + TV:Radio + s(TV,df=5) + Newspaper,data = Adv)
summary(gam1)

# sumary indicates that Newspaper can be dropped

# now consider a model with 'non-parametric interaction'
# between TV and Radio

gam.np=gam(sales~lo(TV,radio,span=0.7)+newspaper,data=Adv)
par(mfrow=c(1,2))
plot(gam.np)

