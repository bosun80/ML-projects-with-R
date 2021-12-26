library(tidyverse)
library(diplyr)
library(readr)
#***********Question 1***************

mydata <- read_csv("features.csv")
str(mydata)

#***********Question 2***************

plot.default(mydata$meditation, mydata$attention, xlab ="meditation",ylab="attention",col=3)


#***********Question 3***************

my_train_model <- lm(attention ~ delta + theta + alpha + beta + gamma + deltaRIR + 
                      thetaRIR + alphaRIR + betaRIR + gammaRIR + PFD + HFD + hjorth_mobility 
                     + hjorth_complexity + spec_entropy + svd_entropy + 
                      + fisher_info + approx_entropy + DFA + Hurst,data = mydata)

#**********Question 4*****************

summary(my_train_model)

#***********Question 5***************

my_train_model1 <- lm(meditation ~ delta + theta + alpha + beta + gamma + deltaRIR + 
                       thetaRIR + alphaRIR + betaRIR + gammaRIR + PFD + HFD + hjorth_mobility 
                     + hjorth_complexity + spec_entropy + svd_entropy + 
                       + fisher_info + approx_entropy + DFA + Hurst,data = mydata)
summary(my_train_model1)





