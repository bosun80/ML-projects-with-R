library(tidyverse)
library(readr)

#**************Question 1*************

df1 <- read.csv("features_Control.csv")
df2 <- read.csv("features_noAction.csv")
df3 <- read.csv("features_Piano.csv")
df4 <- read.csv("features_Read.csv")


#************Question 2***************
df1$Hurst
df2$Hurst
df3$Hurst
df4$Hurst

repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}                               # they need to be repeated

df1$Hurst <- repeat.before(df1$Hurst)
df2$Hurst <- repeat.before(df2$Hurst)
df3$Hurst <- repeat.before(df3$Hurst)
df4$Hurst <- repeat.before(df4$Hurst)


#************Question 3***************

df1$State <- as.factor("Control")
df2$State <- as.factor("NoAction")
df3$State <- as.factor("Piano")
df4$State <- as.factor("Read")

colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)

#***********Question 4*********************

df5 <- rbind(df1,df2,df3,df4)

#***********Question 5*********************

set.seed(100000)

rand <- sample(dim(df5)[1],0.9*dim(df5)[1])

df5_train <- df5[rand,]
df5_test  <- df5[-rand,]

#df5_train_labels <- as.factor(df5[rand,21])
#df5_test_labels <- as.factor(df5[rand,21])

#************Question 6********************
#m <- rpart(y ~ x, data = mydata)
library(rpart)
df5_train_model <- rpart(State ~ delta + theta + alpha + beta + gamma + deltaRIR + thetaRIR         
                         + alphaRIR + betaRIR + gammaRIR + PFD + HFD + hjorth_mobility +hjorth_complexity
                         + spec_entropy + svd_entropy + fisher_info + approx_entropy + DFA + Hurst, data = df5_train)

#*************Question 7***********************
#p <- predict(m,test, type = "vector"/"class")
df5_pred_model <- predict(df5_train_model,df5_test, type = "class")

#**************Question 8**********************

library(gmodels)
CrossTable(df5_test$State, df5_pred_model)

#**************Question 9*********************

summary(df5_train_model)

#**************Question 10*********************

#install.packages("rpart.plot")
library(rpart.plot)

df5_plot <- rpart.plot(df5_train_model,digits = 3)




