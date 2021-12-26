library(tidyverse)
library(readr)

#************Question 1******************
  
df1 <- read.csv("features_channel1.csv")
df2 <- read.table("trainTargets.txt",header= F)

dim(df1)
dim(df2)

#***********Question 2*******************

  normalize <- function(x) {
    return(ifelse(min(x) < max(x), ((x - min(x))/(max(x) - min(x))),0.5))
  }


df1[,c(1:19)] <- apply(df1[,c(1:19)],2,normalize)


#***********Question 3********************
#Add target feature "response"
#Changing df2 to factors P300 or otherwise from 1 or 0

df1$response <- ifelse(df2$V1 == 1,"P300","otherwise")

#************Question 4*******************

df3_train <- df1[1:3840,]
df3_test <- df1[3841:4800,]


training_data <- df3_train
testing_data <- df3_test

#***********Question 5*********************
#install.packages("neuralnet")
library(neuralnet)

df3_train$

  y <- model.matrix(~ df3_train$response + 0, data = df3_train[, c('response'), drop=FALSE])
# fix up names for as.formula
y_feats <- gsub("^[^ ]+\\$", "", colnames(y))
colnames(y) <- y_feats

df3_train <- df3_train[, !(colnames(df3_train) == "response")]
feats <- colnames(df3_train)
df3_train <- cbind(y, df3_train)

# Concatenate strings
f <- paste(feats, collapse=' + ')
y_f <- paste(y_feats, collapse=' + ')
f <- paste(y_f, '~', f)

# Convert to formula
f <- as.formula(f)

df3_train_model <- neuralnet(f, df3_train, stepmax = 1e+08,hidden = 5)


#************Question 6***********************

model_results <- compute(df3_train_model, df3_test[1:19]) 

#************Question 7***********************

library(gmodels)
predicted <- as.data.frame(model_results$net.result)
colnames(predicted) <- c('P300','otherwise')
pred <- colnames(predicted)[max.col(predicted,ties.method="first")]
pred <- as.factor(pred)

#Creating crosstable to compare predict response to true response e.g test(predicted) & test label(actual)

CrossTable(pred, df3_test$response)


#**************Question 8*********************
  
plot(df3_train_model)

#**************Question 9*********************
#repeating 5-7 using Gaussian RBF kernel

#install.packages("kernlab")
library(kernlab)

svm_model <- ksvm(response ~ .,data = training_data, kernel = "vanilladot")

svm_pred <- predict(svm_model,testing_data)


#Creating crosstable to compare predict response to true response

CrossTable(svm_pred, df3_test$response)







