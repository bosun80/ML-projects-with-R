library(tidyverse)
library(readr)

#************Question 1******************

df1 <- read.csv("features_channel1.csv")
df2 <- read.table("trainTargets.txt",header= F)

dim(df1)
dim(df2)
#***********Question 2*******************
colnames(df1)

normalize <- function(x) {
  return(ifelse(min(x) < max(x), ((x - min(x))/(max(x) - min(x))),0.5))
}


df1[,c(1:19)] <- apply(df1[,c(1:19)],2,normalize)


#***********Question 3********************
#Add target feature "response"
#Changing df2 to factors P300 or otherwise from 1 or 0

df1$response <- ifelse(df2$V1 == 1,"P300","otherwise")

#************Question 4*******************
colnames(df1)

df1_train <- df1[1:4320,]
df1_test <- df1[4321:4800,]

training_data <- df1_train
testing_data <- df1_test

training_data1 <- df1[1:4320,]

#***************Question 5******************
#install.packages("stats")
library(stats)

kmeansresult <- kmeans(training_data[-20], 2)

#****************Question 6******************

clusterAssignments <- ifelse(kmeansresult$cluster == 1, "otherwise","P300")

nrow(clusterAssignments)


#************ANSWER TO QUESTION 7**************
library(gmodels)
CrossTable(clusterAssignments,training_data$response)

#** ANSWER TO QUESTION 8*****************************

kmeansresult$centers

#** ANSWER TO QUESTION 9*****************************

rownumber <- ifelse(clusterAssignments != training_data$response,"not match","match")
training_data$newcolumn <- rownumber
training_data <- training_data[-grep("not match", training_data$newcolumn),]
training_data <- as.data.frame(training_data)
dim(training_data)

training_data$newcolumn <- NULL

#** ANSWER TO QUESTION 10*****************************
library(neuralnet)

neuralmodel <- neuralnet(response~. , data = training_data1, hidden = 5)
neuralmodel2 <- neuralnet(response~. , data = training_data, hidden = 5)

#** ANSWER TO QUESTION 11*****************************


predictionNeural <- compute(neuralmodel,testing_data)
predictionNeural2 <- compute(neuralmodel2,testing_data)

#** ANSWER TO QUESTION 12*****************************

predicted <- as.data.frame(predictionNeural$net.result)
colnames(predicted) <- c('other','P300')
pred <- colnames(predicted)[max.col(predicted,ties.method="first")]
pred <- as.factor(pred)

predicted2 <- as.data.frame(predictionNeural2$net.result)
colnames(predicted2) <- c('other','P300')
pred2 <- colnames(predicted2)[max.col(predicted2,ties.method="first")]
pred2 <- as.factor(pred2)




CrossTable(pred,testing_data$response)

CrossTable(pred2,testing_data$response)

#** ANSWER TO QUESTION 13*****************************

plot(neuralmodel)





