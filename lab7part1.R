library(tidyverse)
library(readr)

#************Question 1**************
  
df1 <- read.csv("extracted_Control_5FD.csv")
df2 <- read.csv("extracted_noAction_5FD.csv")
df3 <- read.csv("extracted_Piano_5FD.csv")
df4 <- read.csv("extracted_Read_5FD.csv") 

#***********Question 2***************

df1$label <- "Control"
df2$label <- "noAction"
df3$label <- "Piano"
df4$label <- "Read"

colnames(df1)  
colnames(df2)
colnames(df3)
colnames(df4)
#**********Question 3******************
str(df1)

normalize <- function(x) {
  return(ifelse(min(x) < max(x), ((x - min(x))/(max(x) - min(x))),0.5))
}

df1[,c(1:20)] <- apply(df1[,c(1:20)],2,normalize)
df2[,c(1:20)] <- apply(df2[,c(1:20)],2,normalize)
df3[,c(1:20)] <- apply(df3[,c(1:20)],2,normalize)
df4[,c(1:20)] <- apply(df4[,c(1:20)],2,normalize)

#***********Question 4*******************

set.seed(1000000)
rand1 <- sample(dim(df1)[1],0.8*dim(df1)[1])
rand2 <- sample(dim(df2)[1],0.8*dim(df2)[1])
rand3 <- sample(dim(df3)[1],0.8*dim(df3)[1])
rand4<- sample(dim(df4)[1],0.8*dim(df4)[1])

df1_train <- df1[rand1,]
df2_train <- df2[rand2,]
df3_train <- df3[rand3,]
df4_train <- df4[rand4,]

df1_test <- df1[-rand1,]
df2_test <- df2[-rand2,]
df3_test <- df3[-rand3,]
df4_test <- df4[-rand4,]

#**********Question 5*******************

df5_train <- rbind(df1_train,df2_train,df3_train,df4_train)
df5_test <- rbind(df1_test,df2_test,df3_test,df4_test)

#**********Question 6*******************

df5_train$label <- as.factor(df5_train$label)
df5_test$label  <- as.factor(df5_test$label)
#The labels are multiple

#install.packages("neuralnet")
library(neuralnet)


y <- model.matrix(~ df5_train$label + 0, data = df5_train[, c('label'), drop=FALSE])
# fix up names for as.formula
y_feats <- gsub("^[^ ]+\\$", "", colnames(y))
colnames(y) <- y_feats

df5_train <- df5_train[, !(colnames(df5_train) == "label")]
feats <- colnames(df5_train)
df5_train <- cbind(y, df5_train)

# Concatenate strings
f <- paste(feats, collapse=' + ')
y_f <- paste(y_feats, collapse=' + ')
f <- paste(y_f, '~', f)

# Convert to formula
f <- as.formula(f)

df5_train_model <- neuralnet(f, df5_train, stepmax = 1e+08,hidden = 5)

#str(df5_train)
#***************Question 7******************

model_results <- compute(df5_train_model, df5_test[1:20]) 


#***************Question 8********************

library(gmodels)
predicted <- as.data.frame(model_results$net.result)
colnames(predicted) <- c('extracted_Control_5FD', 'extracted_noAction_5FD', 'extracted_Piano_5FD', 'extracted_Read_5FD')
pred <- colnames(predicted)[max.col(predicted,ties.method="first")]
pred <- as.factor(pred)

CrossTable(pred, df5_test$label)
plot(df5_train_model)

#****************Question 9*********************

plot(df5_train_model)

#****************Question 10*********************

df_all <- rbind(df1,df2,df3,df4)

df_all[,c(1:20)] <- apply(df_full[,c(1:20)],2,normalize)


colnames(df1_train)
colnames(df2_train)


