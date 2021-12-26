
#*************Question1****************
library(tidyverse)
library(readr)

mydata1 <- read.csv("protein-peptides1.csv")
mydata2 <- read.csv("protein-peptides2.csv")
mydata3 <- read.csv("protein-peptides3.csv")

#***********Question2****************

mydata1 <- mydata1[-c(1:4,5,15,17,19,22,23)]
mydata2 <- mydata2[-c(1:4,5,15,17,19,22,23)]
mydata3 <- mydata3[-c(1:4,5,15,17,19,22,23)]

#***********Question3****************

#tidyr::separate(x,"class"",c("levels"))

str(mydata1)
mydata1 <- tidyr::separate(mydata1,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata1$RangeDiff <- mydata1$RangeStart - mydata1$RangeEnd

mydata2 <- tidyr::separate(mydata2,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata2$RangeDiff <- mydata2$RangeStart - mydata2$RangeEnd

mydata3 <- tidyr::separate(mydata3,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata3$RangeDiff <- mydata3$RangeStart - mydata3$RangeEnd

colnames(mydata1)
colnames(mydata2)
colnames(mydata3)

******************Question 4****************
#str(mydata1)

mydata1$strain <- "Ecoli1241-B11" 
mydata2$strain <- "Efaecalis-B" 
mydata3$strain <- "PA5591-B7"

names(mydata1)[10] <- "Intensity.Sample"
names(mydata2)[10] <- "Intensity.Sample"
names(mydata3)[10] <- "Intensity.Sample"

  
mydata4 <- rbind(mydata1,mydata2,mydata3)
str(mydata4)

#****************Question 5*****************

set.seed(43276)
rand <- sample(dim(mydata4)[1],.75*dim(mydata4)[1])

mydata4_train <- mydata4[rand,-16]
mydata4_test <- mydata4[-rand,-16]
dim(mydata4_train)

mydata4_train_labels <- as.factor(mydata4[rand,16])
str(mydata4_train_labels)

mydata4_test_labels  <- mydata4[-rand,16]

#***************Question7********************
#install.packages("C50")
library(C50)

mydata5_model <- C5.0(mydata4_train,mydata4_train_labels,trial = 10)

mydata5_pred <- predict(mydata5_model,mydata4_test,type="class")

#***************Question8*******************

library(gmodels)
CrossTable(mydata5_pred,mydata4_test_labels, prop.chisq =F,dnn = c('predicted','actual'))

#****************Question9********************

summary(mydata5_model)






