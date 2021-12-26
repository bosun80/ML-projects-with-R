#............Question 1...............
mydat <- read.csv("220_stats.csv")
mydat <- data.frame(mydat)

#...........Question 2...............

#remove unwanted data i.e 1,2 and 8 columns
mydat <- mydat[-c(1:2,8)]
mydat$Importance <- as.factor(mydat$Importance)

#............Question 3...............

discretize <- function(x) {
  return(round((10*(x-min(x)))/(max(x)-min(x))))
}
#mydat[,1]
#colnames(mydat)

#.............Question 4..............

mydat1 <- mydat[,-6]
dim(mydat1)
mydat1_train <- mydat1[1:107,]
mydat1_test <- mydat1[108:135,]
mydat1_train_labels <- mydat[1:107,6]
mydat1_test_labels <- mydat[108:135,6]
#set.seed(135)

#............Question 5...............

library(class)
mydat1_pred <- knn(train = mydat1_train, test = mydat1_test, cl = mydat1_train_labels, k=12)
library(gmodels)
CrossTable(x= mydat1_test_labels, y= mydat1_pred, prop.chisq = F)

#............Question 6...............

install.packages("e1071")
library(e1071)
mydat1classifier <- naiveBayes(mydat1_train, mydat1_train_labels, laplace = 1)

#.............Question 7..............

mydat1classifier_pred <- predict(mydat1classifier,mydat1_test)

#.............Question 8...............

CrossTable(mydat1_test_labels, mydat1classifier_pred, prop.chisq = F)
