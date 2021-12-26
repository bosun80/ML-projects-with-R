
.............Question 1................
#install.packages("tidyverse")
#install.packages("readr")

library(tidyverse)
library(readr)

mydata1 <- read_csv("peptide_peak30.csv")
mydata2 <- read_csv("peptide_peak40.csv")

mydata1 <- as.data.frame(mydata1)
mydata2 <- as.data.frame(mydata2)

.............Question 2...................

#tidyr::separate(x,"class"",c("levels"))
mydata1 <- tidyr::separate(mydata1,"1/K0 Range", c("Startvalue", "Endvalue"),
sep = "-",convert = T,remove = T)
mydata1$Range <- mydata1$Endvalue - mydata1$Startvalue

mydata1 <- mydata1[c(1,2,3,4,5,6,7,8,9,14,10,11,12,13)]

mydata1[1,]

mydata2 <- tidyr::separate(mydata2,"1/K0 Range", c("Startvalue", "Endvalue"),
sep = "-",convert = T,remove = T)

mydata2$Range <- mydata2$Endvalue - mydata2$Startvalue

mydata2 <- mydata2[c(1,2,3,4,5,6,7,8,9,14,10,11,12,13)]
mydata2[2,]


normalize <- function(x) {
  return(ifelse(min(x) < max(x), ((x - min(x))/(max(x) - min(x))),0.5))
}

mydata1[,(c(2:11))] <- apply(mydata1[,(c(2:11))],2,normalize)
mydata2[,(c(2:11))] <- apply(mydata2[,(c(2:13))],2,normalize)


.............Question3..................

mydata1$Common <- factor(mydata1$Common, levels = c("T","F"),labels =c("TRUE","FALSE))
mydata2$Common <- factor(mydata2$Common, levels = c("T","F"),labels =c("TRUE","FALSE))

mydata1$Common <- mydata1$Peptide %in% mydata2$Peptide
mydata2$Common <- mydata2$Peptide %in% mydata1$Peptide

..............Question4...................

colnames(mydata1)[11] <- "intensity sample 41"
colnames(mydata2)[11] <- "intensity sample 41"
mydata3 <- rbind(mydata1,mydata2)

...............Question5..................
dim(mydata3)
mydata3 <- mydata3[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
#str(mydata3)
#leave only the numeric and interger variables
mydata4 <- mydata3[c(1,3:14)]

mydata4_train <- mydata4[1:47385,]
mydata4_test <- mydata4[47386:59231,]

mydata4_train_labels <- mydata4[1:47385,1]
mydata4_test_labels <- mydata4[47386:59231,1]

 ................Question 6..........

library(class)
 
mydata4_test_pred <- knn(mydata4_train,mydata4_test,mydata4_train_labels, k = 217)


.................Question 7..............

library(gmodels)

mytable <- CrossTable(x= mydata4_test_labels,y = mydata4_test_pred, prop.chisq = FALSE)

### The cell proportion are divided into four and each in percentage
### The top-left is the F/F is probability that false test label is falsely predicted i.e 153/11846 = 1.29%
### The top-right is F/T is probability that the false test label is truely predicted i.e 2208/11846 = 18.6%
### The down-left is the T/F is probability that true test labels is falsely predicted i.e 88/11846 = 0.7%
### The down-right is T/T is probability that the true test label is truely predicted i.e 9397/11846 = 79.3%
  

  



