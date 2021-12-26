#*************Question1****************
library(tidyverse)
library(readr)

mydata1 <- read.csv("protein-peptides01.csv")
mydata2 <- read.csv("protein-peptides02.csv")
mydata3 <- read.csv("protein-peptides03.csv")
mydata4 <- read.csv("protein-peptides04.csv")
mydata5 <- read.csv("protein-peptides05.csv")
mydata6 <- read.csv("protein-peptides06.csv")
mydata7 <- read.csv("protein-peptides07.csv")
mydata8 <- read.csv("protein-peptides08.csv")


#*************Question2******************
colnames(mydata1)
mydata1 <- mydata1[-c(1:2,5,15,16,17,19,22,23)]
mydata2 <- mydata2[-c(1:2,5,15,16,17,19,22,23)]
mydata3 <- mydata3[-c(1:2,5,15,16,17,19,22,23)]
mydata4 <- mydata4[-c(1:2,5,15,16,17,19,22,23)]
mydata5 <- mydata5[-c(1:2,5,15,16,17,19,22,23)]
mydata6<-  mydata6[-c(1:2,5,15,16,17,19,22,23)]
mydata7 <- mydata7[-c(1:2,5,15,16,17,19,22,23)]
mydata8 <- mydata8[-c(1:2,5,15,16,17,19,22,23)]

#*************Question3*********************


#tidyr::separate(x,"class"",c("levels"))

str(mydata1)
mydata1 <- tidyr::separate(mydata1,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata1$RangeDiff <- mydata1$RangeStart - mydata1$RangeEnd

mydata2 <- tidyr::separate(mydata2,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata2$RangeDiff <- mydata2$RangeStart - mydata2$RangeEnd

mydata3 <- tidyr::separate(mydata3,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata3$RangeDiff <- mydata3$RangeStart - mydata3$RangeEnd

mydata4 <- tidyr::separate(mydata4,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata4$RangeDiff <- mydata4$RangeStart - mydata4$RangeEnd

mydata5 <- tidyr::separate(mydata5,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata5$RangeDiff <- mydata5$RangeStart - mydata5$RangeEnd

mydata6 <- tidyr::separate(mydata6,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata6$RangeDiff <- mydata6$RangeStart - mydata6$RangeEnd

mydata7 <- tidyr::separate(mydata7,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata7$RangeDiff <- mydata7$RangeStart - mydata7$RangeEnd

mydata8 <- tidyr::separate(mydata8,"X1.k0.Range", c("RangeStart", "RangeEnd"),sep = "-",convert = T,remove = T)
mydata8$RangeDiff <- mydata8$RangeStart - mydata8$RangeEnd



colnames(mydata1)
colnames(mydata2)
colnames(mydata3)
colnames(mydata4)
colnames(mydata5)
colnames(mydata6)
colnames(mydata7)
colnames(mydata8)

#***************Question4*****************

mydata1$Protein.Accession <- mydata1$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata2$Protein.Accession <- mydata2$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata3$Protein.Accession <- mydata3$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata4$Protein.Accession <- mydata4$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata5$Protein.Accession <- mydata5$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata6$Protein.Accession <- mydata6$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata7$Protein.Accession <- mydata7$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]
mydata8$Protein.Accession <- mydata8$Protein.Accession %>% str_split('\\|', simplify = TRUE) %>% .[,1]

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata1$Peptide)
mydata1$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata2$Peptide)
mydata2$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata3$Peptide)
mydata3$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata4$Peptide)
mydata4$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata5$Peptide)
mydata5$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata6$Peptide)
mydata6$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata7$Peptide)
mydata7$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)

Peptide1 <- gsub('^[^.]+\\.([^ ]+)\\.[^.]+$', '\\1',mydata8$Peptide)
mydata8$Peptide1 <- gsub('\\([^ ]+\\)', '',Peptide1)


#**************Question5******************
names(mydata1)[12] <- "intensity.sample"
names(mydata2)[12] <- "intensity.sample"
names(mydata3)[12] <- "intensity.sample"
names(mydata4)[12] <- "intensity.sample"
names(mydata5)[12] <- "intensity.sample"
names(mydata6)[12] <- "intensity.sample"
names(mydata7)[12] <- "intensity.sample"
names(mydata8)[12] <- "intensity.sample"

mydata1$Strain <- "Ecoli1241-B11"
mydata2$Strain <- "Ecoli5391-B10"
mydata3$Strain <- "Ecoli83972-B5"
mydata4$Strain <- "EcoliCFT073-B6"
mydata5$Strain <- "Efaecalis-B4"
mydata6$Strain <- "KLEB-B9"
mydata7$Strain <- "MG-B8"
mydata8$Strain <- "PA5591-B7"

#**************Question6********************
mydata1 <- mydata1[-2]
mydata2 <- mydata2[-2]
mydata3 <- mydata3[-2]
mydata4 <- mydata4[-2]
mydata5 <- mydata5[-2]
mydata6 <- mydata6[-2]
mydata7 <- mydata7[-2]
mydata8 <- mydata8[-2]

dim(mydata1)
dim(mydata2)
dim(mydata3)
dim(mydata4)
dim(mydata5)
dim(mydata6)
dim(mydata7)
dim(mydata8)


# I m extracting 1000 rows from each clean dataset and then we wl rbing new datasets, so in total dataset mydata9, we will have 8000 rows.

df1a <- mydata1[1:1000,]
df2a <- mydata2[1:1000,]
df3a <- mydata3[1:1000,]
df4a <- mydata4[1:1000,]
df5a <- mydata5[1:1000,]
df6a <- mydata6[1:1000,]
df7a <- mydata7[1:1000,]
df8a <- mydata8[1:1000,]

mydata9 <- rbind(df1a,df2a,df3a,df4a, df5a, df6a,df7a,df8a)

#mydata9 <- rbind(mydata1,mydata2,mydata3,mydata4,mydata5,mydata6,mydata7,mydata8)
str(mydata9)

#**************Question7*********************

set.seed(566666666)
rnd <- sample(dim(mydata9)[1],0.75*dim(mydata9)[1])
# we need Strain column as well in training and test data for rweka
mydata9_train <- mydata9[rnd,]
mydata9_test  <- mydata9[-rnd,]

#************Question 9********************
str(mydata9_train)

mydata9_train_labels <- as.factor(mydata9[rnd,17])
mydata9_test_labels <- as.factor(mydata9[-rnd,17])
colnames(mydata9)
#**************Question 10******************
library(C50)

# if you will use training data using all columns then c5.0 will be like:
colnames(mydata9)
mydata9_model <- C5.0(mydata9_train[-17],mydata9_train_labels,trial = 10)


#***************Question 11******************

mydata9_pred <- predict(mydata9_model,mydata9_test[-17], type = "class")

#**************Question 12*******************

library(gmodels)
library(gmodels)
mydata9_train$Protein.Accession <- as.factor(mydata9_train$Protein.Accession)
mydata9_train$Strain <- as.factor(mydata9_train$Strain)
mydata9_train$Protein.Accession <- as.factor(mydata9_train$Protein.Accession)
mydata9_train$Peptide1 <- as.factor(mydata9_train$Peptide1)

CrossTable(mydata9_pred,mydata9_test_labels, p.chisq = F, dnn = c('Predict','Actual'))

summary(mydata9_model)

#**************Question 13********************

#install.packages("RWeka")
library(RWeka)
# we are using dot(.) after ~ bcz we want to predict using all variables

mydata9_oneR_model <- OneR(Strain ~ .,data = mydata9_train) 

#**************Question 14*************************

mydata9_oneR_pred <- predict(mydata9_oneR_model,mydata9_test,type = "class")

#**************Question 15**************************

#  if we will still get error of something like CHr then first just convert every charater vector into factor like:
a <- as.factor(a) # just example, you can check type by str(a)

library(gmodels)
CrossTable(mydata9_oneR_pred,mydata9_test_labels,p.chisq=F)

summary(mydata9_oneR_model)

str(mydata9_test)
