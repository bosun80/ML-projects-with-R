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

colnames(mydata1)
colnames(mydata2)
colnames(mydata3)
colnames(mydata4)
colnames(mydata5)
colnames(mydata6)
colnames(mydata7)
colnames(mydata8)

str(mydata1)
str(mydata2)
str(mydata3)
str(mydata4)
str(mydata5)
str(mydata6)
str(mydata7)
str(mydata8)

dim(mydata1)
dim(mydata2)
dim(mydata3)
dim(mydata4)
dim(mydata5)
dim(mydata6)
dim(mydata7)
dim(mydata8)



mydata9 <- rbind(mydata1,mydata2,mydata3,mydata4,mydata5,mydata6,mydata7,mydata8)
str(mydata9)

#**************Question7*********************

set.seed(566666666)
rnd <- sample(dim(mydata9)[1],0.75*dim(mydata9)[1])

mydata9_train <- mydata9[rnd,-17]
mydata9_test  <- mydata9[-rnd,-17]

#************Question 9********************
mydata9_train_labels <- as.factor(mydata9[rnd,17])
mydata9_test_labels <- as.factor(mydata9[-rnd,17])

#**************Question 10******************
library(C50)
mydata9_model <- C5.0(mydata9_train,mydata9_train_labels,trial = 10)

#***************Question 11******************

mydata9_pred <- predict(mydata9_model,mydata9_test_labels, type = "class")

#**************Question 12*******************

library(gmodels)
CrossTable(mydata9_pred,mydata9_model, p.chisq = F, dnn = c('Predict','Actual'))
summary(mydata9_model)

#**************Question 13********************

#install.packages("RWeka")
library(RWeka)

mydata9_oneR_model <- OneR(mydata9$Strain ~ mydata9_train,mydata9)

#**************Question 14*************************

mydata9_oneR_pred <- predict(OneR_model,mydata9_test,type = "class")

#**************Question 15**************************

library(gmodels)
CrossTable(mydata9_oneR_pred,mydata9_oneR_model,p.chisq = F, dnn =c('Predict','Actual'))

summary(mydata9_oneR_model)


