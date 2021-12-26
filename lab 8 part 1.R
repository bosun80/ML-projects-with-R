#*******************Question 1**************

library(tidyverse)
library(tools)
library(arules)
library(dplyr)
B11 <- read.csv("E:/DATA MINING/apriori data/ecoli11.csv", stringsAsFactors = TRUE)
B10 <- read.csv("E:/DATA MINING/apriori data/ecoli10.csv", stringsAsFactors = TRUE)
B5 <- read.csv("E:/DATA MINING/apriori data/ecoli5.csv", stringsAsFactors = TRUE)
B6 <- read.csv("E:/DATA MINING/apriori data/ecoli6.csv", stringsAsFactors = TRUE)
B4 <- read.csv("E:/DATA MINING/apriori data/efaec4.csv", stringsAsFactors = TRUE)
B9 <- read.csv("E:/DATA MINING/apriori data/kleb9.csv", stringsAsFactors = TRUE)
B8 <- read.csv("E:/DATA MINING/apriori data/mg8.csv", stringsAsFactors = TRUE)
B7 <- read.csv("E:/DATA MINING/apriori data/pa55.csv", stringsAsFactors = TRUE)
str(B11)

#***************Question 2*****************
#Remove Column 1,4 and 5

df_Ecoli1241B11 <- B11[c(-1,-4,-5)]
df_Ecoli5391B10 <- B10[c(-1,-4,-5)]
df_Ecoli83972B5 <- B5[c(-1,-4,-5)]
df_EcoliCFT073B6 <- B6[c(-1,-4,-5)]
df_EfaecalisB4 <- B4[c(-1,-4,-5)]
df_KLEBB9 <- B9[c(-1,-4,-5)]
df_MGB8 <- B8[c(-1,-4,-5)]
df_PA5591B7 <- B7[c(-1,-4,-5)]

#*****************Question 3********************

#####Create a column for Bacterium and name the Bacteria strain
df_Ecoli1241B11$Bacterium <- "Ecoli1241B11"
df_Ecoli5391B10$Bacterium <- "Ecoli5391B10"
df_Ecoli83972B5$Bacterium <- "Ecoli83972B5"
df_EcoliCFT073B6$Bacterium <- "EcoliCFT073B6"
df_EfaecalisB4$Bacterium <- "EfaecalisB4"
df_KLEBB9$Bacterium <- "KLEBB9"
df_MGB8$Bacterium <- "MGB8"
df_PA5591B7$Bacterium <- "PA5591B7"
str(df_Ecoli1241B11)

#***************Question 4***********************
#Create a transaction

df_Ecoli1241B11 <- df_Ecoli1241B11 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_Ecoli1241B11$Bacterium <- as.factor(df_Ecoli1241B11$Bacterium)
transactions_Ecoli1241B11 <- as(df_Ecoli1241B11, "transactions")
df_Ecoli5391B10 <- df_Ecoli5391B10 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_Ecoli5391B10$Bacterium <- as.factor(df_Ecoli5391B10$Bacterium)
transactions_Ecoli5391B10 <- as(df_Ecoli5391B10, "transactions")
df_Ecoli83972B5 <- df_Ecoli83972B5 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_Ecoli83972B5$Bacterium <- as.factor(df_Ecoli83972B5$Bacterium)
transactions_Ecoli83792B5 <- as(df_Ecoli83972B5, "transactions")
df_EcoliCFT073B6 <- df_EcoliCFT073B6 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_EcoliCFT073B6$Bacterium <- as.factor(df_EcoliCFT073B6$Bacterium)
transactions_EcoliCFT073B6 <- as(df_EcoliCFT073B6, "transactions")
df_EfaecalisB4 <- df_EfaecalisB4 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_EfaecalisB4$Bacterium <- as.factor(df_EfaecalisB4$Bacterium)
transactions_EfaecalisB4 <- as(df_EfaecalisB4, "transactions")
df_KLEBB9 <- df_KLEBB9 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_KLEBB9$Bacterium <- as.factor(df_KLEBB9$Bacterium)
transactions_KLEBB9 <- as(df_KLEBB9, "transactions")
df_MGB8 <- df_MGB8 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_MGB8$Bacterium <- as.factor(df_MGB8$Bacterium)
transactions_MGB8 <- as(df_MGB8, "transactions")
df_PA5591B7 <- df_PA5591B7 %>%
  group_by(Bacterium, Protein) %>%
  mutate(rn = paste0("Peptide", row_number())) %>%
  spread(rn, Peptide)
df_PA5591B7$Bacterium <- as.factor(df_PA5591B7$Bacterium)
transactions_PA5591B7 <- as(df_PA5591B7, "transactions")


#************Question 5*****************

#write and read back transactions
write.csv(df_Ecoli1241B11,"Ecoli1241B11")
write.csv(df_Ecoli5391B10,"Ecoli5391B10")
write.csv(df_Ecoli83972B5,"Ecoli83972B5")
write.csv(df_EcoliCFT073B6,"EcoliCFT073B6")
write.csv(df_EfaecalisB4,"EfaecalisB4")
write.csv(df_KLEBB9,"KLEBB9")
write.csv(df_MGB8,"MGB8")
write.csv(df_PA5591B7,"PA5591B7")
t1 <- read.transactions("Ecoli1241B11", sep = ",")
t2 <- read.transactions("Ecoli5391B10", sep = ",")
t3 <- read.transactions("Ecoli83972B5", sep = ",")
t4 <- read.transactions("EcoliCFT073B6", sep = ",")
t5 <- read.transactions("EfaecalisB4", sep = ",")
t6 <- read.transactions("KLEBB9", sep = ",")
t7 <- read.transactions("MGB8", sep = ",")
t8 <- read.transactions("PA5591B7", sep = ",")

transactionsT <- c(t1,t2,t3,t4,t5,t6,t7,t8)

#**************Question 6********************

summary(transactionsT)

#***************Qusetion 7*******************

#Using setting, support = 0.1, confidence = 0.25, and minlen = 3, as a starting point to analyze the transactions using 
the apriori() function. Does the function return any rules?
  
  my_rules1 <- apriori(transactionsT, parameter = list(support =0.1, confidence = 0.25, minlen = 3))
summary(my_rules1)

#***************Question 8********************

# Adjust the support and confidence parameters and run the apriori function again. Repeat this until you get a 
# manageable set of rules, say between 10 and 100 rules. Note that minlen = 3 should never be changed

my_rules2 <- apriori(transactionsT, parameter = list(support =0.008, confidence = 0.25, minlen = 3))

#***************Question 9*********************

#Summary of rules

summary(my_rules2)
my_rules3 <- apriori(transactionsT, parameter = list(support =0.006, confidence = 0.25, minlen = 3))
summary(my_rules3)

#**************Question 10**********************

#10 not bacteria listed
subset(my_rules3, items %in% c("Ecoli1241B11", "Ecoli5391B10", "Ecoli83972B5", "EcoliCFT073B6","EfaecalisB4", "KLEBB9", "MGB8","PA5591B7"))

inspect(my_rules3)

#***************Question 11**********************

#Print out the rules in the subset, sorted by lift, using the inspect() function
inspect(sort(my_rules3, by = "lift"))


#write rules to .csv
write(my_rules3, file = "my_arules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# convert rule set to a data frame

my_rules_df <- as(my_rules3, "data.frame")
str(my_rules_df)

