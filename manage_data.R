
#loading the data
library(haven)
library(dplyr)
library(haven)
library("rstudioapi")

source("manage_data.R", chdir = T)
setwd(dirname(getActiveDocumentContext()$path))  

Data = read_dta("Z12_2016_part.dta")
write.csv(Data, file = "Z12_2016_part.csv")
View(Data)

names(Data)
NewData = subset(Data, select = c("WOJ","B2","B4","B5","B6","B7","WIEK","STAZ_OGOL","TD5","TWO_ROK") )
View(NewData)

#descriptive statistics of the data
summary(NewData)

#structure of the data
str(NewData)

#number of rows
nrow(NewData)

###### THE DATA CLEANING PROCESS #####

# chr -> factor/numeric variable

NewData$WOJ <- as.factor(NewData$WOJ)
NewData$B2 <- as.factor(NewData$B2)
NewData$B4 <- as.factor(NewData$B4)
NewData$B5 <- as.factor(NewData$B5)
NewData$B6 <- as.factor(NewData$B6)
NewData$B7 <- as.factor(NewData$B7)
NewData$WIEK <- as.factor(NewData$WIEK)
NewData$STAZ_OGOL <- as.numeric(NewData$STAZ_OGOL)
NewData$TD5 <- as.numeric(NewData$TD5)
NewData$TWO_ROK<- as.numeric(NewData$TWO_ROK)

# how many NAs do we have?

sapply(NewData,function(x) sum(is.na(x)))
# 0 NA's

# let us see what is the distribution of the chosen variables

#WOJ
par(mfrow = c(1,1))
barplot(table(NewData$WOJ), main = "Distribution of the WOJ variable")

#B2
par(mfrow = c(1,1))
barplot(table(NewData$B2), main = "Distribution of the B2 variable")

#B4
par(mfrow = c(1,1))
barplot(table(NewData$B4), main = "Distribution of the B4 variable")

#B5
par(mfrow = c(1,1))
barplot(table(NewData$B5), main = "Distribution of the B5 variable")

#B6
par(mfrow = c(1,1))
barplot(table(NewData$B6), main = "Distribution of the B6 variable")

#B7
par(mfrow = c(1,1))
barplot(table(NewData$B7), main = "Distribution of the B7 variable")

#WIEK
par(mfrow = c(1,1))
barplot(table(NewData$WIEK), main = "Distribution of the WIEK variable")

#decomposition of variables 
hist(dataset_salary$age)
hist(dataset_salary$`educational-num`)
hist(dataset_salary$`hours-per-week`)
hist(dataset_salary$`capital-gain`)
hist(dataset_salary$`capital-loss`)

#codebook
codebook <- enframe(get_labels(NewData))
colnames(codebook) <- c("variable_id", "item_text")
View(codebook)