
#loading the data
library(haven)
Data = read_dta("C:\\Users\\gracz\\Dropbox\\My PC (LAPTOP-QJJ2QSFO)\\Desktop\\Z12_2016_part.dta")
write.csv(Data, file = "Z12_2016_part.csv")
View(Data)

names(Data)
NewData = subset(Data, select = c("WOJ","B2","B4","B5","B6","B7","WIEK","STAZ_OGOL","TD5","TWO_ROK") )
View(NewData)

#descriptive statistics of the data
summary(NewData)

#structure of the data
str(NewData)

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

# let us see what is the distribution of the income indicator in the dataset.

par(mfrow = c(1,1))
plot(table(NewData$TWO_ROK), main = "SALARY")

#decomposition of variables 
hist(dataset_salary$age)
hist(dataset_salary$`educational-num`)
hist(dataset_salary$`hours-per-week`)
hist(dataset_salary$`capital-gain`)
hist(dataset_salary$`capital-loss`)

