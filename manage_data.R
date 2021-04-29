
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

# chr -> factor

dataset_salary$workclass <- as.factor(dataset_salary$workclass)
dataset_salary$education <- as.factor(dataset_salary$education)
dataset_salary$`marital-status` <- as.factor(dataset_salary$`marital-status`)
dataset_salary$occupation <- as.factor(dataset_salary$occupation)
dataset_salary$relationship <- as.factor(dataset_salary$relationship)
dataset_salary$race <- as.factor(dataset_salary$race)
dataset_salary$gender<- as.factor(dataset_salary$gender)
dataset_salary$`native-country` <- as.factor(dataset_salary$`native-country`)
dataset_salary$income <- as.factor(dataset_salary$income)

# how many NAs do we have?

sapply(dataset_salary,function(x) sum(is.na(x)))
# 0 NA's

# let us see what is the distribution of the income indicator in the dataset.

par(mfrow = c(1,1))
barplot(table(dataset_salary$income), main = "Frequency of Having Income over 50k")

#decomposition of variables 
hist(dataset_salary$age)
hist(dataset_salary$`educational-num`)
hist(dataset_salary$`hours-per-week`)
hist(dataset_salary$`capital-gain`)
hist(dataset_salary$`capital-loss`)

usethis::use_git()
