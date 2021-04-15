file = "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
#data
dataset_salary = read.csv(file, header=FALSE)

names(dataset_salary) = c("age","workclass","fnlwgt", "education","educational-num","marital-status",
                          "occupation","relationship","race","gender","capital-gain","capital-loss",
                          "hours-per-week","native-country","income")

head(dataset_salary,10)
View(dataset_salary)


#descriptive statistics of the data
summary(dataset_salary)

#structure of the data
str(dataset_salary)

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


