file = "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
#data
dataset_salary = read.csv(file, header=FALSE)

names(dataset_salary) = c("age","workclass","fnlwgt", "education","educational-num","marital-status",
                          "occupation","relationship","race","gender","capital-gain","capital-loss",
                          "hours-per-week","native-country","income")

head(dataset_salary,10)

mean(dataset_salary$age)

#description of sample
mean(dataset_salary$age)

table(dataset_salary$gender)
table(dataset_salary$income)
table(dataset_salary$relationship)





#