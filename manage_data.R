
#loading the data
if(!require(haven)){install.packages("haven")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tibble)){install.packages("tibble")}
if(!require(sjlabelled)){install.packages("sjlabelled")}
if(!require(rstudioapi)){install.packages("rstudioapi")}
if(!require(margins)){install.packages("margins")}

source("manage_data.R", chdir = T)
setwd(dirname(getActiveDocumentContext()$path))  

Data = read_dta("Z12_2016_part.dta")
write.csv(Data, file = "Z12_2016_part.csv")
View(Data)

names(Data)
NewData = subset(Data, select = c("WOJ","B4","B5","B6","B7","WIEK","STAZ_OGOL","TD5","TWO_ROK") )
NewData <- NewData %>% filter(B7 == 10)
NewData <- NewData %>% filter(TWO_ROK > 22200)

# Using model.matrix


head(NewData)

NewData$WOJ <- factor(NewData$WOJ, exclude = NULL)

woj_dekod <- model.matrix(~.-1, data = NewData[ c("WOJ")],
                          contrasts.arg = list(
                            WOJ = contrasts(NewData$WOJ, contrasts = FALSE)
                          ))

head(woj_dekod)

NewData = cbind(NewData,woj_dekod)
head(NewData)
View(NewData)


#descriptive statistics of the data
summary(NewData)

#structure of the data
str(NewData)

#number of rows
nrow(NewData)

###### THE DATA CLEANING PROCESS #####

# chr -> factor/numeric variable

NewData$WIEK2 <- NewData$WIEK2^2
NewData$B4 <- as.factor(NewData$B4)
NewData$B5 <- as.factor(NewData$B5)
NewData$B6 <- as.factor(NewData$B6)
NewData$B7 <- as.factor(NewData$B7)
NewData$WIEK <- as.numeric(NewData$WIEK)
NewData$STAZ_OGOL <- as.numeric(NewData$STAZ_OGOL)
NewData$TD5 <- as.numeric(NewData$TD5)
NewData$TWO_ROK<- as.numeric(NewData$TWO_ROK)

NewData$WOJ02 <- as.factor(NewData$WOJ02)
NewData$WOJ04 <- as.factor(NewData$WOJ04)
NewData$WOJ06 <- as.factor(NewData$WOJ06)
NewData$WOJ08 <- as.factor(NewData$WOJ08)
NewData$WOJ10 <- as.factor(NewData$WOJ10)
NewData$WOJ12 <- as.factor(NewData$WOJ12)
NewData$WOJ14 <- as.factor(NewData$WOJ14)
NewData$WOJ16 <- as.factor(NewData$WOJ16)
NewData$WOJ18 <- as.factor(NewData$WOJ18)
NewData$WOJ20 <- as.factor(NewData$WOJ20)
NewData$WOJ22 <- as.factor(NewData$WOJ22)
NewData$WOJ24 <- as.factor(NewData$WOJ24)
NewData$WOJ26 <- as.factor(NewData$WOJ26)
NewData$WOJ28 <- as.factor(NewData$WOJ28)
NewData$WOJ30 <- as.factor(NewData$WOJ30)
NewData$WOJ32 <- as.factor(NewData$WOJ32)
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

install.packages("truncreg")
library(truncreg)

m <- truncreg(TWO_ROK ~  B4 + B5 +B6 + WIEK + WIEK2 + STAZ_OGOL + TD5 + WOJ14, data = NewData, 
              point = 22200, direction = "left")

summary(m)

#WYNIKI ESTYMACJI:
# z modelu wynika B5 i B6 mogą być nieistotne, zatem należy to sprawdzić:
m2 <- update(m, . ~ . - B5,-B6)
pchisq(-2 * (logLik(m2) - logLik(m)), df = 2, lower.tail = FALSE)
# wniosek: B5 i B6 są jednak istotne


#efekty cząstkowe
install.packages("margins")
library(margins)
x <- lm(TWO_ROK ~  B4 + B5 +B6 + WIEK + WIEK2 + STAZ_OGOL + TD5 + WOJ14, data = NewData)
(m <- margins(x))
summary(m)







