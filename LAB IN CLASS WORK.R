mtcars
head(mtcars)
str(mtcars)
model1<-lm(mpg~ cyl +wt , data=mtcars)
model1
plot(model1, pch = 18, col='yellow',which= c(2))
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)
round(CooksDistance ,5)
sort(round(CooksDistance,5))
install.packages("glimpse")
library(dplyr)
library(ISLR)
head(Hitters)
dim(Hitters)
is.na(Hitters)
HitterData<- na.omit(Hitters)
dim(HitterData)
glimpse(HitterData)
head(HitterData)
SalaryPredictModel1 <- lm(Salary ~., data =HitterData)
summary(SalaryPredictModel1)
cooksD <- cooks.distance(SalaryPredictModel1)
influential <-cooksD[(cooksD > (3*mean(cooksD, na.rm = TRUE)))]
influential
names_of_influential <- names(influential)
names_of_influential
outliers <- HitterData[names_of_influential ,]
Hitters_Without_Outliers <- HitterData %>% anti_join(outliers)

SalaryPredictModel2<- lm(Salary ~., data =Hitters_Without_Outliers)
summary(SalaryPredictModel2)
