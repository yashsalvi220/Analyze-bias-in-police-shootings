shooting.df<-read.csv("shootings_1.csv") 

#pie chart
library(plyr)
count(shooting.df, vars = "race")
values <-  c(93, 1298, 902, 78, 48, 2476)
Race <-  c("Asian","Black","Hispanic","Native", "Other","White")
pct <- round(values/sum(values)*100)
Race <- paste(Race, pct)
Race <- paste(Race,"%",sep="")
pie(values, labels = Race)

#Bar chart
values2 <- c(93/0.059, 1298/0.134, 902/0.185, 48/0.013, 2476/0.763)
barplot(values2, main="Relative frequency by race",xlab="Race",ylab="Relative value",names.arg = c("Asian", "Black", "Hispanic", "Native", "White"),border="red",col="blue",density=10)


#Logistic Regression

train.index<-sample(c(1:dim(shooting.df)[1]), dim(shooting.df)[1]*0.6) 
train.df<-shooting.df[train.index,]
valid.df<-shooting.df[-train.index,]


#flee

logit.reg3<-glm(flee_factor ~ armed_factor + black + mental_illness, data=train.df, family="binomial")
options(scipen=999)
summary(logit.reg3)

exp(coef(logit.reg3))

logit.reg3.pred<-predict(logit.reg3, valid.df, type="response") 
data.frame(actual=valid.df$flee_factor[1:5], predicted = logit.reg3.pred[1:5]) 

#attack

logit.reg4<-glm(attack~ armed_factor + white + black + flee_factor + mental_illness, data=train.df, family="binomial")
options(scipen=999)
summary(logit.reg4)

exp(coef(logit.reg4))

logit.reg4.pred<-predict(logit.reg4, valid.df, type="response") 
data.frame(actual=valid.df$attack[1:5], predicted = logit.reg4.pred[1:5]) 


logit.regb<-glm(black ~ armed_factor + attack + flee_factor + mental_illness, data=train.df, family="binomial")
options(scipen=999)
summary(logit.regb)

exp(coef(logit.regb))

logit.regb.pred<-predict(logit.regb, valid.df, type="response") 
data.frame(actual=valid.df$black[1:5], predicted = logit.regb.pred[1:5]) 

logit.regw<-glm(white ~ armed_factor + attack + flee_factor + mental_illness, data=train.df, family="binomial")
options(scipen=999)
summary(logit.regb)

exp(coef(logit.regb))

logit.regw.pred<-predict(logit.regw, valid.df, type="response") 
data.frame(actual=valid.df$white [1:5], predicted = logit.regw.pred[1:5]) 


#Association Rules

library(arules)
ct.df<-read.csv("shootingas.csv")
# convert to matrix
ct.mat<-as(ct.df,"matrix")
# convert it to a transactions-based format
ct.trans<-as(ct.mat, "transactions")

ct.trans
inspect(ct.trans) 

##1.2	Draw an item frequency plot and answer which statistics course was the most popular course 
itemFrequencyPlot(ct.trans) 


rules1 <- apriori (ct.trans, parameter=list(support=0.1, confidence=0.30, target="rules"), appearance = list (rhs=c("black","white","hispanic","asian","native")))
inspect(rules1)
inspect(head(sort(rules1, by="lift"),10))




#Load the 2nd dataset
policshootings <-read.csv("fatal-police-shootings-data.csv")
policshootings
#removing missing data fields
#checking for missing fields
any(is.na(policshootings))
sum(is.na(policshootings))

#exploring dataset
summary(policshootings)
str(policshootings)

#Explore dataset

#1. Explore number of male and female in the population
#download appropriate library for analysis
library(graphics)
#check the datatype of gender
policshootings$gender
str(policshootings$gender)
#create table to use barplot to find number of males vs females
gender<- table(policshootings$gender)
#to check if table has any null values
sum(is.na(gender))
#bar plot of gender
barplot(gender[order(gender)],horiz = TRUE,
        las = 1, col = c( "red","green"), border= NA, main = "Gender proportion",
        xlab = "Number of males with respect to females")

#creating a data column  to store male and female in numeric format as 1 for males  and 0 for females
str(policshootings$gender)
#creating a new column gender 1 to store male and female in the form of numeric as 1 and 0
#Male is 1, Female is 0
policshootings$gender1 <- ifelse(policshootings$gender== "M",1,0)
str(policshootings$gender1)
policshootings

#2. Explore number of castes
#check the datatype of race
str(policshootings$race)
#identifying any null values
sum(is.na(policshootings$race))


#Convert race into factors for better analysis
policshootings$race1<- as.factor(policshootings$race)
str(policshootings$race1)


#3.Explore number of deaths  taken by years and months
#Convert date from character to date format
#download necessary libary to faciliate the process
library(lubridate)
#convert date to dateformat
policshootings$year<- mdy(policshootings$date)
class(policshootings$year)
#consider the months of the shootings
policshootings$month<- month(policshootings$year, label = TRUE)
#consider the year of the shootings
policshootings$year1<- year(policshootings$year)
#consider the year and month of the shootings
library(zoo)
policshootings$year2<- as.yearmon(policshootings$year, "%m/%Y")
policshootings
#bar graph for police shootings in months
monthplot<- table(policshootings$month)
barplot(monthplot[order(monthplot)], horiz= FALSE,
        las = 1, col = rainbow(12), border= NA, main = "Shootings by months",
        xlab = " Months", ylab = "Frequency of shootings")
#bar graph for police shootings in years
yearplot<- table(policshootings$year1)
barplot(yearplot[order(yearplot)], horiz= TRUE,
        las = 1, col = rainbow(6), border= NA, main = "Shootings by years",
        xlab = " Frequency of shootings", ylab = "Years")

#4. Explore the number of times body camera worn associated with police shootings
#check class of body camera
class(policshootings$body_camera)
str(policshootings$body_camera)
#convert bodycamera values to numeric values for better analysis
policshootings$body_camera1<- as.numeric(policshootings$body_camera)
summary(policshootings$body_camera1)
count(policshootings$body_camera1)
hist(policshootings$body_camera1)

#5. Explore number of people shot to the number of states
#bar plot to explore number of people shot to the number of states
str(policshootings$state)
stategraph<- table(policshootings$state)
barplot(stategraph[order(stategraph)],horiz = FALSE,
        las = 1, col = "grey", border= NA, main = "State proportion",
        xlab = "Number of states")


#6. Explore number of people shot to the different age groups
#grouping age groups into ranges
policshootings$age1<- cut(policshootings$age, breaks = c(0,18,30,50, Inf), labels = c('0-18','18-30','30-50','50+'))
plot(policshootings$age1)
#bargraph to understand age distribution in number of shootings
agegroup<- table(policshootings$age1)
barplot(agegroup[order(agegroup)], hori= FALSE, las = 1, col = rainbow(4), border = NA, main = "Age distribution", 
             xlab="Age groups", ylab= "Number of shootings")
#build a violin plot to analyze the age distribution in depth
#convert datatype of age from integer to  numeric for generating violin plots
str(policshootings$age)
policshootings$age2<- as.numeric(policshootings$age)
str(policshootings$age2)
#installing required packages
library(ggplot2)
#violingraph to understand age distribution
ggplot(policshootings, aes(x="Age",y =age2)) + geom_violin() 
#boxplot to understand age distribution
boxplot(policshootings$age2, horizontal = TRUE, axes = FALSE, staplewex = 1)
text(x=fivenum(policshootings$age2), labels =fivenum(policshootings$age2), y=1.25)

#Convert variables into dummy variables for time series analysis
library(dplyr)
model.matrix(~race1 -1, policshootings)

data_frame_race<- select(policshootings, year2, race1)
data_frame_race1_dummy <- data.frame(data_frame_race[ , ! colnames(data_frame_race) %in% "race1"],       # Create dummy data
                                     model.matrix( ~ race1 - 1, data_frame_race))
names(data_frame_race1_dummy)
names(data_frame_race1_dummy)[1] <- "year3"
names(data_frame_race1_dummy)
##creating time series data to predict the number of black shootings for the upcoming years
library(forecast)
timeseries<- select(data_frame_race1_dummy, year3,race1B)

#decision tree model
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")
library(rpart)
library(rpart.plot)
library(caret)

policshootings<- cbind(policshootings,timeseries)
summary(policshootings)
policeshootings<- policshootings[-c(1:4,6:10,14,16,18:20,22,24, 26:27)]
set.seed(1)
decision_train_index<- sample(c(1:dim(policeshootings)[1]),dim(policeshootings)[1]*0.6)
decision_train.df <- policeshootings[decision_train_index,]
decision_test.df<- policeshootings[-decision_train_index,]

default.ct<- rpart(race1B~., data =decision_train.df, method = "class")
prp(default.ct, type=1, under= TRUE, split.font = 1)

default.ct.point.pred.train<- predict(default.ct,decision_test.df, type= "class")

