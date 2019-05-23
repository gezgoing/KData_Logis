# Install and Load packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# importing training Data and test data
train=read.csv("Bikesharing_train.csv")
test=read.csv("Bikesharing_test.csv")

# names of all the variables
names(train)
names(test)

head(train,2)
head(test,2)

# As there are only 9 variables in test unlike train in which there are 12, we make test data contain 12 by adding up 3 columns(casual,registered and count) with all data values=0
test$registered=0
test$casual=0
test$count=0

# combine the train and test to interpret the independent variable distribution well
swaroop1=rbind(train,test)

# structure of swaroop1
str(swaroop1)

# counting the number of missing values
sapply(swaroop1, function(x) sum(is.na(x)))
# No missing values

# frequency tables for each numeric variable
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(swaroop1$season)
hist(swaroop1$weather)
hist(swaroop1$humidity)
hist(swaroop1$holiday)
hist(swaroop1$workingday)
hist(swaroop1$temp)
hist(swaroop1$atemp)
hist(swaroop1$windspeed)

# Contribution of Weather: Weather1 has more contribution=65.7%
prop.table(table(swaroop1$weather))

# converting discrete variables into factors
swaroop1$season=as.factor(swaroop1$season)
swaroop1$weather=as.factor(swaroop1$weather)
swaroop1$holiday=as.factor(swaroop1$holiday)
swaroop1$workingday=as.factor(swaroop1$workingday)

# ----------------------------------
# Hypothesis testing I: Hourly Trend
# ----------------------------------
swaroop1$hour=substr(swaroop1$datetime,12,13)
swaroop1$hour=as.factor(swaroop1$hour)
swaroop1$datetime[1]
substr(swaroop1$datetime[1],12,13)

# separating swaroop1 into train and test
train=swaroop1[as.integer(substr(swaroop1$datetime,9,10))<20,]
test=swaroop1[as.integer(substr(swaroop1$datetime,9,10))>19,]
swaroop1$datetime[1]
substr(swaroop1$datetime[1],9,10)

# plot of hour and users count
par(mfrow=c(1,1))
head(train,2)
boxplot(train$count~train$hour, xlab="hour", ylab="user count")

# plot of casual users and hour
boxplot(train$casual~train$hour, xlab="hour", ylab="casual users")

# plot of registered users and hour
boxplot(train$registered~train$hour, xlab="hour",ylab="Registered users")
# from the plots, registered and count plots are similar. 


# Casual plot is different from the other two. So hour is a significant variable.
# As plots have many outliers. Performing log transformation: 
boxplot(log(train$count)~train$hour, xlab="hour",ylab="Log(count)")


# ----------------------------------
# hypothesis testing II: Daily trend adding up a new column day (월요일~일요일) to swaroop1
# ----------------------------------
substr(swaroop1$datetime[1],1,10)
date=substr(swaroop1$datetime,1,10)
days<-factor(weekdays(as.Date(date)),levels=c('월요일','화요일','수요일','목요일','금요일','토요일','일요일'))
swaroop1$day=days
weekdays(as.Date(date[1]))

# again separating the data into train and test 
train=swaroop1[as.integer(substr(swaroop1$datetime,9,10))<20,]
test=swaroop1[as.integer(substr(swaroop1$datetime,9,10))>19,]
head(train,2)
str(train)

# plots of day and dependent variables
boxplot(train$count~train$day, xlab="day", ylab="users count")
boxplot(train$casual~train$day, xlab="day", ylab="casual users")
boxplot(train$registered~train$day, xlab="day", ylab="registered users")

# ----------------------------------
# Hypothesis III: Rain
# ----------------------------------
boxplot(train$registered~train$weather, xlab="weather", ylab="registered users")
boxplot(train$casual~train$weather, xlab="weather", ylab="casual users")
boxplot(train$count~train$weather, xlab="weather", ylab="users count")


# ----------------------------------
# Hypothesis IV: Temperature, windspeed and Humidity=As these are numeric variables, we find correlation
# ----------------------------------
sweety = train[,c('casual','registered','count','temp','atemp','humidity','windspeed')]
cor(sweety)

# Variable temp is positively correlated with dependent variables (casual is more compared to registered)
# Variable atemp is highly correlated with temp.
# Windspeed has lower correlation as compared to temp and humidity


# ----------------------------------
# Hypothesis V: Time(trend of bike demand over year)=adding year column to swaroop1 and separate swaroop1 into train and test
# ----------------------------------
swaroop1$year=substr(swaroop1$datetime,1,4)
swaroop1$year=as.factor(swaroop1$year)
train=swaroop1[as.integer(substr(swaroop1$datetime,9,10))<20,]
test=swaroop1[as.integer(substr(swaroop1$datetime,9,10))>19,]

# Plot of count and year
boxplot(train$count~train$year,xlab="year", ylab="count")

# plot of casual users and year
boxplot(train$casual~train$year, xlab="year", ylab="casual users")

# plot of registered users and year
boxplot(train$registered~train$year, xlab="year", ylab="registered users")


# ----------------------------------
# Hypothesis VI:month
# ----------------------------------
swaroop1$month=substr(swaroop1$datetime,6,7)
swaroop1$month=as.integer(swaroop1$month)
train=swaroop1[as.integer(substr(swaroop1$datetime,9,10))<20,]
test=swaroop1[as.integer(substr(swaroop1$datetime,9,10))>19,]

par(mfrow=c(3,1))
# Plot of count and month
boxplot(train$count~train$month,xlab="month", ylab="count")

# plot of casual users and month
boxplot(train$casual~train$month, xlab="month", ylab="casual users")

#plot of registered users and month
boxplot(train$registered~train$month, xlab="month", ylab="registered users")

# Feature engineering:Hour bins
# convert hour to integer in both test and train
train$hour=as.integer(train$hour)
test$hour=as.integer(test$hour)


# Running decision tree for registered users
lakshmi=rpart(registered~hour,data=train)
par(mfrow=c(1,1))
library(rattle)
fancyRpartPlot(lakshmi)


# Combining train and test
swaroop1=rbind(train,test)

# Creating bins for registered users
swaroop1$bucket_reg=0
swaroop1$bucket_reg[swaroop1$hour<8]=1
swaroop1$bucket_reg[swaroop1$hour>=22]=2
swaroop1$bucket_reg[swaroop1$hour>9 & swaroop1$hour<18]=3
swaroop1$bucket_reg[swaroop1$hour==8]=4
swaroop1$bucket_reg[swaroop1$hour==9]=5
swaroop1$bucket_reg[swaroop1$hour==20 | swaroop1$hour==21]=6
swaroop1$bucket_reg[swaroop1$hour==19 | swaroop1$hour==18]=7

# Running decision tree for casual users 
prasad=rpart(casual~hour, data=train)
fancyRpartPlot(prasad)

# Creating bins for casual users
swaroop1$bucket_cas=0
swaroop1$bucket_cas[swaroop1$hour<=8]=1
swaroop1$bucket_cas[swaroop1$hour==9 | swaroop1$hour==10]=2
swaroop1$bucket_cas[swaroop1$hour>10 & swaroop1$hour<=20]=3
swaroop1$bucket_cas[swaroop1$hour>21]=4

# Running decision tree for temperature 
par(mfrow=c(2,1))
suneetha=rpart(registered~temp,data=train)
fancyRpartPlot(suneetha)

# Running decision tree for temperature
pranathi=rpart(casual~temp,data=train)
fancyRpartPlot(pranathi)

# Creating bins for temperature:Registered
swaroop1$temp_reg=0
swaroop1$temp_reg[swaroop1$temp<13]=1
swaroop1$temp_reg[swaroop1$temp>=13 & swaroop1$temp<23]=2
swaroop1$temp_reg[swaroop1$temp>=23 & swaroop1$temp<30]=3
swaroop1$temp_reg[swaroop1$temp>=30]=4

# creating bins for temp:Casual
swaroop1$temp_cas=0
swaroop1$temp_cas[swaroop1$temp<15]=1
swaroop1$temp_cas[swaroop1$temp>=15 & swaroop1$temp<23]=2
swaroop1$temp_cas[swaroop1$temp>=23 & swaroop1$temp<30]=3
swaroop1$temp_cas[swaroop1$temp>=30]=4

# creating bins for year (Quarterly)
swaroop1$bucket_year[swaroop1$year=='2011']=1
swaroop1$bucket_year[swaroop1$year=='2011' & swaroop1$month>3]=2
swaroop1$bucket_year[swaroop1$year=='2011' & swaroop1$month>6]=3
swaroop1$bucket_year[swaroop1$year=='2011' & swaroop1$month>9]=4
swaroop1$bucket_year[swaroop1$year=='2012']=5
swaroop1$bucket_year[swaroop1$year=='2012' & swaroop1$month>3]=6
swaroop1$bucket_year[swaroop1$year=='2012' & swaroop1$month>6]=7
swaroop1$bucket_year[swaroop1$year=='2012' & swaroop1$month>9]=8

# finding percentage of observations in each bin
table(swaroop1$bucket_year)
prop.table(table(swaroop1$bucket_year))

# creating bins for days
unique(swaroop1$holiday)
unique(swaroop1$workingday)

swaroop1$bucket_day=0
swaroop1$bucket_day[swaroop1$holiday==0 & swaroop1$workingday==0]="weekend"
swaroop1$bucket_day[swaroop1$holiday==1]="holiday"
swaroop1$bucket_day[swaroop1$holiday==0 & swaroop1$workingday==1]="working day"

# Creating new column weekend
swaroop1$weekend=0
swaroop1$weekend[swaroop1$day=="토요일" | swaroop1$day=="일요일"]=1
table(swaroop1$weekend)


# structure of swaroop1
str(swaroop1)

# convert discrete variables into factors
# swaroop1$season=as.factor(swaroop1$season)
# swaroop1$holiday=as.factor(swaroop1$holiday)
# swaroop1$workingday=as.factor(swaroop1$workingday)
# swaroop1$weather=as.factor(swaroop1$weather)
swaroop1$hour=as.factor(swaroop1$hour)
swaroop1$month=as.factor(swaroop1$month)
# swaroop1$day=as.factor(swaroop1$day)
swaroop1$bucket_day=as.factor(swaroop1$bucket_day)
# swaroop1$bucket_year=as.factor(swaroop1$bucket_year)
swaroop1$weekend=as.factor(swaroop1$weekend)
swaroop1$bucket_reg=as.factor(swaroop1$bucket_reg)
swaroop1$bucket_cas=as.factor(swaroop1$bucket_cas)
swaroop1$temp_reg=as.factor(swaroop1$temp_reg)
swaroop1$temp_cas=as.factor(swaroop1$temp_cas)
swaroop1$bucket_year=as.factor(swaroop1$bucket_year)


str(swaroop1)


# separate swaroop1 into train and test
train=swaroop1[as.integer(substr(swaroop1$datetime,9,10))<20,]
names(train)
test=swaroop1[as.integer(substr(swaroop1$datetime,9,10))>19,]

# As there are many outliers, we perform logtransformation: plus 1 is to avoid log(0)
train$reg1=train$registered+1
train$cas1=train$casual+1
train$logcas=log(train$cas1)
train$logreg=log(train$reg1)
test$logreg=0
test$logcas=0

# plot of logreg and weather
boxplot(train$logreg~train$weather,xlab="weather", ylab="registered users")

# plot of logreg and season
boxplot(train$logreg~train$season,xlab="season", ylab="registered users")


# Building randomforest model I
head(train,2)
names(train)
set.seed(415)
library(randomForest)

# 2 min 20 sec
system.time(
  fit1 <- randomForest(logreg ~ season+holiday+workingday+weather+atemp+humidity+windspeed+hour+day+year+bucket_reg+bucket_year+bucket_day+weekend+temp_reg, data=train,importance=TRUE, ntree=250)
)
# omitted var: datetime, temp, casual, registered, count, month, <<<bucket_cas>>>,<<<temp_cas>>>,reg1,cas1,logcas


# predicting test data's response variable logreg
pred1=predict(fit1,test)
test$logreg=pred1
predict(fit1,train[1:10,],type='response')
train$logreg[1:10]

library(Metrics)
quantile(train$logreg)
sd(train$logreg)
rmse(predict(fit1,train,type='response'),train$logreg)
# 0.1671217

mape(predict(fit1,train,type='response'),train$logreg)
# 0.03786868


# Bulding randomforest model II: logcas
set.seed(415)
names(train)

# 2 min 10 sec
system.time(
  fit2 <- randomForest(logcas ~ season+holiday+workingday+weather+atemp+humidity+windspeed+hour+day+year+bucket_cas+temp_cas+bucket_year+bucket_day+weekend, data=train,importance=TRUE, ntree=250)
)
# omitted var: datetime, temp, casual, registered, count, month, <<<bucket_reg>>>,<<<temp_reg>>>,reg1,cas1,logreg


# predicting test data's response variable logcas
pred2=predict(fit2,test)
test$logcas=pred2

# library(Metrics)
quantile(train$logcas)
sd(train$logcas)
rmse(predict(fit2,train,type='response'),train$logcas)
# 0.257362

mape(predict(fit2,train,type='response'),train$logcas)
# 0.173156

# changing logcas to casual and logreg to registered and predicting count
test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$count=test$casual+test$registered

# predictions column and datetime column
s<-data.frame(datetime=test$datetime,count=test$count)

# generating a csv file of predictions
write.csv(s,file="submit.csv",row.names=FALSE)