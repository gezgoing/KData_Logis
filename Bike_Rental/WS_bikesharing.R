# 출처: https://github.com/Punya-Swaroop-Sirgiri/Bike-Rental-Demand-Prediction-R

# Install and Load packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# importing training Data and test data
df=read.csv("Bikesharing_train.csv")

set.seed(415)
ind<-sample(2,nrow(df),prob=c(0.7,0.3),replace=T)
table(ind)

train<-df[ind==1,]
test<-df[ind==2,]

train$trtt<-'tr'
test$trtt<-'tt'

head(train,2)
substr(train$datetime[1],12,13)
table(substr(train$datetime,12,13))
table(substr(test$datetime,12,13))

substr(train$datetime[1],6,10)
table(substr(train$datetime,6,10))
table(substr(test$datetime,6,10))


# names of all the variables
names(train)
names(test)

head(train,2)
head(test,2)

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
par(mfrow=c(1,1))

# Contribution of Weather: Weather1 has more contribution=65.7%
prop.table(table(swaroop1$weather))

# converting discrete variables into factors
swaroop1$season=as.factor(swaroop1$season)
swaroop1$weather=as.factor(swaroop1$weather)
swaroop1$holiday=as.factor(swaroop1$holiday)
swaroop1$workingday=as.factor(swaroop1$workingday)

# ----------------------------------
# Multiple Reg
# ----------------------------------
train<-swaroop1[swaroop1$trtt=='tr',]
test<-swaroop1[swaroop1$trtt=='tt',]

names(train)
reg1<-lm(count~.,data=train[,c(2:9,12)])

library(Metrics)
quantile(train$count)
sd(train$count)
rmse(predict(reg1,train),train$count)
rmse(predict(reg1,train),train$count)/sd(train$count)
# 0.8511898

mape(predict(reg1,train),train$count)

quantile(test$count)
sd(test$count)
rmse(predict(reg1,test),test$count)
rmse(predict(reg1,test),test$count)/sd(test$count)
# 0.848638

mape(predict(reg1,test),test$count)

# ----------------------------------
# Hypothesis testing I: Hourly Trend
# ----------------------------------
swaroop1$hour=substr(swaroop1$datetime,12,13)
swaroop1$hour=as.factor(swaroop1$hour)
swaroop1$datetime[1]
substr(swaroop1$datetime[1],12,13)

# separating swaroop1 into train and test
train<-swaroop1[swaroop1$trtt=='tr',]
test<-swaroop1[swaroop1$trtt=='tt',]

# plot of hour and users count
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
train<-swaroop1[swaroop1$trtt=='tr',]
test<-swaroop1[swaroop1$trtt=='tt',]
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
train<-swaroop1[swaroop1$trtt=='tr',]
test<-swaroop1[swaroop1$trtt=='tt',]

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
train<-swaroop1[swaroop1$trtt=='tr',]
test<-swaroop1[swaroop1$trtt=='tt',]

par(mfrow=c(3,1))
# Plot of count and month
boxplot(train$count~train$month,xlab="month", ylab="count")

# plot of casual users and month
boxplot(train$casual~train$month, xlab="month", ylab="casual users")

#plot of registered users and month
boxplot(train$registered~train$month, xlab="month", ylab="registered users")
par(mfrow=c(1,1))

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
swaroop1$hr_reg=0
swaroop1$hr_reg[swaroop1$hour<=7]=1
swaroop1$hr_reg[swaroop1$hour>=22]=2
swaroop1$hr_reg[swaroop1$hour>9 & swaroop1$hour<18]=3
swaroop1$hr_reg[swaroop1$hour==8]=4
swaroop1$hr_reg[swaroop1$hour==9]=5
swaroop1$hr_reg[swaroop1$hour==20 | swaroop1$hour==21]=6
swaroop1$hr_reg[swaroop1$hour==19 | swaroop1$hour==18]=7

# Running decision tree for casual users 
prasad=rpart(casual~hour, data=train)
fancyRpartPlot(prasad)

# Creating bins for casual users
swaroop1$hr_cas=0
swaroop1$hr_cas[swaroop1$hour<=8]=1
swaroop1$hr_cas[swaroop1$hour==9 | swaroop1$hour==10]=2
swaroop1$hr_cas[swaroop1$hour>=21]=3
swaroop1$hr_cas[swaroop1$hour>=11 & swaroop1$hour<21]=4

# Running decision tree for temperature 
par(mfrow=c(2,1))
suneetha=rpart(registered~temp,data=train)
fancyRpartPlot(suneetha)

# Running decision tree for temperature
pranathi=rpart(casual~temp,data=train)
fancyRpartPlot(pranathi)
par(mfrow=c(1,1))

# Creating bins for temperature:Registered
swaroop1$temp_reg=0
swaroop1$temp_reg[swaroop1$temp<12]=1
swaroop1$temp_reg[swaroop1$temp>=12 & swaroop1$temp<23]=2
swaroop1$temp_reg[swaroop1$temp>=23 & swaroop1$temp<30]=3
swaroop1$temp_reg[swaroop1$temp>=30]=4

# creating bins for temp:Casual
swaroop1$temp_cas=0
swaroop1$temp_cas[swaroop1$temp<14]=1
swaroop1$temp_cas[swaroop1$temp>=15 & swaroop1$temp<23]=2
swaroop1$temp_cas[swaroop1$temp>=23 & swaroop1$temp<29]=3
swaroop1$temp_cas[swaroop1$temp>=29]=4

# creating bins for year (Quarterly)
swaroop1$qtr_bin[swaroop1$year=='2011']=1
swaroop1$qtr_bin[swaroop1$year=='2011' & swaroop1$month>3]=2
swaroop1$qtr_bin[swaroop1$year=='2011' & swaroop1$month>6]=3
swaroop1$qtr_bin[swaroop1$year=='2011' & swaroop1$month>9]=4
swaroop1$qtr_bin[swaroop1$year=='2012']=5
swaroop1$qtr_bin[swaroop1$year=='2012' & swaroop1$month>3]=6
swaroop1$qtr_bin[swaroop1$year=='2012' & swaroop1$month>6]=7
swaroop1$qtr_bin[swaroop1$year=='2012' & swaroop1$month>9]=8

# finding percentage of observations in each bin
table(swaroop1$qtr_bin)
prop.table(table(swaroop1$qtr_bin))

# creating bins for days
unique(swaroop1$holiday)
unique(swaroop1$workingday)

swaroop1$day_bin=0
swaroop1$day_bin[swaroop1$holiday==0 & swaroop1$workingday==0]="weekend"
swaroop1$day_bin[swaroop1$holiday==1]="holiday"
swaroop1$day_bin[swaroop1$holiday==0 & swaroop1$workingday==1]="working day"

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
swaroop1$hr_reg=as.factor(swaroop1$hr_reg)
swaroop1$hr_cas=as.factor(swaroop1$hr_cas)
swaroop1$temp_reg=as.factor(swaroop1$temp_reg)
swaroop1$temp_cas=as.factor(swaroop1$temp_cas)
swaroop1$qtr_bin=as.factor(swaroop1$qtr_bin)
swaroop1$day_bin=as.factor(swaroop1$day_bin)
# swaroop1$bucket_year=as.factor(swaroop1$bucket_year)
swaroop1$weekend=as.factor(swaroop1$weekend)


str(swaroop1)

# As there are many outliers, we perform logtransformation: plus 1 is to avoid log(0)
swaroop1$reg1=swaroop1$registered+1
swaroop1$cas1=swaroop1$casual+1
swaroop1$logcas=log(swaroop1$cas1)
swaroop1$logreg=log(swaroop1$reg1)

# separate swaroop1 into train and test
train<-swaroop1[swaroop1$trtt=='tr',]
test<-swaroop1[swaroop1$trtt=='tt',]


# plot of logreg and weather
boxplot(train$logreg~train$weather,xlab="weather", ylab="registered users")

# plot of logreg and season
boxplot(train$logreg~train$season,xlab="season", ylab="registered users")


# ------------------------
# Multi Reg
# ------------------------
names(train)
reg2<-lm(logreg~.,data=train[,-c(1,7,10:13,16,17,25:27)])

library(Metrics)
quantile(train$registered)
quantile(exp(train$logreg)-1)
sd(train$registered)

exp(predict(reg2,train[1:10,]))-1
train$registered[1:10]

rmse(exp(predict(reg2,train))-1,train$registered)
rmse(exp(predict(reg2,train))-1,train$count)/sd(train$count)
# 0.6292599 vs. 0.8511898

mape(predict(reg1,train),train$count)
mape(exp(predict(reg2,train))-1,train$registered)

quantile(test$registered)
sd(test$registered)
rmse(exp(predict(reg2,test))-1,test$registered)

rmse(exp(predict(reg2,test))-1,test$registered)/sd(test$registered)
# 0.5512029 vs. 0.848638
rmse(predict(reg1,test),test$count)/sd(test$count)

mape(exp(predict(reg2,test))-1,test$registered)
mape(predict(reg1,test),test$count)


# Building randomforest model I
head(train,2)
names(train)
set.seed(415)
library(randomForest)

# 1 min
system.time(
  fit1 <- randomForest(logreg ~ season+holiday+workingday+weather+atemp+humidity+windspeed+hour+day+year+hr_reg+qtr_bin+day_bin+weekend+temp_reg, data=train,importance=TRUE, ntree=250)
)
# omitted var: datetime, temp, casual, registered, count, month, <<<hr_cas>>>,<<<temp_cas>>>,reg1,cas1,logcas


# predicting test data's response variable logreg
pred1_tr=predict(fit1,train,type='response')
pred1_tt=predict(fit1,test,type='response')

pred1_tr[1:10]
train$logreg[1:10]

library(Metrics)
### For Train Set
quantile(train$registered)
sd(train$registered)
rmse(exp(pred1_tr)-1,train$registered)
# 20.30154
rmse(exp(pred1_tr)-1,train$registered)/sd(train$registered)
# 0.1339228

mape(exp(pred1_tr)-1,train$registered)
# 0.1238327

### For Test Set
quantile(test$registered)
sd(test$registered)
rmse(exp(pred1_tt)-1,test$registered)
# 35.71363

mape(exp(pred1_tt)-1,test$registered)
# 0.2343347


# Bulding randomforest model II: logcas
set.seed(415)
names(train)

# 1 min
system.time(
  fit2 <- randomForest(logcas ~ season+holiday+workingday+weather+atemp+humidity+windspeed+hour+day+year+hr_cas+temp_cas+qtr_bin+day_bin+weekend, data=train,importance=TRUE, ntree=250)
)
# omitted var: datetime, temp, casual, registered, count, month, <<<hr_reg>>>,<<<temp_reg>>>,reg1,cas1,logreg


# predicting test data's response variable logcas
pred2_tr=predict(fit2,train,type='response')
pred2_tt=predict(fit2,test,type='response')

# library(Metrics)

### For Train Dataset
quantile(train$casual)
sd(train$casual)

rmse(exp(pred2_tr)-1,train$casual)
# 8.962211
rmse(exp(pred2_tr)-1,train$casual)/sd(train$casual)
# 0.1783849

mape(exp(pred2_tr)-1,train$casual)
# 0.2978666


### For Test Dataset
quantile(test$casual)
sd(test$casual)

rmse(exp(pred2_tt)-1,test$casual)
# 14.81157
rmse(exp(pred2_tt)-1,test$casual)/sd(test$casual)
# 0.3005814

mape(exp(pred2_tt)-1,test$casual)
# 0.5565638


### For count
quantile(test$count)
sd(test$count)

rmse(exp(pred1_tt)-1+exp(pred2_tt)-1,test$count)
# 42.19409
rmse(exp(pred1_tt)-1+exp(pred2_tt)-1,test$count)/sd(test$count)
# 0.2352507

mape(exp(pred1_tt)-1+exp(pred2_tt)-1,test$count)
# 0.2317485

