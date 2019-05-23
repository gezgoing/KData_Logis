#=================#
# 03. 시계열 분석 #
#=================#

##################
# 2. 시계열 모형 #
##################

## (5) R을 이용한 시계열 분석 사례

# (a) Fitting an AR model

# data load
data(lynx)
head(lynx)

# data 탐색
ts.plot(lynx)
head(lynx)
hist(lynx)
hist(log(lynx))
# acf
acf(lynx)

# log transformation
ts.plot(log(lynx))
acf(log(lynx))

# pacf
acf(log(lynx),type="partial")

# AR model 생성
llynx.ar <- ar.yw(log(lynx))

# 결과 확인
names(llynx.ar)
llynx.ar$order.max

# 모델의 차수를 결정
ts.plot(llynx.ar$aic, main="AIC for Log(Lynx)")
llynx.ar$order
llynx.ar$ar

# Visualization
ts.plot(log(lynx) - llynx.ar$resid)
lines(log(lynx), col=2)
plot(forecast(llynx.ar))


# (b) Fitting an ARIMA model in R

# data load
install.packages("fpp")
library(fpp)
data(elecequip)
?elecequip
# data 탐색
eeadj <- seasadj(stl(elecequip, s.window="periodic"))
plot(eeadj)

# 모델 선택
tsdisplay(diff(eeadj),main="")

fit_310 <- Arima(eeadj, order=c(3,1,0))
fit_410 <- Arima(eeadj, order=c(4,1,0))
fit_210 <- Arima(eeadj, order=c(2,1,0))
fit_311 <- Arima(eeadj, order=c(3,1,1))
fit_310$aic;fit_410$aic;fit_210$aic;fit_311$aic;

# 잔차 확인
Acf(residuals(fit_311))
Box.test(residuals(fit_311), lag=24, fitdf=4, type="Ljung")

# 모델을 이용한 예측
plot(forecast(fit_311))

# (c) auto.arima와 비교

# auto.arima가 위의 과정과 같은 결과를 얻는 것을 확인
library(forecast)
fit_eeadj<-auto.arima(eeadj)
plot(forecast(fit_eeadj))
summary(fit_eeadj)
summary(fit_311)

# AR Model의 data lynx를 auto.arima에 적용
fit_lynx<-auto.arima(lynx)
plot(lynx-fit_lynx$residuals)
lines(lynx, col=2)
summary(fit_lynx)
plot(forecast(fit_lynx))

# (d) 분해시계열(decompose)

# data load
library(zoo)
kospi<-read.csv("kospi.csv")
str(kospi)

# time series 생성
kospi_ts<-ts(kospi$close,frequency=250)
kospi_decomp<-decompose(kospi_ts)
plot(kospi_decomp)
str(kospi_decomp)
