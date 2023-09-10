# library
library(forecast)
library(tseries)
library(TTR)
library(caret)

# 서울시전력사용량 데이터 불러오기
ETD <- read.csv('C:/Users/DODO/R_Projects/시계열자료분석/서울시전력사용량.csv', header=FALSE, sep=",")  

# time series 자료로 변환 
ETD.ts = ts(ETD[,2], frequency = 12, start = c(1996,1))
head(ETD.ts)
tail(ETD.ts)

#train - test 분리
test = ETD.ts[301:312]
ETD.ts = ETD.ts[1:300]
ETD.ts = ts(ETD.ts, frequency = 12, start = c(1996,1)) # 2020년까지 
test = ts(test, frequency = 12, start = c(2021,1)) # 2021년
test

# 시각화
plot(ETD.ts) 

x = lowess(ETD.ts, f=0.1)
lines(x, col='red')

ma.cen <- ma(ETD.ts, order=12, centre=TRUE)
lines(ma.cen, col='blue')

# <분산의 안정화: Box-Cox>
boxcox(ETD.ts~time(ETD.ts)) # 점선: 신뢰구간

bc = boxcox(ETD.ts~time(ETD.ts), plotit = F) 
lam = bc$x[which.max(bc$y)]; lam

par(mfrow=c(1,3))
ETD.ts1 = (ETD.ts^lam - 1)/lam # lambda가 0이 아닐때
ETD.ts2 = log(ETD.ts) # lambda가 0일때
plot(ETD.ts)
plot(ETD.ts1)
plot(ETD.ts2)

# 추세 회귀 함수
tt = 1:length(ETD.ts2) 
reg = lm(ETD.ts2~tt+I(tt^2))

par(mfrow=c(1,1))
plot(tt, ETD.ts2, type='l')
lines(reg$fitted.values, col = 'blue')  # 회귀직선 시각화

# 추세 제거
ETD.ts3 = reg$residuals
ETD.ts3 = ts(ETD.ts3, start = start(ETD.ts2), frequency = frequency(ETD.ts2))
plot(ETD.ts3)

# 계절조정
DD = decompose(ETD.ts3)
plot(DD)
DD$seasonal
ETD.ts4 = DD$x - DD$seasonal
plot(ETD.ts4)

# <차분 차수 결정 및 차분 시행>
plot(ETD.ts4)
plot(diff(ETD.ts4)) 
plot(diff(ETD.ts4, differences=2)) 

acf(ETD.ts4) 
acf(diff(ETD.ts4, differences=1))
acf(diff(ETD.ts4, differences=2)) 

# <adf test>
# 차분 1번 
kpss.test(ETD.ts4, null="Trend", lshort=T)
kpss.test(diff(ETD.ts4, differences=1), null="Trend", lshort=T)

# <acf, pacf를 보고 모형의 차수 선택>
# 대략적으로 ar: 3~4, ma: 1~3으로 고려 
x = diff(ETD.ts4, differences=1)
par(mfrow=c(1,2))
acf(x) 
pacf(x) 

out1 = arima(ETD.ts4, order=c(3,1,1))
out2 = arima(ETD.ts4, order=c(3,1,2))
out3 = arima(ETD.ts4, order=c(3,1,3))
out4 = arima(ETD.ts4, order=c(4,1,1))
out5 = arima(ETD.ts4, order=c(4,1,2))
out6 = arima(ETD.ts4, order=c(4,1,3))

AIC(out1, out2, out3, out4, out5, out6) # aic가 가장 작은 모형 선택

# <과다적합 분석>
out = arima(ETD.ts4, order=c(4,1,3)); out # ar4는 유의하지 않음, ma4도 고려 
out = arima(ETD.ts4, order=c(3,1,4)); out # ma4도 유의하지 않음
out = arima(ETD.ts4, order=c(3,1,3)); out

# <잔차분석 함수> 
tsdiag(out) 

# <함수로 예측>
pred = forecast(out, h=12)
pred_2021 = as.data.frame(pred)[,1]

par(mfrow=c(1,1))
plot(forecast(out, h=12)) # 예측값 시각화

# <역변환> 
pred_2021 = pred_2021 + DD$figure # 계절성분 더해주기 

tt = 301:312
df <- data.frame('tt'=tt,
                 'I(tt^2)'=I(tt^2))

trend_2021 = predict(reg, df)

pred_2021 = pred_2021 + trend_2021 # 추세성분 더해주기 
pred_2021 = exp(pred_2021) # 로그변환 한거 역변환

plot(x = c(1:12), test, type='l', col = 'red')
lines(x = c(1:12) ,y = pred_2021) 
title(main="ETD 2021")

result_rmse = RMSE(pred_2021, test)
result_rmse

