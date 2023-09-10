# library
library(forecast)
library(tseries)
library(TTR)
library(caret)
library(MASS)

# 서울시전력사용량 데이터 불러오기
ETD <- read.csv('C:/Users/DODO/R_Projects/시계열자료분석/서울시전력사용량.csv', header=FALSE, sep=",")  

# time series 자료로 변환 
ETD.ts = ts(ETD[,2], frequency = 12, start = c(1996,1))
head(ETD.ts)
tail(ETD.ts)

# train - test 분리
test = ETD.ts[301:312]
ETD.ts = ETD.ts[1:300]
ETD.ts = ts(ETD.ts, frequency = 12, start = c(1996,1)) # 2020년까지 
test = ts(test, frequency = 12, start = c(2021,1)) # 2021년
test

# <시각화1>
"
- 분산이 증가하는 경향을 보임으로 분산 안정화가 필요 해보임 
- 계절성을 띄고 있기는 하나 차분, 계절차분으로 제거되는 경우도 있음으로 
계절성분 제거는 안하고 진행 
"
plot(ETD.ts) 

x = lowess(ETD.ts, f=0.1)
lines(x, col='red')

ma.cen <- ma(ETD.ts, order=12, centre=TRUE)
lines(ma.cen, col='blue')

# <시각화2> 
"
- 여름과 겨울의 경우 전력 사용량이 많은 편 
- 주기가 12로 볼 수 있다. 
"
ts.plot(tail(ETD.ts, 36)) 

# <분산의 안정화: Box-Cox>
"
- 분산이 안정화되는 것으로 보임 
- log 변환으로도 충분해 보임으로 log 변환 사용
"
boxcox(ETD.ts~time(ETD.ts)) # 점선: 신뢰구간

bc = boxcox(ETD.ts~time(ETD.ts), plotit = F) 
lam = bc$x[which.max(bc$y)]; lam

par(mfrow=c(1,3))
ETD.ts1 = (ETD.ts^lam - 1)/lam # lambda가 0이 아닐때
ETD.ts2 = log(ETD.ts) # lambda가 0일때
plot(ETD.ts)
plot(ETD.ts1)
plot(ETD.ts2)

# <차분 차수 결정 및 차분 시행>
"
- 계절 성분이 존재하는 것으로 보임 -> 계절 차분 시행 
- 정상성을 따르지 않음으로 차분 시행
"
x = diff(ETD.ts2, differences=1, lag=12)

par(mfrow=c(1,3))
plot(x)
acf(x) 
pacf(x) 
kpss.test(x)

"
- 차분 시행 
- 정상화된 것으로 보임 
- 추세성분도 제거된 것으로 보임 
"
x = diff(x, differences=1)
plot(x)
acf(x) 
pacf(x) 
kpss.test(x)

# <acf, pacf를 보고 모형의 차수 선택>
"
- non seasonal: (3~4,1,1~2)
- seasonal: (0,1,1)
"
acf(x) 
pacf(x, lag.max = 36)

out1 = arima(ETD.ts2, order=c(4,1,2), seasonal= list(order= c(0,1,1)))
out2 = arima(ETD.ts2, order=c(3,1,2), seasonal= list(order= c(0,1,1)))
out3 = arima(ETD.ts2, order=c(4,1,1), seasonal= list(order= c(0,1,1)))
out4 = arima(ETD.ts2, order=c(3,1,1), seasonal= list(order= c(0,1,1)))

AIC(out1, out2, out3, out4)

# <과다적합 분석>
out3
out3 = arima(ETD.ts2, order=c(4,1,0), seasonal= list(order= c(0,1,1))); out3

# <auto arima>
auto.arima(ETD.ts2)

# <잔차분석 함수> 
tsdiag(out3) 

# <함수로 예측>
pred = forecast(out3, h=12)
pred_exp = exp(as.data.frame(pred)); pred_exp

pred_2021 = as.data.frame(pred)[,1]
pred_2021 = exp(pred_2021) # 로그변환 한거 역변환

# rmse
pred_train = ETD.ts2 + out3$residuals
pred_train = exp(pred_train)
train_rmse = RMSE(pred_train, ETD.ts)
train_rmse

test_rmse = RMSE(pred_2021, test)
test_rmse

plot(x = c(1:300), y = ETD.ts, type='l', col = 'red')
lines(x = c(1:300), y = pred_train) 
title(main="ETD train (1996~2021)")

plot(x = c(1:12), test, type='l', col = 'red')
lines(x = c(1:12) ,y = pred_2021) 
title(main="ETD test (2021)")





