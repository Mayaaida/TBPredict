library(psych)
library(forecast)
library(tseries)
library(ggplot2)
library(readxl)
library(lmtest)

#Data Training
> Dataset_TB_2020_2024 <- read_excel("~/Maya/Skripsi/Data/Dataset TB 2020-2024.xlsx")
> View(Dataset_TB_2020_2024)
> summary(Dataset_TB_2020_2024$`X`)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
81.0   141.0   223.5   224.1   292.2   403.0 
> TB_2020_2024_ts <- ts(
            Dataset_TB_2020_2024$`X`,
            start = c(2020, 1),
            frequency = 12)
> plot(TB_2020_2024_ts,main = "Data Asli")
> abline(v = seq(2020, 2025, by = 1), col = "blue", lty = 2)
> abline(v = seq(2020, 2025, by = 1/12), col = "gray", lty = 3)
> seasonplot(TB_2020_2024_ts,
     type = "o",
     pch = 16,
     main = "Data TB 2020-2024",
     xlab = "Bulan",
     ylab = "Jumlah Kasus",
     year.labels = TRUE,
     col=rainbow(6))
ggsubseriesplot(TB_2020_2024_ts)
> dekomposisi <- decompose(TB_2020_2024_ts, type = "multiplicative")
> par(mfrow = c(1, 2))
> acf(as.vector(TB_2020_2024_ts),
      lag.max = 36,
      main    = "ACF - Data Asli")
> pacf(as.vector(TB_2020_2024_ts),
       lag.max = 36,
       main    = "PACF - Data Asli")
> par(mfrow = c(1, 1))

#Import Data Testing 2025
Data_Testing_2025 <- read_excel("~/Maya/Skripsi/Data/Data Testing 2025.xlsx")
test_ts <- ts(Data_Testing_2025$X, start = c(2025, 1), frequency = 12)

#Uji Stasionertas Data Train
> adf.test(TB_2020_2024_ts) 
Augmented Dickey-Fuller Test

data:  TB_2020_2024_ts
Dickey-Fuller = -3.4661, Lag order = 3, p-value = 0.05393
alternative hypothesis: stationary

> kpss.test(TB_2020_2024_ts, null="Level")
KPSS Test for Level Stationarity

data:  TB_2020_2024_ts
KPSS Level = 1.4594, Truncation lag parameter = 3, p-value = 0.01

Warning message:
  In kpss.test(TB_2020_2024_ts, null = "Level") :
  p-value smaller than printed p-value

#Differencing Data Train
> TB_2020_2024_diff <- diff(TB_2020_2024_ts, differences = 1)
> plot(TB_2020_2024_diff, main="Differencing 1")
> adf.test(TB_2020_2024_diff)
Augmented Dickey-Fuller Test

data:  TB_2020_2024_diff
Dickey-Fuller = -6.0075, Lag order = 3, p-value = 0.01
alternative hypothesis: stationary

Warning message:
  In adf.test(TB_2020_2024_diff) : p-value smaller than printed p-value
> kpss.test(TB_2020_2024_diff)
KPSS Test for Level Stationarity

data:  TB_2020_2024_diff
KPSS Level = 0.066659, Truncation lag parameter = 3, p-value = 0.1
Warning message:
  In kpss.test(TB_2020_2024_diff) : p-value greater than printed p-value

#ACF PACF Non Musiman
> par(mfrow = c(1, 2))
> acf(as.vector(TB_2020_2024_diff),
             lag.max = 36,
             main    = "ACF - Differencing 1")
> pacf(as.vector(TB_2020_2024_diff),
               lag.max = 36,
               main    = "PACF - Differencing 1")
> par(mfrow = c(1, 1))

#Deteksi Perlu/Tidaknya Diff Musiman
nsdiffs(TB_2020_2024_ts)

#Estimasi Model
model1 <- arima(TB_2020_2024_ts, order=c(1,1,1), seasonal=list(order=c(1,0,1), period=12))
Error in optim(init[mask], armafn, method = optim.method, hessian = TRUE,  : 
                 non-finite finite-difference value [1] #terlalu overparameterized untuk data train
               
model2 <- arima(TB_2020_2024_ts, order=c(2,1,1), seasonal=list(order=c(1,0,1), period=12))
model3 <- arima(TB_2020_2024_ts, order=c(0,1,1), seasonal=list(order=c(1,0,1), period=12))
model4 <- arima(TB_2020_2024_ts, order=c(1,1,1), seasonal=list(order=c(0,0,1), period=12))
model5 <- arima(TB_2020_2024_ts, order=c(2,1,1), seasonal=list(order=c(0,0,1), period=12))
model6 <- arima(TB_2020_2024_ts, order=c(0,1,1), seasonal=list(order=c(0,0,1), period=12))
model7 <- arima(TB_2020_2024_ts, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=12))

> AIC(model2, model3, model4, model5, model6, model7)
df      AIC
model2  6 622.4479
model3  4 618.4481
model4  4 618.9420
model5  5 620.9327
model6  3 617.0063
model7  3 616.5127
> BIC(model2, model3, model4, model5, model6, model7)
df      BIC
model2  6 634.9131
model3  4 626.7583
model4  4 627.2522
model5  5 631.3204
model6  3 623.2389
model7  3 622.7453
  
#Coeftest
  coeftest(model2)
  coeftest(model3)
  coeftest(model4)
  coeftest(model5)
  coeftest(model6)
  coeftest(model7)
  
#summary
  summary(model3) 
  summary(model4)
  summary(model5)
  summary(model6)
  summary(model7)
  
#Uji Diagnostik
> tsdiag(model7)
  checkresiduals(model3)
  checkresiduals(model4)
  checkresiduals(model5) 
  checkresiduals(model6)
  checkresiduals(model7)
  
#Auto Arima
auto.arima(TB_2020_2024_ts)
  Series: TB_2020_2024_ts 
  ARIMA(0,1,1)(1,0,0)[12] with drift 
  
  Coefficients:
    ma1    sar1   drift
  -0.7624  0.2202  3.8028
  s.e.   0.1037  0.1331  1.7651
  
  sigma^2 = 2021:  log likelihood = -307.44
  AIC=622.88   AICc=623.62   BIC=631.19
  
#Forecast Tahun 2025
> fc_test <- forecast(model7, h=12)
  fc_test
  Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
  Jan 2025       323.7533 269.7699 377.7368 241.1927 406.3140
  Feb 2025       318.1438 261.3032 374.9843 231.2136 405.0739
  Mar 2025       336.7440 277.1832 396.3047 245.6537 427.8342
  Apr 2025       332.9058 270.7438 395.0678 237.8373 427.9743
  May 2025       351.5060 286.8474 416.1647 252.6191 450.3929
  Jun 2025       318.7342 251.6718 385.7967 216.1711 421.2974
  Jul 2025       340.2868 270.9038 409.6699 234.1747 446.3990
  Aug 2025       345.3059 273.6775 416.9344 235.7597 454.8522
  Sep 2025       336.4487 262.6431 410.2543 223.5728 449.3246
  Oct 2025       347.3726 271.4523 423.2930 231.2625 463.4828
  Nov 2025       326.1153 248.1375 404.0930 206.8586 445.3719
  Dec 2025       333.7915 253.8093 413.7738 211.4692 456.1138
  
#Bandingkan forecast dengan aktual
  akurasi <- accuracy(fc_test, test_ts)
  print(akurasi)
  
#Tabel perbandingan
  hasil_tabel <- data.frame(
    Bulan = format(seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month"), "%B"),
    Actual = as.numeric(test_ts),
    Prediction = round(as.numeric(fc_test$mean), 0)
  )
  hasil_tabel$Error <- round(abs(hasil_tabel$Actual - hasil_tabel$Prediction), 2)
  hasil_tabel$MAPE <- round((hasil_tabel$Error / hasil_tabel$Actual) * 100, 2)
  hasil_tabel$MAPE_label <- paste0(hasil_tabel$MAPE, "%")
  mae  <- mean(hasil_tabel$Error)
  rmse <- sqrt(mean((hasil_tabel$Actual - hasil_tabel$Prediction)^2))
  baris_rmse <- data.frame(
    Bulan      = "RMSE",
    Actual     = NA,
    Prediction = NA,
    Error      = rmse,
    MAPE       = NA,
    MAPE_label = ""
  )
  hasil_tabel_final <- rbind(hasil_tabel, baris_rmse)
  print(hasil_tabel_final)
  
#Gabung Data Train dan Data Test
full_ts <- ts(c(as.numeric(TB_2020_2024_ts), as.numeric(test_ts)), start = c(2020, 1), frequency = 12)
length(full_ts)

#Refit Model7
model_final <- Arima(full_ts, order = c(0,1,1), 
                     seasonal = list(order = c(1,0,0), period = 12))
summary(model_final)

#Forecast 2026-2028
forecast <- forecast(model_final, h = 36)
forecast

plot(forecast, main = "Prediksi Kasus TB Paru Kabupaten Pemalang 2026-2028", xlab = "Tahun",
     ylab = "Jumlah Kasus",xaxt = "n")
axis(
  1,
  at = seq(2020, 2028, by = 1),
  labels = c(
    "2020","2021","2022","2023",
    "2024","2025","2026","2027","2028"
  )
)
points(time(full_ts),full_ts, pch = 1,col = "black",cex = 0.2)
points(time(forecast$mean),
       forecast$mean,type = "o",pch = 16,col = "blue",lwd = 2)