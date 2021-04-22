# rm(list = ls())
library(tseries)
library(forecast)
library(GeneCycle)
library(data.table)

setwd("D:/Behavior/123_new")
purchaseCSV = list.files(path="D:/Behavior/purchase",pattern="\\.csv$", recursive = TRUE)
purchasetimes = c()
for(i in 1:675){
  purchasetimes[i] = nrow(read.csv(purchaseCSV[i]))
  print(round(i/675,3))
}

### 
setwd("D:/Behavior/")
multi1 = read.csv("multiTimeline1.csv", header = T, fileEncoding = "utf-8")
multi2 = read.csv("multiTimeline2.csv", header = T, fileEncoding = "utf-8")
multi3 = read.csv("multiTimeline3.csv", header = T, fileEncoding = "utf-8")
multi4 = read.csv("multiTimeline4.csv", header = T, fileEncoding = "utf-8")

multi1 = as.numeric(as.character(multi1[-1,]))
multi2 = as.numeric(as.character(multi2[-1,]))
multi3 = as.numeric(as.character(multi3[-1,]))
multi4 = as.numeric(as.character(multi4[-1,]))

googletimes = c(multi1, multi2, multi3, multi4)
#googletimes = googletimes*20

ts_googletimes = ts(googletimes, frequency = 1)
ts_purchase = ts(purchasetimes, frequency = 1)

########
acf(purchasetimes)
pacf(purchasetimes)
adf.test(ts_googletimes) # p-value<0.05 ：is stationary
acf(googletimes)
pacf(googletimes)
mod = auto.arima(ts_purchase)#, seasonal = F, test = "adf", ic = "aic")
summary(mod)
plot(forecast(mod))

mod2 = auto.arima(ts_googletimes)
summary(mod2)
plot(forecast(mod2))
forecast(mod2)
googletimes[666:675]

per = periodogram(as.numeric(googletimes))

per = periodogram(as.numeric(purchasetimes))
data.table(period = 1/per$freq, spec = per$spec)[order(-spec)][1:5]
#       period   spec.V1
# 1: 337.50000 337641.75
# 2: 168.75000 155685.30


bestfit = list(aic=mod$aic, p=0,q=0,fit = mod)
for(i in 1:3){
  for(j in 1:3){
    z1 = fourier(ts(purchasetimes, frequency = 338),K=i)
    z2 = fourier(ts(purchasetimes, frequency = 169),K=j)
    fit = auto.arima(purchasetimes, xreg = cbind(z1,z2), seasonal = F)
    if(fit$aic < bestfit$aic){
      bestfit = list(aic = fit$aic, p=i, q = j, fit = fit)
    }
  }
}

bestfit
model =  auto.arima(ts_purchase, xreg=fourier(ts_purchase,K=1), seasonal=FALSE)
forecast_r = forecast()



