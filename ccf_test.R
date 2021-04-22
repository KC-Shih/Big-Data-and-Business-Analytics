# rm(list = ls())
library(tseries)
library(forecast)
library(astsa)
setwd("D:/1082/bda/123_new")
all_times_wk = read.csv("all_times_wk.csv")
# head(all_times_wk)
attach(all_times_wk)
googletrend = googletrend*8
plot(googletrend, type = "l")
lines(purchasetimes_wk, col = "red") 

## 
# ccf(x-variable, y-variable)
# cross correlation means that cross correlation between X(t+h) vs Y(t)
# google trend is x for all sample
par(mfcol=c(3,3))
ccf(googletrend, addToCarttimes_wk,lag.max = 7)
ccf(googletrend, purchasetimes_wk, lag.max = 7)
ccf(googletrend, searchtimes_wk ,lag.max = 7)
ccf(googletrend, mainPageViewtimes_wk ,lag.max = 7)
ccf(googletrend, categoryPageViewtimes_wk ,lag.max = 7)
ccf(googletrend, activityPageViewtimes_wk ,lag.max = 7)
ccf(googletrend, checkouttimes_wk ,lag.max = 7)
ccf(googletrend, userRegisterationtimes_wk ,lag.max = 7)
ccf(googletrend, trafficSourcetimes_wk ,lag.max = 7)
# 全部資料

ccf(googletimes11_19,purchasetimes11_19, lag.max = 7)
ccf(googletimes11_19,checkouttimes11_19, lag.max = 7)
ccf(googletimes11_19,addToCarttimes11_19, lag.max = 7)
ccf(googletimes11_19,searchtimes11_19, lag.max = 7)

############################
purchasetimes11_19 = purchasetimes[485:515]

googletimes11_18 = googletimes[124:154]

plot(purchasetimes11_19, type = "l")
lines(googletimes11_19, col = "red")

plot(purchasetimes11_18, type = "l")
lines(googletimes11_18, col = "red")

###############
### 2018
# 三個月 ：78:169
purchasetimes11_18_3month = purchasetimes[78:169]
googletimes11_18_3month = googletimes[78:169]

# 一個月
purchasetimes11_18_1month = purchasetimes[124:154]
googletimes11_18_1month = googletimes[124:154]

# 一周：136:142
purchasetimes11_18_1week = purchasetimes[135:142]
googletimes11_18_1week = googletimes[135:142]
# 前後10天
purchasetimes11_18 = purchasetimes[134:144]
googletimes11_18 = googletimes[134:144]

ccf(googletimes11_18, purchasetimes11_18, lag.max = 7, main = "2018雙十一前後10天")
# plot 2018
par(mfcol=c(1,3))
ccf(googletimes11_18_1week, purchasetimes11_18_1week, lag.max = 7)
ccf(googletimes11_18_1month, purchasetimes11_18_1month, lag.max = 7)
ccf(googletimes11_18_3month, purchasetimes11_18_3month, lag.max = 7)
#########################
# 2019
# 三個月 459:550
purchasetimes11_19_3month = purchasetimes[459:550]
googletimes11_19_3month = googletimes[459:550]

# 一個月 473:534
purchasetimes11_19_1month = purchasetimes[473:534]
googletimes11_19_1month = googletimes[473:534]

# 一周 501:507
purchasetimes11_19_1week = purchasetimes[501:507]
googletimes11_19_1week = googletimes[501:507]
ccf(googletimes11_19, purchasetimes11_19)

# 前後10天
purchasetimes11_19 = purchasetimes[497:511]
googletimes11_19 = googletimes[497:511]
ccf(googletimes11_19, purchasetimes11_19, lag.max = 7, main = "2019雙十一前後10天")

# plot 2019
par(mfcol=c(1,3))
ccf(googletimes11_19_1week, purchasetimes11_19_1week, lag.max = 7)
ccf(googletimes11_19_1month, purchasetimes11_19_1month, lag.max = 7)
ccf(googletimes11_19_3month, purchasetimes11_19_3month, lag.max = 7, main = "cross correlation between Google Trend and Purchase times",
    sub = "Three months before and after 2019/11/11", col.main="blue")

# title(main="cross correlation between Google Trend and Purchase times", col.main="red",
#       sub="Three months before and after 2019/11/11", col.sub="blue")
# # 2018/2019 一起畫
par(mfrow=c(2,3))
ccf(googletimes11_18_1week, purchasetimes11_18_1week, lag.max = 7)
ccf(googletimes11_18_1month, purchasetimes11_18_1month, lag.max = 7)
ccf(googletimes11_18_3month, purchasetimes11_18_3month, lag.max = 7)
ccf(googletimes11_19_1week, purchasetimes11_19_1week, lag.max = 7)
ccf(googletimes11_19_1month, purchasetimes11_19_1month, lag.max = 7)
ccf(googletimes11_19_3month, purchasetimes11_19_3month, lag.max = 7)
#########################
lag2.plot (googletimes11_19,  purchasetimes11_19, max.lag = 3)
lag2.plot (googletimes11_19_3month,  purchasetimes11_19_3month, max.lag = 3)

#########################
ts_google_wk = ts(googletrend, frequency = 1)
ts_purchasetimes_wk = ts(purchasetimes_wk, frequency = 1)
auto.arima(ts_google_wk)
auto.arima(ts_purchasetimes_wk)
# acf(ts_google_wk)
# pacf(ts_google_wk)
# acf(ts_purchasetimes_wk)
# pacf(ts_purchasetimes_wk)

####################################
par(mfcol=c(1,3))
purchasetimes3 = purchasetimes[585:675]
googletimes3 = googletimes[585:675]
ccf(googletimes3, purchasetimes3, lag.max = 7, main = "最後三個月")

purchasetimes1 = purchasetimes[645:675]
googletimes3 = googletimes[645:675]
ccf(googletimes3, purchasetimes3, lag.max = 7, main = "最後一個月")

purchasetimes3 = purchasetimes[665:675]
googletimes3 = googletimes[665:675]
ccf(googletimes3, purchasetimes3, lag.max = 7, main = "最後10天")

par(mfcol=c(1,2))
googletimes3_19 = googletimes[190:280]
purchasetimes3_19 = purchasetimes[190:280]
ccf(googletimes3_19, purchasetimes3_19, lag.max = 7, main = "2019一到三月")

googletimes3_19 = googletimes[250:280]
purchasetimes3_19 = purchasetimes[250:280]
ccf(googletimes3_19, purchasetimes3_19, lag.max = 7, main = "2019三月")
  
  ############
ccf(googletrend, purchasetimes_wk, lag.max = 5, 
      main = "cross correlation between Google Trends and Purchase times(all)")
ccf(googletimes11_19_3month, purchasetimes11_19_3month, 
      lag.max = 5, main = "cross correlation between Google Trends and Purchase times(2019.11.11)")

  
  
  