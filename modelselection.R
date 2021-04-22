
setwd("D:/behavior")
dat = read.csv("all_times.csv")
attach(dat)
#####
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
googletimes = googletimes*20

### stepwise selection by aic ###
library(leaps)
library(jtools)
fit=lm(googletimes~1)
step(fit,scope=list(lower=~1,upper=~addToCarttimes+categoryPageViewtimes+activityPageViewtimes
                    +checkouttimes+mainPageViewtimes+productPageViewtimes+purchasetimes
                    +searchtimes+trafficSourcetimes+userRegisterationtimes)
      ,direction="both")
# Call:
#   lm(formula = googletimes ~ productPageViewtimes + addToCarttimes + 
#        userRegisterationtimes + trafficSourcetimes + categoryPageViewtimes + 
#        activityPageViewtimes)
summary(lm(formula = googletimes ~ productPageViewtimes + addToCarttimes + 
                     userRegisterationtimes + trafficSourcetimes + categoryPageViewtimes + 
                     activityPageViewtimes))
#####BIC########
#####forward####
fit=lm(googletimes~1)
all=formula(lm(googletimes~addToCarttimes+categoryPageViewtimes+activityPageViewtimes
               +checkouttimes+mainPageViewtimes+productPageViewtimes+purchasetimes
               +searchtimes+trafficSourcetimes+userRegisterationtimes))

step(fit,direction="forward",scope=all, k=log(675))
# 
# Call:
#   lm(formula = googletimes ~ productPageViewtimes + addToCarttimes + 
#        userRegisterationtimes + trafficSourcetimes)

summ(lm(googletimes ~ productPageViewtimes + addToCarttimes + 
                     userRegisterationtimes + trafficSourcetimes))

fit = lm(googletimes~poly(productPageViewtimes,2,raw=T)+poly(addToCarttimes,2,raw=T)
         +poly(userRegisterationtimes,2,raw=T)+poly(trafficSourcetimes,2,raw=T))
summ(fit)

