setwd("D:/Behavior/123_new")

# 讀入各資料夾檔案名稱 (需要將資料手動分開放到各自資料夾)
addToCartCSV = list.files(path="D:/Behavior/addToCart",pattern="\\.csv$", recursive = TRUE)
categoryPageViewCSV = list.files(path="D:/Behavior/categoryPageView",pattern="\\.csv$", recursive = TRUE)
activityPageViewCSV = list.files(path="D:/Behavior/activityPageView",pattern="\\.csv$", recursive = TRUE)
checkoutCSV = list.files(path="D:/Behavior/checkout",pattern="\\.csv$", recursive = TRUE)
mainPageViewCSV = list.files(path="D:/Behavior/mainPageView",pattern="\\.csv$", recursive = TRUE)
productPageViewCSV = list.files(path="D:/Behavior/productPageView",pattern="\\.csv$", recursive = TRUE)
purchaseCSV = list.files(path="D:/Behavior/purchase",pattern="\\.csv$", recursive = TRUE)
searchCSV = list.files(path="D:/Behavior/search",pattern="\\.csv$", recursive = TRUE)
trafficSourceCSV = list.files(path="D:/Behavior/trafficSource",pattern="\\.csv$", recursive = TRUE)
userRegisterationCSV = list.files(path="D:/Behavior/userRegisteration",pattern="\\.csv$", recursive = TRUE)

##########################
# 次數變數
addToCarttimes = c()
categoryPageViewtimes = c()
activityPageViewtimes = c()
checkouttimes = c()
mainPageViewtimes = c()
productPageViewtimes = c()
purchasetimes = c()
searchtimes = c()
trafficSourcetimes = c()
userRegisterationtimes = c()
###########################
# 計算資料夾中各個檔案的列數量(即有幾筆數據))
for(i in 1:675){
  addToCarttimes[i] = nrow(read.csv(addToCartCSV[i]))
  categoryPageViewtimes[i] = nrow(read.csv(categoryPageViewCSV[i]))
  activityPageViewtimes[i] = nrow(read.csv(activityPageViewCSV[i]))
  checkouttimes[i] = nrow(read.csv(checkoutCSV[i]))
  mainPageViewtimes[i] = nrow(read.csv(mainPageViewCSV[i]))
  productPageViewtimes[i] = nrow(read.csv(productPageViewCSV[i]))
  purchasetimes[i] = nrow(read.csv(purchaseCSV[i]))
  searchtimes[i] = nrow(read.csv(searchCSV[i],fileEncoding = "UTF-8"))
  trafficSourcetimes[i] = nrow(read.csv(trafficSourceCSV[i]))
  userRegisterationtimes[i] = nrow(read.csv(userRegisterationCSV[i]))
  print(round(i/675,3))
}
###########################
# 繪製時序圖
# all category
day = seq_along(addToCarttimes)
#                                                             #max
plot(day, addToCarttimes, type = "l", ylim = c(0,60100), ylab = "times", xlab = "date from 2018-6-26" , main = "行為資料次數比較(all)", lwd = 1.5)# 7991
lines(day, categoryPageViewtimes, col = "blue", lwd = 1.5 )   # 56843
lines(day, activityPageViewtimes, col = "red", lwd = 1.5 )    # 1638
lines(day, checkouttimes, col = "orange", lwd = 1.5 )         # 4158
lines(day, mainPageViewtimes, col = "yellow", lwd = 1.5 )     # 32070
lines(day, productPageViewtimes, col = "green" , lwd = 1.5)   # 60099
lines(day, purchasetimes, col = "purple", lwd = 1.5 )         # 1791
lines(day, searchtimes, col = "pink" , lwd = 1.5)             # 1421
# lines(day, trafficSourcetimes, col = "FFEBCD" )             # 460227
lines(day, userRegisterationtimes, col = "brown4", lwd = 1.5 ) # 273
legend(x = 250, y = 60000,
       c("搜尋", "加入購物車", "開始結帳", "購買", "瀏覽分類頁","瀏覽活動頁","瀏覽商品頁","瀏覽首頁","會員註冊"), 
       pch=c(2,3,5,8,1,4,7,6,9),
       col=c("pink","black","orange", "purple","blue","red","green","yellow","brown4"))


# 只有 traffic Source(因為他的筆數較多，自己畫)
plot(day, trafficSourcetimes, col = "red", type = "l", main = "traffic Source", ylab = "times", xlab = "date from 2018-6-26")
# 搜尋 加入購物車 開始結帳 購買

# plot(day, addToCarttimes, type = "l", ylab = "times",xlab = "date from 2018-6-26" , main = "行為資料次數比較", lwd = 1.5)
plot(day, checkouttimes, type = "l", ylab = "times",xlab = "date from 2018-6-26" , main = "行為資料次數比較", lwd = 1.5, col = "orange")
# lines(day, searchtimes, col = "pink" ) 
# lines(day, checkouttimes, col = "orange" , lwd = 1.5)
lines(day, purchasetimes, col = "purple" , lwd = 1.5)
lines(day, googletimes, col = "green", lwd = 1.5)
# legend(x = 0, y = 8000,
#        c("搜尋", "加入購物車", "開始結帳", "購買"), 
#        pch=c(2,3,5,8),
#        col=c("green","black","orange", "purple"))
legend(x = 0, y = 4000,
       c("Google Trends", "開始結帳", "購買"), 
       pch=c(2,3,8),
       col=c("green","orange", "purple"))

##########################
# 2019雙十一前後一周
# 497-511(11/4-11/18)

double11_2019 = matrix(nrow = 4,ncol = 15)

searchtimes11_19 = searchtimes[497:511]
double11_2019[1,] = searchtimes11_19

addToCarttimes11_19 = addToCarttimes[497:511]
double11_2019[2,] = addToCarttimes11_19

checkouttimes11_19 = checkouttimes[497:511]
double11_2019[3,] = checkouttimes11_19

purchasetimes11_19 =  purchasetimes[497:511]
double11_2019[4,] = purchasetimes11_19

googletimes11_19 = googletimes[497:511]


colnames(double11_2019) = c("11/4","11/5","11/6","11/7","11/8","11/9","11/10",
                            "11/11","11/12","11/13","11/14","11/15","11/16","11/17","11/18")
rownames(double11_2019) = c("search", "addToCart", "checkout", "purchase")
day = seq_along(purchasetimes11_19)
plot(x = day, y = as.numeric(double11_2019[2,]), ylab = "times",xlab = "date", 
     type = "l", ylim = c(0,8000), col = "yellow", 
     main = "2019雙十一前後一週行為指標次數", xaxt = "n")
lines(day, as.numeric(double11_2019[3,]), type = "l", col = "blue")
lines(day, as.numeric(double11_2019[4,]), type = "l", col = "red")
legend(x = 1.5, y = 8000,
       c("加入購物車", "開始結帳", "購買"), 
       pch=c(2,3,5),
       col=c("yellow", "blue", "red"))
axis(side = 1,at = 1:15,labels = c("11/4","11/5","11/6","11/7","11/8","11/9","11/10",
                         "11/11","11/12","11/13","11/14","11/15","11/16","11/17","11/18"))
#############################
# 只畫Google Trends, 開始結帳, 購買

plot(x = day, y = googletimes11_19, ylab = "times",xlab = "date", 
     type = "l", ylim = c(0,8000), col = "green", 
     main = "2019雙十一前後一週行為指標次數", xaxt = "n")
lines(day, purchasetimes11_19, type = "l", col = "purple")
lines(day, checkouttimes11_19, type = "l", col = "orange")
legend(x = 1.5, y = 8000,
       c("Google Trends", "開始結帳", "購買"), 
       pch=c(2,3,5),
       col=c("green", "orange", "purple"))
axis(side = 1,at = 1:15,labels = c("11/4","11/5","11/6","11/7","11/8","11/9","11/10",
                                   "11/11","11/12","11/13","11/14","11/15","11/16","11/17","11/18"))


# 畫多變數分布圖
pairs(~double11_2019[2,]+double11_2019[3,]+double11_2019[4,], 
      labels = c("加入購物車", "開始結帳", "購買"))

corp = cbind(searchtimes11_19, addToCarttimes11_19,checkouttimes11_19,purchasetimes11_19)

# 共變異矩陣及共變異圖
round(cor(corp),3)
# library(corrplot)
# corrplot(cor(corp), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

##########################
# 加入"搜尋"畫圖
plot(x = day, y = as.numeric(double11_2019[2,]), ylab = "times",xlab = "date", 
     type = "l", ylim = c(0,8000), col = "yellow", 
     main = "2019雙十一前後一週行為指標次數", xaxt = "n")
lines(day, as.numeric(double11_2019[3,]), type = "l", col = "blue")
lines(day, as.numeric(double11_2019[4,]), type = "l", col = "red")
lines(day, as.numeric(double11_2019[1,]), type = "l", col = "purple")
legend(x = 1.5, y = 8000,
       c("加入購物車", "開始結帳", "購買", "搜尋"), 
       pch=c(2,3,5),
       col=c("yellow", "blue", "red","purple"))
axis(side = 1,at = 1:15,labels = c("11/4","11/5","11/6","11/7","11/8","11/9","11/10",
                                   "11/11","11/12","11/13","11/14","11/15","11/16","11/17","11/18"))


##########################
# 補：google trends 資料來源及次數
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

###########################
# 把所有次數何在一起輸出，資料已經上傳，大家可以直接用

all_times = cbind(searchtimes, googletimes, mainPageViewtimes, categoryPageViewtimes, activityPageViewtimes,
                  productPageViewtimes, addToCarttimes, checkouttimes, purchasetimes, 
                  userRegisterationtimes, trafficSourcetimes)
row.names(all_times) = 1:675
all_times = c(1:5)
write.csv(all_times, "all_times.csv")


