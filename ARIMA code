setwd("C:/Users/lhj/Desktop/Tcd/数据挖掘/group_work")
df<-read.csv('new.csv',sep=',')
df <- df[c('price',"tradeTime")]
sum(is.na(df$tradeTime))
sum(is.na(df$price))

# data prepartion
df$tradeTimeTs <- as.Date(df$tradeTime, format = "%Y/%m/%d")
df$year <- year(df$tradeTimeTs)
df$month <- month(df$tradeTimeTs)
df$tradeTimeM <- as.Date(paste0(df$year,'-',df$month,'-01'))
df$tradeTimeY <- as.Date(paste0(df$year,'-01-01'))

# Time series preprocessing
df_train <- data.frame(df %>% filter(year>2009 & year<2017))
df_test <- data.frame(df %>% filter(year>=2017))
df %>% filter(year>2009) %>% group_by(tradeTimeM) %>% summarise(count=n(), mean = mean(price)) %>% 
  ggplot(aes(x=tradeTimeM, y= mean)) + 
  geom_line() + geom_point(aes(size=count)) +
  labs(title='Average price of monthly traded', 
       subtitle='number of homes traded by month is the size of the area') 
df_timer <- df_train %>%
  group_by(tradeTimeM) %>%
  summarise(mean = mean(price))
df_timer2 <- df_test %>%
  group_by(tradeTimeM) %>%
  summarise(mean = mean(price))
df_timer3 <- df %>%
  group_by(tradeTimeM) %>%
  summarise(mean = mean(price))
  
# Time series analysis  
df_ts <- ts(df_timer$mean,start = c(2010, 1),freq=12)
acf(df_ts,48)
df_stl = stl(df_ts, s.window="periodic")
df_seasadj <- seasadj(df_stl)
adf.test(df_seasadj)
df_stl = stl(df_ts, s.window="periodic")
df_seasonal <- seasadj(df_stl)
adf.test(df_seasonal)
ts1 <- diff(df_seasadj)
plot(ts1)
adf.test(ts1)
ts2 <- diff(df_seasadj,1,2)
plot(ts2)
adf.test(ts2)
par(mfrow = c(1,2))
acf(ts2,lag.max=20)
pacf(ts2,lag.max=20)

# arima fitting
am <- auto.arima(df_seasadj,d=2,stepwise=FALSE, approximation=FALSE)
am
ggtsdisplay(residuals(am))

# white noise test
Box.test(am$residual,type="Box-Pierce",lag=5)

# Draw a forecast map
df_train.ts <- ts(df_timer$mean,start = c(2010,1),end = c(2016,12),freq = 12)
df_test.ts <- ts(df_timer2$mean,start = c(2017,1),frequency = 12)
window1 <- window(df_train.ts,start = c(2010,1),end = c(2016,12))
window2 <- window(df_test.ts,start = c(2017,1),end = c(2017,12))

pred <- forecast(am,h=12,level=c(99.5))
pred
plot(pred)
pred$mean
train_df <- data.frame(time = as.Date(c('2017-01-01','2017-02-01','2017-03-01','2017-04-01','2017-05-01','2017-06-01',
                  '2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01','2017-12-01')),train = pred$mean)
new_df <- cbind(train_df,price = window2)
Sys.setlocale("LC_TIME", "English")
plot1 <- ggplot(data = new_df, aes(x = time)) + geom_line(aes(y = price, linetype = "actual", colour = "actual"), size = 0.8) 
plot2 <- plot1 + geom_line(aes(y = pred$mean, linetype = "prediction", colour = "prediction"), size = 0.8)

plot2 + scale_linetype_manual(name = "", values = c("actual" = "solid", "prediction" = "twodash")) +
  scale_colour_manual(name = "", values = c("actual" = "red", "prediction" = "blue")) + 
  labs(title = 'Monthly Average homes prices in ARIMA') + theme(axis.title =element_blank()) + theme_minimal(12) + scale_color_brewer(name='',palette='Set1') 

# observe the error
accuracy(forecast(am),window2)


