setwd("C:/Users/lhj/Desktop/Tcd/数据挖掘/group_work")
df<-read.csv('new.csv',sep=',')
df <- df[c('price',"tradeTime")]

# data preparation
df$tradeTimeTs <- as.Date(df$tradeTime, format = "%Y/%m/%d")
df$year <- year(df$tradeTimeTs)
df$month <- month(df$tradeTimeTs)
df$tradeTimeM <- as.Date(paste0(df$year,'-',df$month,'-01'))
df$tradeTimeY <- as.Date(paste0(df$year,'-01-01'))
df_train <- data.frame(df %>% filter(year>2009 & year<2017))
df_test <- data.frame(df %>% filter(year>=2017))
df_timer <- df_train %>%
  group_by(tradeTimeM) %>%
  summarise(mean = mean(price))
df_timer2 <- df_test %>%
  group_by(tradeTimeM) %>%
  summarise(mean = mean(price))
  
# time series analysis
df_train.ts <- ts(df_timer$mean,start = c(2010,1),end = c(2016,12),freq = 12)
df_test.ts <- ts(df_timer2$mean,start = c(2017,1),frequency = 12)
window1 <- window(df_train.ts,start = c(2010,1),end = c(2016,12))
window2 <- window(df_test.ts,start = c(2017,1),end = c(2017,12))
myds_month <- decompose(df_train.ts)
plot(myds_month)

# forecast plot
my_df_ts <- data.frame(price = df_train.ts, as.numeric(time(df_train.ts)))
names(my_df_ts) <- c("price", "time")
mymodel <- tslm(price~season+trend,my_df_ts)
my_fc <- forecast(mymodel,h=12)
autoplot(my_fc) + geom_vline(xintercept= as.Date('2017-01-01'))
test_df <- data.frame(time = as.Date(c('2017-01-01','2017-02-01','2017-03-01','2017-04-01','2017-05-01','2017-06-01',
                  '2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01','2017-12-01')),train = my_fc$mean)
new_df2 <- cbind(test_df,price = window2)
Sys.setlocale("LC_TIME", "English")
plot1 <- ggplot(data = new_df2, aes(x = time)) + geom_line(aes(y = price, linetype = "actual", colour = "actual"), size = 0.8) 
plot2 <- plot1 + geom_line(aes(y = my_fc$mean, linetype = "prediction", colour = "prediction"), size = 0.8)

plot2 + scale_linetype_manual(name = "", values = c("actual" = "solid", "prediction" = "twodash")) +
  scale_colour_manual(name = "", values = c("actual" = "red", "prediction" = "blue")) + 
  labs(title = 'Monthly Average homes prices in Linear Regression') + theme(axis.title =element_blank()) + theme_minimal(12) + scale_color_brewer(name='',palette='Set1') 

# observe error
accuracy(forecast(mymodel),window2)
