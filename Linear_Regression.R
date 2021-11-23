setwd("C:/Users/lhj/Desktop/Tcd/数据挖掘/group_work")
df<-read.csv('new.csv',sep=',')
house <- df

# explore missing values
colpla =c(rgb(5,28,44, max =255) ,
rgb(31,64,230, max =255),
rgb(175,195,255, max =255)  ,
rgb(229,84,108, max =255)
)
sum(is.na(df$DOM))
sum(is.na(df$buildingType))
sum(is.na(df$communityAverage))
sum(is.na(df$Lng))
sum(is.na(df$Lat))
sum(is.na(df$tradeTime))
sum(is.na(df$followers))
sum(is.na(df$totalPrice))
sum(is.na(df$price))
sum(is.na(df$square))
sum(is.na(df$livingRoom))
sum(is.na(df$drawingRoom))
sum(is.na(df$kitchen))
sum(is.na(df$bathRoom))
sum(is.na(df$floor))
sum(is.na(df$constructionTime))
sum(is.na(df$renovationCondition))
sum(is.na(df$buildingStructure))
sum(is.na(df$ladderRatio))
sum(is.na(df$elevator))
sum(is.na(df$fiveYearsProperty))
sum(is.na(df$subway))
sum(is.na(df$district))
matrixplot(df)
typeof(df$communityAverage)
typeof(df$buildingType)

# data cleaning
df <- df[,-c(1,2,5,7)]
df$floor <- as.numeric(sapply(df$floor, function(x) strsplit(x,' ')[[1]][2]))
df$livingRoom <- as.numeric(df$livingRoom)
df$bathRoom <- as.numeric(df$bathRoom)
df$district <-as.factor(df$district)
df$drawingRoom <- as.numeric(df$drawingRoom)
df$district <- as.factor(df$district)
df$constructionTime <-as.numeric(df$constructionTime)
df$buildingType <- as.factor(df$buildingType)
df$buildingStructure <- as.factor(df$buildingStructure)
df$elevator <- as.factor(df$elevator)
df$fiveYearsProperty <- as.factor(df$fiveYearsProperty)
df$subway <- as.factor(df$subway)
df$renovationCondition <- as.factor(df$renovationCondition)
summary(df)
kable(df %>% group_by(buildingType) %>% summarise(count=n()))
df$buildingType<-as.factor(sub("1","Tower",df$buildingType,ignore.case = FALSE,fixed = TRUE))
df$buildingType<-as.factor(sub("2","Bungalow",df$buildingType,ignore.case = FALSE,fixed = TRUE))
df$buildingType<-as.factor(sub("3","Mix_plate_tower",df$buildingType,ignore.case = FALSE,fixed = TRUE))
df$buildingType<-as.factor(sub("4","plate",df$buildingType,ignore.case = FALSE,fixed = TRUE))
df$buildingType[df$buildingType == 'NaN'] <- NA
df$buildingType[df$buildingType == 0.048] <- NA
df$buildingType[df$buildingType == 0.125] <- NA
df$buildingType[df$buildingType == 0.250] <- NA
df$buildingType[df$buildingType == 0.333] <- NA
df$buildingType[df$buildingType == 0.375] <- NA
df$buildingType[df$buildingType == 0.429] <- NA
df$buildingType[df$buildingType == 0.5] <- NA
df$buildingType[df$buildingType == '0.0plate8'] <- NA
df$buildingType[df$buildingType == '0.Bungalow5'] <- NA
df$buildingType[df$buildingType == '0.Mix_plate_tower33'] <- NA
df$buildingType[df$buildingType == '0.Mix_plate_tower75'] <- NA
df$buildingType[df$buildingType == '0.plateBungalow9'] <- NA
df$buildingType[df$buildingType == '0.TowerBungalow5'] <- NA
df$buildingType[df$buildingType == 0.667] <- NA

# delete elevator's NA also drop subway and fiveyearsproperty column's NA
df <- df[-which(is.na(df$elevator)),]
# delete buildingType and communityAverage missing valus,because they are very small
df <- df[-which(is.na(df$buildingType)),]
df <- df[-which(is.na(df$communityAverage)),]

df$constructionTime[df$constructionTime == '未知'] <- NA
kable(df %>% group_by(constructionTime) %>% summarise(count=n()) %>% arrange(-count))
18747/316648
imputed_Data <- mice(df, m=5, maxit = 50, method = 'pmm', seed = 500)
imputed_Data$imp$constructionTime
df <- complete(imputed_Data)
summary(df)
kable(df %>% group_by(renovationCondition) %>% summarise(count=n()))
df$renovationCondition<-as.factor(sub("1","Other",df$renovationCondition,ignore.case = FALSE,fixed = TRUE))
df$renovationCondition<-as.factor(sub("2","Rough",df$renovationCondition,ignore.case = FALSE,fixed = TRUE))
df$renovationCondition<-as.factor(sub("3","Simplicity",df$renovationCondition,ignore.case = FALSE,fixed = TRUE))
df$renovationCondition<-as.factor(sub("4","Hardcover",df$renovationCondition,ignore.case = FALSE,fixed = TRUE))
kable(df %>% group_by(buildingStructure ) %>% summarise(count=n()))
df$buildingStructure<-as.factor(sub("1","Unknown",df$buildingStructure,ignore.case = FALSE,fixed = TRUE))
df$buildingStructure<-as.factor(sub("2","Mix",df$buildingStructure,ignore.case = FALSE,fixed = TRUE))
df$buildingStructure<-as.factor(sub("3","Brick_Wood",df$buildingStructure,ignore.case = FALSE,fixed = TRUE))
df$buildingStructure<-as.factor(sub("4","Brick_Concrete",df$buildingStructure,ignore.case = FALSE,fixed = TRUE))
df$buildingStructure<-as.factor(sub("5","Steel",df$buildingStructure,ignore.case = FALSE,fixed = TRUE))
df$buildingStructure<-as.factor(sub("6","Steel_Concrete",df$buildingStructure,ignore.case = FALSE,fixed = TRUE))
# kable(df %>% group_by(elevator) %>% summarise(count=n()))
# df$elevator<-as.factor(sub("1","has_elevator",df$elevator,ignore.case = FALSE,fixed = TRUE))
# df$elevator<-as.factor(sub("0","not_have_elevator",df$elevator,ignore.case = FALSE,fixed = TRUE))
# kable(df %>% group_by(subway) %>% summarise(count=n()))
# df$subway<-as.factor(sub("1","has_subway",df$subway,ignore.case = FALSE,fixed = TRUE))
# df$subway<-as.factor(sub("0","not_have_subway",df$subway,ignore.case = FALSE,fixed = TRUE))
# kable(df %>% group_by(fiveYearsProperty) %>% summarise(count=n()))
# df$fiveYearsProperty<-as.factor(sub("1","has_less_than_5",df$fiveYearsProperty,ignore.case = FALSE,fixed = TRUE))
# df$fiveYearsProperty<-as.factor(sub("0","not_have_less_than_5",df$fiveYearsProperty,ignore.case = FALSE,fixed = TRUE))
sum(is.na(df))
str(df)
summary(df)
build_type <- class.ind(df$buildingType)
df <- cbind(df[-13],build_type)
build_s <- class.ind(df$buildingStructure)
df <- cbind(df[-15],build_s)
r_Condition <- class.ind(df$renovationCondition)
df <- cbind(df[-14],r_Condition)
# ele <- class.ind(df$elevator)
# df <- cbind(df[-15],ele)
# sub <- class.ind(df$subway)
# df <- cbind(df[-16],sub)
# fiveYears <- class.ind(df$fiveYearsProperty)
# df <- cbind(df[-15],fiveYears)
# district2 <- class.ind(df$district)
# df <- cbind(df[-18],district2)
df2 <- df
df3 <- df
df4 <- df

# Correlation analysis
time <- df3[,3]
df3 <- df3[,-c(3,15:18,20:27,30)]

my_data <- scale(df3, center = T, scale = T)
corrplot::corrplot(round(cor(my_data),2),
         method='circle',
         tl.cex=0.5)
corr_matrix <- round(cor(my_data),2)
# Steel_Concrete and  Mix 0.91 correlation,Steel_Concrete needs to be removed
res2<-rcorr(my_data)
res2$r
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
my_data2 <- flattenCorrMatrix(res2$r,res2$P)
abs(my_data2$cor) > 0.8
my_data2[abs(my_data2$cor)>0.8,]
# res.pca <- PCA(my_data, graph = T)

# divide train and test
df2$tradeTime <- as.Date(df2$tradeTime, format = "%Y/%m/%d")
df2$year <- year(df2$tradeTime)
df2$month <- month(df2$tradeTime)
df2$tradeTimeM <- as.Date(paste0(df2$year,'-',df2$month,'-01'))
df2$tradeTimeY <- as.Date(paste0(df2$year,'-01-01'))
table(df2$year)
df2 <- df2[,-c(1:2,17:19,20:27,30,37:38,44,46)]
# write.csv(df2, file = "new2.csv")
df_train <- data.frame(df2 %>% filter(year>2009 & year<2017))
df_test <- data.frame(df2 %>% filter(year>=2017))
df_train <- df_train[,-c(26)]
df_test <- df_test[,-c(26)]
df_train2 <- df_train[,-c(26:27)]
df_test2 <- df_test[,-c(26:27)]

# train model!
control <- trainControl(method = "cv",number = 5)

reg_lm <- train(price ~., data = df_train2 ,method= 'lm', trControl = control)
summary(reg_lm)

# plot 
train_dates <- sort(unique(df_train$tradeTimeM))
list1 <- list()
for (i in 1:length(train_dates)){
    current <- data.frame(df_train %>% filter(tradeTimeM == train_dates[i]))
    current_pred <- mean(predict(reg_lm,current))
    list1[[i]] <- data.frame('Prediction'= current_pred,'True' = mean(current$price),'date'= train_dates[i])
}
list2 <- list()
test_dates <- sort(unique(df_test$tradeTimeM))
for (i in 1:length(test_dates)){
    current <- data.frame(df_test %>% filter(tradeTimeM == test_dates[i]))
    current_pred <- mean(predict(reg_lm,current))
    list2[[i]] <- data.frame('Prediction'= current_pred,'True' = mean(current$price),'date'= test_dates[i])
}
LG_TRAIN <- do.call('rbind',list1)
LG_TRAIN$split<-rep('train', nrow(LG_TRAIN))
LG_TEST<- do.call('rbind',list2)
LG_TEST$split<-rep('test', nrow(LG_TEST))

LG <- data.frame(rbind(LG_TRAIN, LG_TEST))
a <- melt(id=c('date','split'),LG)
ggplot(aes(x=date,y=value),data = a) + geom_line(aes(color=variable),size=1) + geom_vline(xintercept= as.Date('2017-01-01') ) 

# observe the error
mse <- mean((LG$True[1:79]-LG$Prediction[1:79])^2)
rmse<-mse^0.5
mse2 <- mean((LG$True[80:91]-LG$Prediction[80:91])^2)
rmse2 <-mse2^0.5
sprintf("Trainset RMSE : %f ", rmse)
sprintf("Testset RMSE : %f ", rmse2 )
