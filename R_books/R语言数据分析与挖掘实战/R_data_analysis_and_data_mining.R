# R语言数据分析与挖掘实战
rm(list=ls())
# library----
library(tidyverse)
library(stringr)
library(jsonlite)
library(lubridate)
library(data.table)
library(tidyr)
library(RMySQL)
library(jiebaRD)
library(jiebaR)

windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace//Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)
csv_encoding <- ifelse(Sys.info()[1]=='Windows', 'UTF-8', 'gb')

# chapter3---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter3/data'))
saledata <- fread('catering_sale.csv',
                  header = T,
                  stringsAsFactors = F,
                  na.strings = '')
str(saledata)
dim(saledata)
length(unique(saledata$日期))
complete.cases(saledata) %>% sum
apply(saledata, 2, function(x) sum(is.na(x)))
saledata[is.na(saledata$销量), ]
sp <- boxplot(saledata$销量, boxwex=0.7)
title('check outlier values')
saledata$日期 <- ymd(saledata$日期)
plot(saledata$日期, saledata$销量, type='b')

dishdata <- fread('catering_dish_profit.csv', header=T,
                  stringsAsFactors=F, na.strings='')
barplot(dishdata$盈利, names.arg = dishdata$菜品名,
        xlab='菜品', ylab='盈利额')

accratio <- cumsum(dishdata$盈利)/sum(dishdata$盈利)
op <- par()
par(new=T, mar=c(4,4,4,4))
points(c(1:length(accratio)-0.5), accratio*10000, type='b')
axis(4, col='red', col.axis='red', at=0:10000, label=c(0:10000/10000))

# chapter5---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter5/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = '',
                  encoding = 'UTF-8'))
}
bankloan_train_data <- bankloan
names(bankloan_train_data) <- c(paste('x', 1:8, sep=''), 'y')
bankloan_glm <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8,
                    data=bankloan_train_data,
                    family = binomial(link=logit))

summary(bankloan_glm)
logit.setp1 <- step(bankloan_glm, direction = 'both')
logit.setp2 <- step(bankloan_glm, direction = 'forward')
logit.step3 <- step(bankloan_glm, direction = 'backward')

km <- kmeans(consumption_data, centers = 3)
print(km)
consumption_data_group <- cbind(consumption_data, km$cluster)

# chapter6---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter6/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}

trainData$class <- as.factor(trainData$class)
library(nnet)
nnet.model <- nnet(class~ele_ind+loss_ind+alarm_ind,
                   trainData,
                   size=10, decay=0.05)
summary(nnet.model)
confusion <- table(trainData$class,
                   predict(nnet.model, trainData, type='class'))
accuracy <- sum(diag(confusion))/sum(confusion)

library(tree)
tree.model <- tree(class~ele_ind+loss_ind+alarm_ind, trainData)
summary(tree.model)
plot(tree.model)
text(tree.model)
consufion <- table(trainData$class,
                   predict(tree.model, trainData, type='class'))
accuracy <- sum(diag(confusion))/sum(confusion)


# chapter7---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter7/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}

clean_air_data <- air_data %>%
  select(15:18, 20:29) %>%
  filter(!is.na(SUM_YR_1), !is.na(SUM_YR_2)) %>%
  filter(!((SUM_YR_1==0|SUM_YR_2==0)&(avg_discount!=0)&(SEG_KM_SUM>0)))

sub_air_data <- air_data %>%
  filter(!is.na(SUM_YR_1), !is.na(SUM_YR_2)) %>%
  filter(!((SUM_YR_1==0|SUM_YR_2==0)&
             (avg_discount!=0)&
             (SEG_KM_SUM>0))) %>%
  select(LOAD_TIME, FFP_DATE, LAST_TO_END,
         FLIGHT_COUNT, SEG_KM_SUM, avg_discount) %>%
  mutate(LOAD_TIME=ymd(LOAD_TIME),
         FFP_DATE=ymd(FFP_DATE),
         L=as.numeric((LOAD_TIME-FFP_DATE)/30),
         R=LAST_TO_END,
         F_=FLIGHT_COUNT,
         M=SEG_KM_SUM,
         C=avg_discount) %>%
  select(L, R, F_, M, C)

result <- kmeans(zscoreddata[, -1], centers = 5)
stars(result$centers, draw.segments = T, key.loc = c(0, 5))


# chapter8---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter8/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}
names(data_)
names(processedfile)
dis_con <- function(x, char){
  temp <- kmeans(x, 4, nstart=20)
  temp_sym <- paste(temp$cluster, char, sep='')
  return(temp_sym)
}
model_data <- data_ %>%
  mutate(x1=dis_con(肝气郁结证型系数, 'A'),
         x2=dis_con(热毒蕴结证型系数, 'B'),
         x3=dis_con(冲任失调证型系数, 'C'),
         x4=dis_con(气血两虚证型系数, 'D'),
         x5=dis_con(脾胃虚弱证型系数, 'E'),
         x6=dis_con(肝肾阴虚证型系数, 'F')) %>%
  select(x1,x2,x3,x4,x5,x6,TNM分期)

library(arules)
processedfile <- apply(processedfile, 2, as.factor) %>% as.data.frame()
trans <- as(processedfile, 'transactions')
rules <- apriori(trans,
                 parameter = list(support=0.06, confidence=0.75))


# chapter9---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter9/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}
names(moment) <- c('class', 'id',
                   paste(rep(c('R','G','B'),3),
                         rep(c(1,2,3), each=3), sep=''))

library(e1071)
trainData <- transform(trainData, class=as.factor(class))
testData <- transform(testData, class=as.factor(class))
svm.model <- svm(class~., data=trainData[, -2])
confusion <- table(trainData$class,
                   predict(svm.model, trainData, type='class'))
accuracy <- sum(diag(confusion))/sum(confusion)

# chapter10---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter10/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}
water_heater$event_num <- as.numeric(row.names(water_heater))
model.water_heater <- water_heater %>%
  mutate(发生时间=ymd_hms(发生时间)) %>%
  filter(水流量!=0) %>%
  mutate(fdiff=发生时间-lead(发生时间, 1),
         bdiff=发生时间-lag(发生时间, 1)) %>%
  filter(!is.na(bdiff), !is.na(fdiff)) %>%
  mutate(headornot=ifelse(abs(fdiff)>240, 1, 0),
         endornot=ifelse(abs(bdiff)>240, 1, 0)) %>%
  filter((headornot!=0)|(endornot!=0)) %>%
  mutate(head_seq=ifelse(headornot==0, 0, event_num),
         end_seq=ifelse(endornot==0, 0, event_num))

event_seq <- cbind(
  model.water_heater %>% select(head_seq) %>% filter(head_seq!=0),
  model.water_heater %>% select(end_seq) %>% filter(end_seq!=0))

event_data <- cbind(1:dim(event_seq)[1], event_seq)
names(event_data) <- c('事件序号', '事件起始编号', '事件结束编号')

devide_event <- function(df, thresh_hold){
  model.water_heater <- df %>%
    mutate(发生时间=ymd_hms(发生时间)) %>%
    filter(水流量!=0) %>%
    mutate(fdiff=发生时间-lead(发生时间, 1),
           bdiff=发生时间-lag(发生时间, 1)) %>%
    filter(!is.na(bdiff), !is.na(fdiff)) %>%
    mutate(headornot=ifelse(abs(fdiff)/60>thresh_hold, 1, 0),
           endornot=ifelse(abs(bdiff)/60>thresh_hold, 1, 0)) %>%
    filter((headornot!=0)|(endornot!=0)) %>%
    select(event_num, headornot, endornot) %>%
    mutate(head_seq=ifelse(headornot==0, 0, event_num),
           end_seq=ifelse(endornot==0, 0, event_num))
  
  event_seq <- cbind(
    model.water_heater %>% select(head_seq) %>% filter(head_seq!=0),
    model.water_heater %>% select(end_seq) %>% filter(end_seq!=0))
  
  event_data <- cbind(1:dim(event_seq)[1], event_seq)
  names(event_data) <- c('事件序号', '事件起始编号', '事件结束编号')
  return(dim(event_data)[1])
}
devide_time <- seq(2, 8, by=0.25)
devide_result <- cbind(
  devide_time,
  sapply(devide_time, function(x) devide_event(water_heater, x))
) %>% as.data.frame()
names(devide_result) <- c('devide_time', 'event_num')

sub_df <- function(df, x){
  index <- which(df[, 1]==x)
  return(df[index:(index+4), ])
}

calc_a <- function(df){
  a <- rep(0, 4)
  for(i in 2:5){
    a[i] <- abs((df[i,2]-df[1,2])/(df[i,1]-df[1,1]))
  }
  return(mean(a))
}

calc_a_result <- cbind(
  lag(devide_result$devide_time,
      4)[!is.na(lag(devide_result$devide_time, 4))],
  sapply(
    lag(devide_result$devide_time,
        4)[!is.na(lag(devide_result$devide_time, 4))],
    function(x) calc_a(sub_df(devide_result, x))))

# chapter11---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter11/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}

disc_attr_data <- discdata %>%
  filter(DESCRIPTION=='磁盘容量')
disc_attr_data <-
  disc_attr_data[!duplicated(disc_attr_data$ENTITY), ]

disc_usage_data <- discdata %>%
  filter(DESCRIPTION=='磁盘已使用大小') %>%
  select(SYS_NAME, ENTITY, VALUE, COLLECTTIME) %>%
  spread(key='ENTITY', value='VALUE')
names(disc_usage_data) <- c('sys_name', 'coll_time', 'cwc', 'cwd')
names(discdata_processed) <- c('sys_name', 'coll_time', 'cwc', 'cwd')


# chapter12---------------------------------------------------------
setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter12/data'))
for(u in (dir() %>% str_subset('csv') %>% str_replace('.csv', ''))){
  assign(u, fread(paste(u, '.csv', sep = ''),
                  header = T,
                  stringsAsFactors = F,
                  na.strings = ''))
}
# 一些结论
# 101：咨询相关；102：律师相关；199：其他；107：知识类型
# 199中，lawfirm：律师事务所(106001)；
#        ask/exp、ask/online：咨询经验(101009)、在线咨询(101008)
# 199中的带？的页面，通过标题处理
# 瞎逛的用户
# 需要删除的几类数据：
# 1. 咨询发布成功页（标题）
# 2. 中间页面(midques_关键字)
# 3. 网址中带有？，同时无法还原其类型
# 4. 重复数据
# 5. 无点击html行为
# 6. 律师行为
# 7. 搜索引擎数据
con = dbConnect(MySQL(), username='root', password='',
                dbname='datascience')
dbSendQuery(con, 'set names gbk')
dbListTables(con)
dbListFields(con, 'all_gzdata')
res <- dbSendQuery(con, 'select * from all_gzdata')
df_sql <- fetch(res, n=-1)
dbDisconnect(con)
df_sql <- as.data.table(df_sql)

# 增加网页类别和网址的具体类型的分析
get_url_class <- function(x, n){
  
}

df_sql <- df_sql %>%
  mutate(url_type=substr(fullURL, 1, 3))

# 查看不同类型的网页信息
df_sql[, .(fullURLId, head(fullURL, 2)), keyby=fullURLId]

# 网页分类的修正
df_sql$fullURLId[(df_sql$fullURLId=='1999001')&
                   (str_detect(df_sql$fullURL,
                               'ask/exp'))] <- '101009'

df_sql$fullURLId[(df_sql$fullURLId=='1999001')&
                   (str_detect(df_sql$fullURL,
                               'ask/online'))] <- '101008'

df_sql$fullURLId[(df_sql$fullURLId=='1999001')&
                   (str_detect(df_sql$fullURL,
                               'lawfirm'))] <- '106001'

# 处理翻页的数据
df_sql <- df_sql %>%
  mutate(real_full_url=ifelse(
    str_detect(fullURL, '_\\d{1,2}.html$'),
               str_replace(fullURL, '(.*)_\\d{1,2}.html$', '\\1'),
               fullURL))

# 数据清洗
clean_df_sql  <- df_sql %>%
  unique %>%
  filter(str_detect(fullURL, 'lawtime')) %>%
  filter(str_detect(fullURL, '.html$')) %>%
  filter(!str_detect(fullURL, 'midques_')) %>%
  filter(!str_detect(pageTitle, '咨询发布|发布咨询')) %>%
  filter(!str_detect(pageTitle, '快车助手')) %>%
  filter(!((url_type=='199')&(str_detect(fullURL, '//?')))) %>%
  mutate(url_type_level1=(str_split(real_full_url,
                                    '/',
                                    simplify=T)[, 4]),
         url_type_level2=(str_split(real_full_url,
                                    '/',
                                    simplify=T)[, 5]))
  
