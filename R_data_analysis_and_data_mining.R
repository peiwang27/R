# R语言数据分析与挖掘实战
# library----
library(tidyverse)
library(stringr)
library(jsonlite)
library(lubridate)
library(data.table)

windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)


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
