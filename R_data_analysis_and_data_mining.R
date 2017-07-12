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

import_csv_data <- function(x){
  x <- x %>% str_subset('csv') %>% str_replace('.csv', '')
  for(u in x){
    assign(u, fread(paste(u, '.csv', sep = ''),
                    header = T,
                    stringsAsFactors = F,
                    na.strings = ''))
  }
}

setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter3/data'))
saledata <- read.csv('catering_sale.csv',
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


setwd(file.path(data_path,
                'R语言数据分析与挖掘实战/数据及代码/chapter5/data'))
bankloan <- fread('bankloan.csv', header = T, stringsAsFactors = F,
                  na.strings = '')
bankloan_train_data <- bankloan
names(bankloan_train_data) <- c(paste('x', 1:8, sep=''), 'y')
bankloan_glm <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8,
                    data=bankloan_train_data,
                    family = binomial(link=logit))

summary(bankloan_glm)
logit.setp1 <- step(bankloan_glm, direction = 'both')
logit.setp2 <- step(bankloan_glm, direction = 'forward')
logit.step3 <- step(bankloan_glm, direction = 'backward')
import_csv_data(dir())

