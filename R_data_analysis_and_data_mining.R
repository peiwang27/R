# R语言数据分析与数据挖掘实战
# library----
library(tidyverse)
library(stringr)
library(jsonlite)
data_path <- file.path('D:/WorkSpace/CodeSpace/data',
                       'R语言数据分析与挖掘实战/数据及代码')


setwd(file.path(data_path, 'chapter3/data'))
saledata <- read.csv('catering_sale.csv',
                     header = T,
                     stringsAsFactors = F)
dim(saledata)
complete.cases(saledata) %>% sum
complete.cases(saledata) %>% mean
