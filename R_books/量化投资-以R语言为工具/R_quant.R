# R_quant

# 加载相关的包
library(tidyverse)
library(lubridate)
library(stringr)
library(quantmod)
library(data.table)
library(xts)
library(PerformanceAnalytics)

# 设置数据的路径
windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)

stockZA_file_path <- 
  file.path(data_path,
            '量化投资_以R语言为工具/part 3/018/stockszA.csv')

stockza <- fread(stockZA_file_path, header = T, stringsAsFactors = F,
                    na.strings = '')
names(stockza)

wanke_close <- stockza[Stkcd==2, Clsprc]
wanke_close_lag <- lag(wanke_close, 1)
calc_wanke_close <- cbind(wanke_close, wanke_close_lag) %>% as.data.table()
row.names(calc_wanke_close) <- stockza[Stkcd==2, Trddt]

