# R_quant

# 加载相关的包
library(tidyverse)
library(lubridate)
library(stringr)
library(quantmod)

# 设置数据的路径
windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)