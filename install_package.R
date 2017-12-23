# 用于重新安装新版本的R时，进行安装的包的迁移
#library(tidyverse)
#library(data.table)

# 保存历史的包
row.names(installed.packages()) %>%
  write.table('installed.txt')

# 保存之后安装新的版本的R open
wants <- read.table('installed.txt')
has <- wants[,1] %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has, 1])
