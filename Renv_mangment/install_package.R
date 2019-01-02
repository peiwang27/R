# 用于重新安装新版本的R时，进行安装的包的迁移
library(tidyverse)
library(data.table)
library(stringr)

# run next code before install new version
row.names(installed.packages()) %>%
  write.table('installed.txt')

# 保存之后安装新的版本的R open
wants <- read.table('./R/Renv_mangment/installed.txt')
has <- wants[,1] %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has, 1])

# 部分包安装失败的处理
winDownloadPath <- 'C:\\Users\\Admin\\AppData\\Local\\Temp\\RtmpMduBmL\\downloaded_packages'
setwd(winDownloadPath) # 已经下载的包的路径
file_name <- dir(getwd())
package_name <- sapply(file_name, function(x){return(str_replace(x, '(.*)_(.*)', '\\1'))}) # 提取安装的包的名称
uninstall_package <- package_name[!(package_name %in% wants[,1])]

for (i in 1:length(uninstall_package)) {
  install.packages(str_subset(file_name, uninstall_package[i]),
                   repos=NULL,
                   type='source')
}