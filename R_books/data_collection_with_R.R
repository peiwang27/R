### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### 基于R语言的自动数据收集
### --------------------------------------------------------------

# 加载相关的包----
library(tidyverse)
library(lubridate)
library(stringr)
library(XML)
library(maps)

# 设置数据的路径----
windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)

# 第一章 概述---------------------------------------------------
setwd(file.path(data_path,
                '/基于R语言的自动数据收集/ch-1-introduction'))
#url = 'http://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger'
#heritage_parsed <- htmlParse(url, encoding = 'utf-8')

heritage_parsed <- htmlParse('worldheritagedanger.htm',
                             encoding = 'utf-8')
tables <- readHTMLTable(heritage_parsed, stringsAsFactors=F)
class(tables)
names(tables)

danger_table <- tables[[2]]
names(danger_table)
danger_table <- danger_table[, c(1, 3, 4, 6, 7)]
names(danger_table) <- c('name', 'locn', 'crit', 'yins', 'yend')
danger_table_clean <- danger_table %>%
  mutate(crit=ifelse(str_detect(crit, 'Natural'),'nat', 'cult'),
         yins=as.numeric(yins),
         yend=as.numeric(str_replace(yend, '.*(\\d{4}$)', '\\1')))


