### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### 基于R语言的自动数据收集
### --------------------------------------------------------------

# 加载相关的包----
library(tidyverse)
library(data.table)
library(XML)
library(magrittr)

# 数据测试部分
wiki_tables <- readHTMLTable('./datasets/worldheritagedanger.htm')
danger_table <- wiki_tables[[2]]

url <- './datasets/fortunes.html'
parsed_fortunes <- htmlParse(url)
