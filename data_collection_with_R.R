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


### --------------------------------------------------------------
### introduction
### --------------------------------------------------------------

# heritage_parsed <- htmlParse("https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger", encoding = "UTF-8")
heritage_parsed <- file.path(data_path, '基于R语言的自动数据收集/ch-1-introduction/worldheritagedanger.htm')
tables <- readHTMLTable(heritage_parsed, stringsAsFactors=F)
danger_table <- tables[[2]]
danger_table <- danger_table[, c(1,3,4,6,7)]
names(danger_table) <- c('name', 'local', 'crit', 'yins', 'yend')
danger_table$crit <- ifelse(str_detect(danger_table$crit, 'Natural'),
                            'natural',
                            'cult')
danger_table$yins <- as.numeric(danger_table$yins)
danger_table$yend <- danger_table$yend %>%
  str_subset('[[:digit:]]4$') %>%
  as.numeric()
reg_y <- '[/][ -]*[[:digit:]]*[.]*[[:digit:]]*[;]'
reg_x <- '[;][ -]*[[:digit:]]*[.]*[[:digit:]]*'
y_coords <- danger_table$local %>%
  str_extract(reg_y) %>%
  str_sub(3, -2) %>%
  as.numeric() %>%
  round(2)
x_coords <- danger_table$local %>%
  str_extract(reg_x) %>%
  str_sub(3,-1) %>%
  as.numeric() %>%
  round(2)
danger_table$y_coords <- y_coords
danger_table$x_coords <- x_coords
danger_table$local <- NULL

pch <- ifelse(danger_table$crit=='cult', 19, 2)
map('world', col='darkgrey', lwd=0.5)
points(danger_table$y_coords, danger_table$x_coords, pch=pch)
box()


### --------------------------------------------------------------
### html
### --------------------------------------------------------------
url <- 'http://www.r-datacollection.com/materials/html/fortunes.html'
fortunes <- readLines(con=url)
parsed_fortunes <- htmlParse(url)

# 删除节点
h1 <- list('body'=function(x) NULL)
parsed_fortunes <- htmlTreeParse(url, handlers = h1, asTree = T)

h2 <- list(
  startElement = function(node, ...){
    name <- xmlName(node)
    if(name %in% c("div", "title")){NULL}else{node}
  },
  comment = function(node){NULL}
)

parsed_fortunes <- htmlTreeParse(url, handlers = h2, asTree = T)
