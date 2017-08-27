### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### 基于R语言的自动数据收集
### --------------------------------------------------------------

# 加载相关的包----
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(XML)
library(maps)
library(RJSONIO)
library(RCurl)

# 设置数据的路径----
windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)

# 第一章 概述---------------------------------------------------

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

reg_y <- "[/][ -]*[[:digit:]]*[.]*[[:digit:]]*[;]"
reg_x <- "[;][ -]*[[:digit:]]*[.]*[[:digit:]]*"
y_coords <- str_extract(danger_table$locn, reg_y)
y_coords <- str_sub(y_coords, 3, -2) %>%
  as.numeric() %>% round(2)
x_coords <- str_extract(danger_table$locn, reg_x)
x_coords <- str_sub(x_coords, 3, -1) %>%
  as.numeric() %>% round(2)
danger_table_clean$y_coords <- y_coords
danger_table_clean$x_coords <- x_coords
danger_table_clean$locn <- NULL

pch <- ifelse(danger_table_clean$crit=='nat', 19, 2)
map('world', col='darkgrey', lwd=1,
    mar=c(0.1, 0.1, 0.1, 0.1))
points(danger_table_clean$x_coords, danger_table_clean$y_coords,
       pch=pch)
box()

# 第二章 HTML-----------------------------------------------------
setwd(file.path(data_path,
                '基于R语言的自动数据收集/ch-2-html'))
parsed_fortunes <- htmlParse('fortunes.html')

h1 <- list('body'=function(x){NULL})
parsed_fortunes1 <- htmlTreeParse('fortunes.html',
                                  handlers = h1,
                                  asTree = T)

h2 <- list(
  startElement = function(node, ...){
    name <- xmlName(node)
    if(name %in% c('div', 'title')){NULL}
    else{node}
  },
  comment = function(node) {NULL}
)

parsed_fortunes2 <- htmlTreeParse('fortunes.html',
                                  handlers = h2,
                                  asTree = T)

geItalics <- function(){
  i_container = character()
  list(i = function(node, ...){
    i_container <<- c(i_container, xmlValue(node))
  },
  return_I = function() i_container)
}
h3 <- geItalics()
invisible(htmlTreeParse('fortunes.html', handlers = h3))
h3$return_I()


# 第三章 XML and JSON---------------------------------------------
# xml
setwd(file.path(data_path,
                '基于R语言的自动数据收集/ch-3-xml'))

bond <- xmlParse('bond.xml')
root <- xmlRoot(bond)
xmlName(root)
xmlSize(root)


# json
isValidJSON('indy.json')
con <- file('indy.json', 'r')
indy <- RJSONIO::fromJSON(con)

indy.vec <- unlist(indy, recursive = T, use.names = T)
indy.vec[str_detect(names(indy.vec), 'name')]


# 第四章 XPath
setwd(file.path(data_path,
                '基于R语言的自动数据收集/ch-4-xpath'))
parsed_doc <- parsed_fortunes
xpathSApply(parsed_doc, path = '/html/body/div/p/i')
xpathSApply(parsed_doc, path='//p/i')

parsed_stocks <- xmlParse(file='technology.xml')
company <- c('Apple', 'IBM', 'Google')
expQuery <- sprintf('//%s/close', company)

getClose <- function(node){
  value <- xmlValue(node)
  company <- xmlName(xmlParent(node))
  mat <- c(company=company, value=value)
}

stocks <- xpathSApply(parsed_stocks, expQuery, getClose) %>%
  t %>%
  as.data.table() %>%
  mutate(value=as.numeric(value)) %>%
  as.data.table
