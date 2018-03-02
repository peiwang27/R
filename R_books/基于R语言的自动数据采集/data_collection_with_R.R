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
library(plyr)

# 设置数据的路径----
windows_path <- 'D:/WorkSpace/CodeSpace/R/R'
mac_path <- '/Users/machuan/CodeSpace/R/R'
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
parsed_fortunes <- htmlParse('./datasets/基于R语言的自动数据收集/ch-2-html/fortunes.html')

h1 <- list('body'=function(x){NULL})
parsed_fortunes1 <- htmlTreeParse('./datasets/基于R语言的自动数据收集/ch-2-html/fortunes.html',
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

parsed_fortunes2 <- htmlTreeParse('./datasets/基于R语言的自动数据收集/ch-2-html/fortunes.html',
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
invisible(htmlTreeParse('./datasets/基于R语言的自动数据收集/ch-2-html/fortunes.html',
                        handlers = h3))
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

# 第五章 Http-------------------------------------------------
setwd(file.path(data_path,
                '基于R语言的自动数据收集/ch-5-http'))
# 基本请求方法 - get方法
## 获取图片的地址后可以下载了
getURL('http://www.r-datacollection.com/materials/http/helloworld.html')
url <- str_c('http://img2.ph.126.net/SicEe2pmb3km8yGe5kZsKQ',
             '==/6632435558166361073.jpg',
             sep='')
getBinaryURL(url)
getBinaryURL(url) %>% writeBin('test.jpg')

url <- str_c('http://www.r-datacollection.com',
             '/materials/http/helloworld.html',
             sep='')
getURL(url)

url <- "http://www.r-datacollection.com/materials/http/GETexample.php"
cat(getForm(url, name='Eddie', age=32))

# 基本请求方法 - post
url <- "http://www.r-datacollection.com/materials/http/POSTexample.php"
cat(postForm(url, name='Eddie', age=32, style='post'))

# RCurl的底层函数
url <- 'http://www.r-datacollection.com/materials/http/helloworld.html'
curlPerform(url=url)


# 第九章 scraping--------------------------------------------
setwd(file.path(data_path,
                '基于R语言的自动数据收集/ch-9-scraping'))
url <- str_c('http://www.elections.state.md.us/elections/2012',
             '/election_data/index.html',
             sep='')
links <- getHTMLLinks(url)

# 下载csv文件
downloadCSV <- function(filename, baseurl, folder){
  dir.create(folder, showWarnings = F)
  fileurl <- str_c(baseurl, filename, sep='')
  if(file.exists(str_c(folder, '/', filename, sep=''))){
    download.file(fileurl,
                  destfile = str_c(folder, '/', 'filename',
                                   sep=''))
    Sys.sleep(1)
  }
}


# 从HTML采集链接、列表和表格
mac_url <- 'http://en.wikipedia.org/wiki/Machiavelli'
mac_source <- readLines(mac_url)
mac_parsed <- htmlParse(mac_source)
mac_node <- mac_parsed['//p'][[1]]
links <- getHTMLLinks(mac_source)

getHTMLLinks(mac_source,
             xpQuery = '//a[@class="extiw"]/@href')
readHTMLList(mac_source)
readHTMLTable(mac_source)
