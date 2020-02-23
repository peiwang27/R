# baidu index data capture

library(tidyverse)
library(rlist)
library(magrittr)
library(httr)
library(jsonlite)
library(rlist)


query <- c('问诊','免费问诊', '测试')
query <- paste(query, collapse = ',')
APIKey <- '36a3a9559c254d559a9b0e4fc6d7798d' #chuan.ma@qq.com
APIUrl <- 'http://api.91cha.com/index' # 每次10个query，每天50次

# sample
# http://api.91cha.com/index
#?key=36a3a9559c254d559a9b0e4fc6d7798d&kws= 1,2,3,4,5,6,7,8,9,10

url = paste(APIUrl, 
            '?key=',
            APIKey,
            '&kws=',
            query,
            sep = '')
print(url)

response <- GET(url, user_agent='test')
result <- fromJSON(content(response, as='text'))
df <- result$data