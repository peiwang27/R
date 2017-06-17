####--------------------------------------------------------------
####数据科学实战手册
####--------------------------------------------------------------

# 加载相关library
library(tidyverse)
library(stringr)
library(lubridate)
library(XML)
#library(RSQLite)
library(zoo)

#设置路径
windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)

# 数据路径设置
ch_data_path <- file.path(data_path,
                            '数据科学实战手册_R_Python/Chapter0')
ch_data_path <- str_c(ch_data_path, 1:11, '/data')

# ch02 汽车数据可视化---------------------------------------------
setwd(ch_data_path[2])

vehicles <- read_csv(unz('vehicles.csv.zip', 'vehicles.csv'),
                     na='')
names(vehicles)
vehilces_label <- do.call(rbind,
                          str_split(readLines('varlabels.txt'), '-'))

vehicles$trany2 <- if_else(str_sub(vehicles$trany, 1, 4)=='Manu',
                           'Manu', 'Auto') %>% as.factor()

mpgByYr <- vehicles %>%
  group_by(year) %>%
  summarise(avgMPG=mean(comb08),
            avgHghy=mean(highway08),
            avgCity=mean(city08))

mpgByYr %>%
  ggplot(aes(x=year, y=avgMPG)) +
  geom_point() + geom_smooth() +
  xlab('Year') + ylab('AvgMpg') + ggtitle('All cars')

gascars <- vehicles %>%
  filter(fuelType1 %in% c('Regular Gasoline',
                         'Premium Gasoline',
                         'Midgrade Gasoline'),
         atvType != 'Hybrid')
mpgByYr_gas <- gascars %>%
  group_by(year) %>%
  summarise(avgMPG=mean(comb08),
            avgHghy=mean(highway08),
            avgCity=mean(city08))

mpgByYr_gas %>%
  ggplot(aes(x=year, y=avgMPG)) +
  geom_point() + geom_smooth() +
  labs(x='year', y='avgMPG', title='gas cars', subtitle='just for fun')

# ch03 模拟美式橄榄球比赛数据---------------------------------------
# 缺少安装SQLite包

# ch04 分析股票市场数据---------------------------------------------
setwd(ch_data_path[4])
# 读取一行数据判断是否数据有标题
read_lines('finviz.csv', n_max=1)
finviz <- read_csv('finviz.csv', na='')

clean_numeric <- function(s){
  return(
    s %>%
      str_replace_all('%|\\$|\\\\|,|\\)|\\(', '') %>%
      as.numeric()
  )
}
hist(finviz$Price[finviz$Price<150], breaks=100, xla='Price')
