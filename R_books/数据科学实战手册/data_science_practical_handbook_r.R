####--------------------------------------------------------------
####数据科学实战手册
####--------------------------------------------------------------

# 加载相关library
library(tidyverse)
library(stringr)
library(lubridate)
library(XML)
library(RSQLite)
library(zoo)
library(reshape2)
library(maps)
library(RColorBrewer)
library(choroplethr)
library(data.table)
library(bit64)
library(ggplot2)

#设置路径 默认路径
windows_path <- 'D:/WorkSpace/CodeSpace/R/R'
mac_path <- '/Users/machuan/CodeSpace/R/R'
data_path <- ifelse(Sys.info()[1]=='Windows', windows_path, mac_path)

# 数据路径设置
ch_data_path <- str_c(data_path, '/datasets/数据科学实战手册/Chapter0',
                      1:5, '/data', sep='')

# ch02 汽车数据可视化---------------------------------------------
setwd(ch_data_path[2])

vehicles <- read_csv(unz('vehicles.csv.zip', 'vehicles.csv'),
                     na='')
names(vehicles)
vehilces_label <- do.call(rbind,
                          str_split(readLines('varlabels.txt'), '-'))
vehicles$trany2 <- if_else(str_sub(vehicles$trany, 1, 4)=='Manu',
                           'Manu', 'Auto') %>% as.factor()

mpgByYr <- vehicles %>% # 用管道运算符处理
  group_by(year) %>%
  summarise(avgMPG=mean(comb08),
            avgHghy=mean(highway08),
            avgCity=mean(city08))
mpgByYr <- plyr::ddply(vehicles, c('year'), summarise, # use plyr::ddply
                       avgMPG=mean(comb08),
                       avgHghy=mean(highway08),
                       avgCity=mean(city08))

mpgByYr %>%
  ggplot(aes(x=year, y=avgMPG)) +
  geom_point() + geom_smooth() +
  xlab('Year') + ylab('AvgMpg') + ggtitle('All cars')

gascars <- vehicles %>% # 管道运算符
  filter(fuelType1 %in% c('Regular Gasoline',
                          'Premium Gasoline',
                          'Midgrade Gasoline'),
         atvType != 'Hybrid')

mpgByYr_gas <- gascars %>%
  group_by(year) %>%
  summarise(avgMPG=mean(comb08),
            avgHghy=mean(highway08),
            avgCity=mean(city08))

mpgByYr_gas <- plyr::ddply(gascars,
                           c('year'),
                           summarise,
                           avgMPG = mean(comb08))

mpgByYr_gas %>%
  ggplot(aes(x=year, y=avgMPG)) +
  geom_point() + geom_smooth() +
  labs(x='year', y='avgMPG', title='gasonline cars', subtitle='just for fun')

ggplot(gascars, aes(x=displ, y=comb08)) + geom_point() + geom_smooth()

avgCarSize <- plyr::ddply(vehicles, c('year'),
                          summarise,
                          avgDispl=mean(displ))
ggplot(avgCarSize, aes(x=year, y=avgDispl)) + geom_point() + geom_smooth() +
  xlab('year') + ylab('avg displ') + ggtitle('avg displ per year')

# ch03 模拟美式橄榄球比赛数据---------------------------------------
year <- 2013
url <- paste("http://sports.yahoo.com/nfl/stats/byteam?group=Offense&cat=Total&conference=NFL&year=season_",
             year,"&sort=530&old_category=Total&old_group=Offense", sep='')
offense <- readHTMLTable(url, encoding='UTF-8', colClasses = 'Charater')[[7]] # 原url失效，需要重新确认新的url
# ch04 分析股票市场数据---------------------------------------------
setwd(ch_data_path[4])
# 读取一行数据判断是否数据有标题
read_lines('finviz.csv', n_max=1)
finviz <- read_csv('finviz.csv', na='')

clean_numeric <- function(s){
  s %>%
    str_replace_all('%|\\$|\\\\|,|\\)|\\(', '') %>%
    as.numeric()
}
finviz_clean <- cbind(finviz[1:6], apply(finviz[7:69], 2, clean_numeric))

hist(finviz$Price[finviz$Price<150], breaks=100, xla='Price')

sector_avg_price <- finviz %>%
  group_by(Sector) %>%
  summarise(sector_avg_price=mean(Price))

sector_avg_price %>%
  ggplot(aes(Sector, sector_avg_price)) +
  geom_bar(stat='identity') +
  labs(x='sector', y='sector_avg_price', title='Sector Avg Price') +
  theme(axis.text = element_text(angle = 45, hjust = 1))


sector_avg <- finviz %>%
  melt(id='Sector', variable.name='variable') %>%
  filter(variable %in% c('Price',
                         'P/E',
                         'PEG',
                         'P/S',
                         'P/B')) %>%
  filter(!is.na(value))


# ch05 就业数据的可视化---------------------------------------------
setwd(ch_data_path[5])

#ann2012 <- read_csv(unz('2012_annual_singlefile.zip',
#                        '2012.annual.singlefile.csv'))
# 此处使用fread的速度相对较快，但是需要手动解压缩
# 对于文件中存在特别大的数字，需要library(bit64)
ann2012 <- fread('2012.annual.singlefile.csv')

for(u in c('agglevel', 'area', 'industry', 'ownership', 'size')){
  assign(u, read.csv(paste(u, '_titles.csv', sep=''),
                     header = T,
                     stringsAsFactors = F))
}

code = c('agglevel', 'industry', 'ownership', 'size')
ann2012_full <- ann2012
for(i in 1:length(code)){
  # text=<string>参数不可省略...
  eval(parse(text=paste('ann2012_full <- left_join(ann2012_full,',
                        code[i],')',
                        sep='')))
}

area$area_fips <- str_to_title(area$area_fips)
data("county.fips")
county.fips$fips <- str_pad(county.fips$fips, width = 5, pad = 0)
county.fips$county <-
  str_replace(county.fips$polyname, '[a-z\ ]+,([a-z\ ]+)','\\1')
county.fips$county <- str_to_title(county.fips$county)

data(state.fips)
state.fips$fips <- str_pad(state.fips$fips,
                           width=2, pad='0', side='left')
state.fips$state <-
  str_replace(state.fips$polyname, "([a-z\ ]+):[a-z\ \\']+", '\\1')
state.fips$state <- str_to_title(state.fips$state)
mystatefips <- unique(state.fips[, c('fips', 'abb', 'state')])
lower48 <- setdiff(unique(state.fips$state), c('Hawaii', 'Alaska'))
myarea <- merge(area, county.fips,
                by.x='area_fips', by.y='fips',
                all.x =T)
myarea$state_fips <- substr(myarea$area_fips, 1, 2)
myarea <- merge(myarea, mystatefips,
                by.x='state_fips', by.y='fips',
                all.x = T)

ann2012_full <- left_join(ann2012_full, myarea) %>%
  filter(state %in% lower48)

glimpse(ann2012_full)

d.state <- ann2012_full %>%
  filter(agglvl_code==50) %>%
  select(state, avg_annual_pay, annual_avg_emplvl)
d.state$wage <- cut(d.state$avg_annual_pay,
                    quantile(d.state$avg_annual_pay,
                             c(seq(0,0.8,by=0.2), 0.9, 0.95, 0.99, 1)))
d.state$empquantile <- cut(d.state$annual_avg_emplvl,
                           quantile(d.state$annual_avg_emplvl,
                                    c(0,0.8,by=0.2), 0.9, 0.95, 0.99, 1))

x1 <- quantile(d.state$avg_annual_pay,
               c(seq(0,0.8,by=0.2), 0.9, 0.95, 0.99, 1))
xx1 <- paste(round(x1/1000), 'K', sep='')
labs1 <- paste(xx1[-length(xx1)], xx1[-1], sep='-')
levels(d.state$wage) <- labs1

x2 <- quantile(d.state$annual_avg_emplvl,
               c(seq(0,0.8,by=0.2), 0.9, 0.95, 0.99, 1))
xx2 <- ifelse(x2>1000, paste(round(x2/1000), 'K', sep=''), round(x2))
labs2 <- paste(xx2[-length(xx2)], xx2[-1], sep='-')
levels(d.state$empquantile) <- labs2


Discretize <- function(x, breaks=NULL){
  if(is.null(breaks)){
    breaks <- quantile(x, c(seq(0,0.8,by=0.2), 0.9, 0.95, 0.99, 1))
    if(sum(breaks==0)>1){
      temp <- which(breaks==0, arr.ind = T)
      breaks <- breaks[max(temp):length(breaks)]
    }
  }
  x.discrete <- cut(x, breaks, include.lowest = T)
  breaks.eng <- ifelse(breaks>1000,
                       paste0(round(breaks/1000), 'K', sep=''),
                       round(breaks))
  Labs <- paste(breaks.eng[-length(breaks.eng)], breaks.eng[-1], sep='-')
  levels(x.discrete) <- Labs
  return(x.discrete)
}


d.county <- ann2012_full %>%
  filter(agglvl_code==70) %>%
  select(state, county, abb, avg_annual_pay, annual_avg_emplvl) %>%
  mutate(wage = Discretize(avg_annual_pay),
         empquantile = Discretize(annual_avg_emplvl))

state_df <- map_data('state') %>%
  mutate(state = str_to_title(region),
         county = str_to_title(subregion))
county_df <- map_data('county')  %>%
  mutate(state = str_to_title(region),
         county = str_to_title(subregion))

chor_state <- left_join(state_df, d.state, by='state')
ggplot(chor_state, aes(long, lat, group=group)) +
  geom_polygon(aes(fill=wage)) +
  geom_path(color='black', size=0.2) +
  scale_fill_brewer(palette = 'PuRd') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

chor_county <- left_join(county_df, d.county, by='county')
ggplot(chor_county, aes(long, lat, group=group)) +
  geom_polygon(aes(fill=wage)) +
  geom_path(color='black', size=0.2) +
  scale_fill_brewer(palette = 'PuRd') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


d.sectors <- ann2012_full %>%
  filter(industry_code %in% c(11,21,54,52),
         own_code==5,
         agglvl_code==74) %>%
  select(state, county, industry_code, own_code, agglvl_code,
         industry_title, own_title, avg_annual_pay,
         annual_avg_emplvl) %>%
  mutate(wage=Discretize(avg_annual_pay),
         emplevel=Discretize(annual_avg_emplvl)) %>%
  filter(!is.na(industry_code))

chor_sector <- left_join(county_df, d.sectors, by='county')
ggplot(chor_sector, aes(long, lat, group=group)) +
  geom_polygon(aes(fill=emplevel)) +
  geom_polygon(data=state_df, color='black', fill=NA) +
  scale_fill_brewer(palette='PuRd') +
  facet_wrap(~industry_title, ncol=2, as.table = T) +
  labs(fill='Avg Employment Level', x='', y='') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())