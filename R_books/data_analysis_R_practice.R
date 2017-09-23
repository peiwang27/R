# 数据分析R语言实战
# 李诗羽、张飞、王正林 编著

# 加载相关的包
library(tidyverse)
library(data.table)

# 数据路径设置
windows_path <- 'D:/WorkSpace/CodeSpace/Code.Data/R'
mac_path <- '/Users/machuan/CodeSpace/Code.Data/R'
data_path = ifelse(Sys.info()[1]=='Windows',
                   windows_path,
                   mac_path)

# 全部的数据都保存在该路径下
file_path = file.path(data_path, '数据分析_R语言实战/data')
setwd(file_path)

# charpter 2 --------------------------------------------------
# 读取某个包中的数据
library(MASS)
data(package = 'MASS')

# 读取文本文件
salary <- read.table('salary.txt', header = T)
salary2 <- scan('salary.txt',
               what = list(City='', Work=0, Price=0, Salary=0),
               skip = 1)


# charpter 3 --------------------------------------------------
Salary <- salary$Salary
salary_cut <- cut(Salary, 3, labels = c('low', 'medium', 'high'))
table(salary_cut)

# 数据的缺失模式
library(mice)
salary$Salary[salary$Salary>40] <- NA
md.pattern(salary)


# 对缺失数据的处理
imp <- mice(salary, seed=1)
fit <- with(imp, lm(Salary~Work + Price))
pooled <- pool(fit)
summary(pooled)

salary.pre <- salary[is.na(salary$Salary), ][, c(2,3)]
salary.pre <- as.matrix(cbind(rep(1, 11), salary.pre)) # 构造了一个矩阵
q <- pooled$qbar
pre <- salary.pre %*% q
index <- is.na(salary$Salary)
salary$Salary[index] <- pre
salary

# charpter 4 -------------------------------------------------
shoppingResult <- read.table('online shopping.txt', header = T)
with(shoppingResult, plot(period, amount))