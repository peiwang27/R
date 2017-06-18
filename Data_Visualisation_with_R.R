# R语言绘图查询 - ggplot2 & Graphic
library(ggplot2)
library(reshape2)
# 散点图 ----
# points()函数
plot(rnorm(200), rnorm(200), type='p', col='red')
# geom_point()函数
df <- data.frame(x=rnorm(200), y=rnorm(200))
ggplot(df, aes(x=x, y=y))+geom_point()
# 折线图 ----
# lines()函数
data(cars)
plot(cars, main='Stopping Distance vs Speed')
lines(stats::lowess(cars))
# geom_line()
data(economics)
ggplot(economics, aes(x=date, y=unemploy))+geom_line()
# 条形图 ----
# barplot()函数
data("VADeaths")
barplot(VADeaths, beside=T, col=c('green', 'red', 'blue', 'black', 'yellow'),
        legend=row.names(VADeaths), ylim=c(0, 100))
# geom_bar()函数
data(mpg)
ggplot(mpg, aes(class))+geom_bar()
VADeaths_melt <- melt(VADeaths)
ggplot(VADeaths_melt, aes(x=Var2, y=value, fill=Var1))+
  geom_bar(stat='identity')
# 参数dodge类似barplot()函数的beside参数
ggplot(VADeaths_melt, aes(x=Var2, y=value, fill=Var1))+
  geom_bar(stat='identity', position='dodge')
# 柱状堆积比例图
ggplot(VADeaths_melt, aes(x=Var2, y=value, fill=Var1))+
  geom_bar(stat='identity', position='fill')
# 面积图 ----
# polygon()函数,存在比较明显的缺点，设置顺序出现问题，容易造成图层的叠加和遮挡
data(diamonds)
d <- density(diamonds[diamonds$cut=='Ideal',]$price)
plot(d, xlab='price', main='')
polygon(d, col = 'red', border = 'red')
d <- density(diamonds[diamonds$cut=='Premium',]$price)
polygon(d, col = 'yellow', border = 'yellow')
# geom_area()函数
ggplot(diamonds, aes(x=price, fill=cut))+geom_area(stat='bin')
# 密度估计图 ----
# plot(density())
set.seed(1234)
rating <- rnorm(100)
rating2 <- rnorm(100)
plot(density(rating))
lines(density(rating2), col='red')
# geom_density()函数
ggplot(diamonds, aes(x=depth, col=cut))+geom_density()
# 频率图像 ----
data(mtcars)
myhist <- hist(mtcars$mpg, plot=F)
mutiplier <- myhist$counts/myhist$density
mydensity <- density(mtcars$mpg)
mydensity$y <- mydensity$y*mutiplier[1]
plot(mydensity)
# geom_freqpoly()函数
ggplot(diamonds, aes(x=price, col=cut))+geom_freqpoly(binwidth=500)
# 直方图 ----
# hist()函数
data("islands")
hist(sqrt(islands), breaks=12, col='lightblue', border='pink')
# geom_hist()函数，保证输入的数据为data.frame
ggplot(as.data.frame(islands), aes(sqrt(islands)))+geom_histogram()
# 箱线图 ----
# boxplot()函数
data("InsectSprays")
boxplot(count~spray, data=InsectSprays, col='lightgray')
# geom_boxplot()函数
ggplot(InsectSprays, aes(x=spray, y=count))+
  stat_boxplot(geom='errorbar', width=0.5)+
  geom_boxplot()
# 提琴图 ----
# vioplot()函数
library(sm)
library(vioplot)
data(mtcars)
x1 <- mtcars$mpg[mtcars$cyl==6]
x2 <- mtcars$mpg[mtcars$cyl==4]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3, names=c('6 cyl', '4 cyl', '8 cyl'), col='gold')
# geom_violin()函数
ggplot(mtcars, aes(x=factor(cyl), y=mpg))+geom_violin()+geom_boxplot(width=0.1)
# Cleveland点图 ----
# dotchart()函数
data(mtcars)
dotchart(mtcars$mpg, labels=row.names(mtcars), cex=0.7,
         main='Gas Milage for Car Models',
         xlab='mpg')
# geom_dotplot()函数
ggplot(mtcars, aes(x=mpg, y=row.names(mtcars), fill=row.names(mtcars)))+
  geom_dotplot(binaxis = 'y',
               stackgroups = T,
               binwidth = 1,
               method = 'histodot')
# 热图 ----
# heatmap()函数
data("mtcars")
x <- as.matrix(mtcars)
rc <- rainbow(nrow(mtcars), start=0, end=0.3)
cc <- rainbow(ncol(mtcars), start=0, end=0.3)
hv <- heatmap(x,
              col = cm.colors(256),
              scale = 'column',
              RowSideColors = rc,
              ColSideColors = cc,
              margins = c(5, 10),
              xlab = "specification variables",
              ylab = "Car Models",
              main = "Heatmap of Mtcars data")
# pheatmap()函数
library(pheatmap)
test <- matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20,seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) <- paste('Test', 1:10, sep='')
rownames(test) <- paste('Gene', 1:10, sep='')
annotation_col = data.frame(
  CellType = factor(rep(c("CT1", "CT2"), 5)),
  Time = 1:5
)
rownames(annotation_col) = paste("Test", 1:10, sep = "")
# 设置每一行的注释
annotation_row = data.frame(
  GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6)))
)
rownames(annotation_row) = paste("Gene", 1:20, sep = "")
# 设置注释的颜色
ann_colors = list(
  Time = c("white", "firebrick"),
  CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
  GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)
pheatmap(test,
         annotation_col = annotation_col,
         annotation_row = annotation_row,
         annotation_colors = ann_colors)
# 第一第二主成分平面图 ----
library(devtools)
install_github('vqv/ggbiplot')
library(plyr)
library(scales)
library(grid)
library(ggbiplot)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = wine.class, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '')
# 层次聚类图 ----
# hclust()函数
data("USArrests")
hc <- hclust(dist(USArrests), "ave")
plot(hc)
# ggdendrogram()函数
library(ggdendro)
hc <- hclust(dist(USArrests), "ave")
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE, size=2) + 
  labs(title="Dendrogram in ggplot2")
# 系统发育图 ----
hc <- hclust(dist(USArrests), "ave")
library(ape)
plot(as.phylo(hc), type = "fan")
