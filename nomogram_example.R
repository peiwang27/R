# 逻辑回归列线图制作
library(Hmisc)
library(grid)
library(lattice)
library(Formula)
library(ggplot2) 
library(rms)

n <- 1000
set.seed(17)
age <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol <- rnorm(n, 200, 25)
sex <- factor(sample(c('female','male'), n,TRUE))
# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-50) +
  (log(cholesterol -10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)


ddist <- datadist(age,blood.pressure, cholesterol, sex)
options(datadist='ddist')
f <- lrm(y ~ age+ blood.pressure+ cholesterol + sex)
nom <- nomogram(f, fun=plogis,
                fun.at=c(.001,.01, .05,
                         seq(.1,.9, by=.1), .95, .99, .999),
                lp=F, funlabel="最后的发病风险")
plot(nom)
