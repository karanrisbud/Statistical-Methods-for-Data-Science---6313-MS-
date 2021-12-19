#Q1 
p_cancer = read.csv("D:/Fall'21/STATS/mini_project_6/prostate_cancer.csv")
head(p_cancer)
par(mfrows=c(1,2))
boxplot(p_cancer$psa)
qqnorm(p_cancer$psa)

library(GGally)
install.packages(GGally)


1-pnorm(2.93)
qnorm(1-0.01)
1-pnorm(2.326)
2*(1-pt(2.6,2))
qt(1-0.1/2,2)

2*(1-pt(2.91,2))



2*(pt(-2.59,2))

x <- c(30,50,70)
alpha <- 0.1
mu0 <- 80
t.test(x, alternative="greater",mu=mu0,conf.level=(1-alpha))

1-pnorm(0.87)

qnorm(1-0.04)
1-pnorm(1.75)

1-pnorm(2.5)
qnorm(1-0.05)
1-pnorm(1.64)
2*pnorm(-1.082)
qnorm(1-0.02/2)
2*pnorm(-2.326)


x <- c(56, 47, 49, 37, 38, 60, 50, 43, 43, 59, 50, 56, 54, 58)
y <- c(53, 21, 32, 49, 45, 38, 44, 33, 32, 43, 53, 46, 36, 48, 39, 35, 37, 36, 39, 45)
alpha=0.05
t.test(x,y,alternative="greater",conf.level=(1-alpha),var.equal = T)

t.test(x,y,alternative="greater",conf.level=(1-alpha),var.equal = F)



qnorm(1-0.05/2)
2*pnorm(-1.199)
2*(pnorm(-6.199))


1-pnorm(1.8000)
