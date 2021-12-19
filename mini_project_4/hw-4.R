data=read.csv(file="D:/Fall'21/STATS/mini_project_4/gpa.csv")
head(data)
plot(data)
abline(lm(act~gpa),col="red")
gpa=data$gpa
act=data$act
cor(gpa,act)

library(boot)

corr.npar <- function(x, indices) {
  result <- cor(x$gpa[indices],x$act[indices])
  return(result)
}
(corr.npar.boot <- boot(data, corr.npar, R=999,sim="ordinary", stype="i"))

mean(corr.npar.boot$t)

#print(corr.npar.boot)


boot.ci(corr.npar.boot)



pd = read.csv(file="D:/Fall'21/STATS/mini_project_4/voltage.csv")
boxplot(voltage~location,data=pd, main = "Voltage vs Location")
hist(voltage~location,data=pd)

x1=length(pd$location[pd$location==0])
x1
x2=length(pd$location[pd$location==1])
x2