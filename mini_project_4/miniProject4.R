#Q1

library(boot)
data = read.csv(file="D:/Fall'21/STATS/mini_project_4/gpa.csv")
gpa = data$gpa
act = data$act
plot(gpa,act,xlab = "GPA",ylab = "ACT",main = "GPA vs ACT")
abline(lm(act~gpa),col="blue")
#The line has a positive slope which indicates that there is positive relation between gpa and act.
# By viewing the line in scatter plot and seeing the correlation coeffecient we can say that their linear relationship is not strong.(very weakly related)

cor(gpa,act)
# Function calculates pearson correlation between GPA and ACT
corr.npar <- function(data,indices) {
  result <- cor(data$gpa[indices],data$act[indices])
  return(result)
}
(corr.npar.boot <- boot(data,corr.npar,R=999,
                          sim="ordinary", stype="i"))

mean(corr.npar.boot$t)

boot.ci(corr.npar.boot)
sort(corr.npar.boot$t)[c(25, 975)]

#The strength of the relationship varies in degree based on the value of the correlation coefficient.
#As the CI of correlation coeffecient lies between 0.06 to 0.48, we can say there is a positive correlation between two variables, but it is weak and likely unimportant.


#Q2a
vlt = read.csv(file="D:/Fall'21/STATS/mini_project_4/voltage.csv")
location = vlt$location
voltage = vlt$voltage

par(mfrow=c(1,2))
hist(voltage[location==0],xlab = "Remote Location",ylim = c(0,15))
hist(voltage[location==1],xlab = "Local Location",ylim = c(0,15))

boxplot(voltage~location,main = "Voltage vs Location")
summary((voltage[location==0]))
summary((voltage[location==1]))

par(mfrow=c(1,2))
qqnorm((voltage[location==0]))
qqline((voltage[location==0]))

qqnorm((voltage[location==1]))
qqline((voltage[location==1]))

# From the box plot,histogram and summary of the data we can conclude that at local locations voltage used is less.
# Also the distribution is slightly left skewed as mean<median and there are outliers in the case of remote location.
# From QQ plot it shows approximate normality


#Q2b

# We use 2 independent sample to calculate the CI using large samples technique
# Here null hypothesis H0: mean()-mean(local)=0
# Alternate hypothesis H1: mean(remote)-mean(local)!=0

x1=mean(voltage[location==0])
x2=mean(voltage[location==1])
s1=var(voltage[location==0])
s2=var(voltage[location==1])
ci = (x1-x2) + c(-1,1)*qnorm(0.975)*sqrt((s1/30)+(s2/30))
ci

t.test(voltage[location==0],voltage[location==1], alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)

#By using large samples we get CI same as that of t test so the normality assumption is correct.
#We observe that 0 does not lie in the confidence interval, we can say that the difference in population means of voltages at two locations will not be 0.
#This means that we reject null hypothesis thus manufacturing process can not be established at local locations.

#Q2c

#From part A using plots we observed that remote locations require a higher voltage compared to local locations
#From part B we calculated the CI and observed that since 0 does not lie in CI and the CI is positive, the mean voltage required at remote location is more compared to that at local location
#Thus the manufacturing process can not be done locally as the heavy equipment may require higher voltage.
#Also from qq plot in part A we could observe normality in data which could be observed in part B when constructing CI.

#Q3a

vapor = read.csv("D:/Fall'21/STATS/mini_project_4/VAPOR.csv")
temprature = vapor$temprature
theoretical = vapor$theoretical
experimental = vapor$experimental

par(mfrow=c(1,2))
qqnorm(theoretical)
qqline(theoretical)

qqnorm(experimental)
qqline(experimental)

I = (theoretical - experimental)
summary(I)
hist(I)
boxplot(theoretical,experimental)
#From the qq plot and the five point summary we can observe that the data is approximately normal.
#From the boxplot we observe that the data is very similar and mean and median are nearly equal.
#Data is slightly rightskewed as observe by the plot and the summary.
#Now we have to test the mean difference between theoretical and experimental means.
#Null hypothesis H0: mean(theoretical)-mean(experimental) = 0
#Alternate hypothesis H1: mean(theoretical)-mean(experimental)!=0
#we calculate the CI using the t distribution.

t.test(theoretical,experimental,paired = TRUE,conf.level = 0.95,var.equal = FALSE,alternative="two.sided")

#By observing the CI we can see that 0 lies in between the interval hence we accept the null hypothesis and reject the alternate hypothesis.
#from t.test we can observe the mean difference very close to 0 which is also a strong evidence to accept H0.
