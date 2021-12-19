#Q1a

dataset = read.csv("D:/Fall'21/STATS/mini_project_5/bodytemp-heartrate.csv")
male = dataset[dataset$gender == 1,]
female = dataset[dataset$gender == 2,]

mean(male$body_temperature)
mean(female$body_temperature)

par(mfrow=c(1,2))
boxplot(male$body_temperature,ylim=c(96,101),xlab="Male",ylab="Body Temperature")
boxplot(female$body_temperature,ylim=c(96,101),xlab="Female",ylab="Body Temperature")

par(mfrow=c(1,2))
qqnorm(male$body_temperature)
qqline(male$body_temperature)

qqnorm(female$body_temperature)
qqline(female$body_temperature)

summary(male$body_temperature)
summary(female$body_temperature)

# From visualizing the QQ plot , we can assume male and female body distributions to be approximately normal.
# Based on summary and box plot we can not assume equal variances between the two distribution. hence we will use t test with unequal variance and treat them as independent samples.
# We perform hypothesis testing to determine difference in mean body temperature between male and female.
# Null hypothesis h0 : mean(male.body_temp) - mean(female.body_temp)=0
# Alternate hypothesis h1: mean(male.body_temp) - mean(female.body_temp)!=0

t.test(male$body_temperature,female$body_temperature,alternative = "two.sided",var.equal = FALSE)

# Based on the t test we obtain the p value as 0.02394 which is much less than 0.05
# Also the CI does not contain 0. 
# Based on these 2 observations we reject the null hypothesis.
# Thus our conclusion is that there is differece between mean body temperature of male and female.

#Q1b

male_heartrate = dataset[dataset$gender == 1,]
female_heartrate = dataset[dataset$gender == 2,]

mean(male_heartrate$heart_rate)
mean(female_heartrate$heart_rate)

par(mfrow=c(1,2))
boxplot(male_heartrate$heart_rate,xlab="Male",ylab="Heart_Rate")
boxplot(female_heartrate$heart_rate,xlab="Female",ylab="Heart_Rate")

par(mfrow=c(1,2))
qqnorm(male_heartrate$heart_rate)
qqline(male_heartrate$heart_rate)

qqnorm(female_heartrate$heart_rate)
qqline(female_heartrate$heart_rate)

summary(male_heartrate$heart_rate)
summary(female_heartrate$heart_rate)


# From visualizing the QQ plot , we can assume male and female body distributions to be approximately normal.
# Based on summary and box plot we can not assume equal variances between the two distribution. hence we will use t test with unequal variance and treat the two samples as independent sample.
# We perform hypothesis testing to determine difference in mean Heart Rates  between male and female.
# Null hypothesis h0 : mean(male.heart_rate) - mean(female.heart_rate)=0
# Alternate hypothesis h1: mean(male.heart_rate) - mean(female.heart_rate)!=0


t.test(male_heartrate$heart_rate,female_heartrate$heart_rate,alternative = "two.sided",var.equal = FALSE)

# Based on the t test we obtain the p value as 0.5287 which is much higher than 0.05
# Also the CI does  contain 0. 
# Based on these 2 observations we accept the null hypothesis and reject alternate hypothesis as there is insufficient evidence.
# Thus our conclusion is that there is no difference between mean body heart_rate of male and female.



#Q1c


plot(dataset$body_temperature,dataset$heart_rate)
abline(lm(dataset$heart_rate~dataset$body_temperature),col="blue")
cor(dataset$body_temperature,dataset$heart_rate)
lm(dataset$body_temperature~dataset$heart_rate)

par(mfrow=c(1,1))
plot(male$body_temperature,male$heart_rate)
abline(lm(male$heart_rate~male$body_temperature),col="blue")
cor(male$body_temperature,male$heart_rate)
lm(male$body_temperature~male$heart_rate)

plot(female$body_temperature,female$heart_rate)
abline(lm(female$heart_rate~female$body_temperature),col="blue")
cor(female$body_temperature,female$heart_rate)
lm(female$body_temperature~female$heart_rate)

#Based on plot which does not consider gender, the observed line has a positive slope and the value of corelation coeffecient is 0.253
#This indicates that there is "weak positive linear relationship" between body temperature and heart rate.

#based on the 2 plots that considers gender. We observe a line with positive slope and positive value of correlation that indicates positive linear relationship.
#The value of correlation when gender is male is 0.195 and that of female is 0.2869.
#We can say that female's body temperature and heart rate are more strongly connected than that of male.
#Also significant difference in correlation values suggest that it does depend on gender.


#Q2a

z.proportion = function(x,lambda){
  SE = sd(x)/sqrt(length(x))
  ci = (mean(x) + c(-1,1)*qnorm(0.975)*SE)
  population.mean = 1/lambda
  if(ci[2]>population.mean & ci[1]< population.mean)
    return(1)
  else
    return(0)
  
}

boot.proportion = function(x,n,lambda){
  #calculated 999 means
  b = replicate(1000,mean(rexp(n,1/mean(x))))
  ci = sort(b)[c(25,975)]
  population.mean = 1/lambda
  if(ci[2]>population.mean & ci[1]<population.mean)
    return(1)
  else
    return(0)
}

mc.sim = function(n,lambda){
  x.sample = rexp(n,lambda)
  z = z.proportion(x.sample,lambda)
  p = boot.proportion(x.sample,n,lambda)
  return(c(z,p))
  
  
}

monte.Carlo.sim = function(n, lambda){
  #MC Trials
  mc = replicate(5000,mc.sim(n, lambda))
  #output results
  return(c(sum(mc[1,])/length(mc[1,]),sum(mc[2,])/length(mc[2,]) 
           ))
}


print(monte.Carlo.sim(5, 0.01))

# Here We take values of n and lambda and use monte carlo simulations to construct 2 CI (z interval and percentile parametric bootstrapping)
# We check each time whether both the CI contains the true value of population mean.
# Based on that we calculate the coverage probabilities.
# We got the probabilities as:
# z-interval = 
# p-interval =

#Q2b

#make 2 empty matrices
z_matrix = matrix(nrow=4, ncol=4)
p_matrix = matrix(nrow=4, ncol=4)
#all values for lambda and n to test for
lambda.vector = c(0.01, 0.1, 1, 10)
n.vector = c(5, 10, 30, 100)
#for each value n and lambda, run 5000 MC trials and estimate the coverage

row = 1
for(lambda in lambda.vector){
  col = 1
  for(n in n.vector){
    proportion.vector = monte.Carlo.sim(n, lambda)
    print(row)
    z_matrix[row, col] = proportion.vector[1]
    p_matrix[row, col] = proportion.vector[2]
    col = col + 1
  }
  row = row + 1
}

z_matrix
p_matrix
#output both matrices to a csv for ease of access for reporting
write.csv(data.frame(z_matrix), "z_out.csv")
write.csv(data.frame(p_matrix), "p_out.csv")

# We repeat the obove process for the remaining observations
# we obtain the following values represented in table as shown:

#Q2c

par(mfrow=c(1,2))
for (i in c(1,2,3,4)) {
  plot(c(5,10,30,100),z_matrix[i,],main = paste("lambda value :", lambda.vector[i]),xlab = 'n',ylab='proportions',type='b',col='red',xlim = c(1,100),ylim = c(0.7,1))
  lines(c(5,10,30,100),p_matrix[i,],col='blue',type='b')
}

for (i in c(1,2,3,4)) {
  plot(c(0.01,0.1,1,10),z_matrix[,i],main = paste("n value :", n.vector[i]),xlab = 'Lambda',ylab='proportions',type='b',col='red',xlim = c(0.01,10),ylim = c(0.7,1))
  lines(c(0.01,0.1,1,10),p_matrix[,i],col='blue',type='b')
}

# We drew 2 different types of graphs.
# Initially we kept lambda constant and tried to plot probabilities based on different values of n
# Secondly we kept n constant and plotted the probabilities for different values of lambda
# Based on type 1 graphs we can say that there is no significance difference based on different values of lambda.
# Hence we can say that coverage probabilities do not depend on the values of lambda.
# For large sample interval we can observe that the probability reaches close to 0.95 as n reaches 100. So n should be 100 for large sample interval.
# For percentile bootstrap interval we can observe that the probability reaches close to 0.95 as n is 30. So n should be 30 for percentile bootstrap interval.
# Based on the graphs and the results of coverage probabilities for different values of n we can say that percentile bootstrap performs better.
# I would recommend percentile bootstrap as it performs good for small values of n. But if n value is large we can observe from the graph that lines are nearly same. So when n is large, large sample interval provides similar results as percentile bootstrap with less computation.
# Hence when n is large it would be better to use large sample interval.

#Q2d

# The conclusion does not depend on any specific value of lambda
# This is observed from the type 1 graph where when we keep n constant and try to find probabilities based on different values of lambda, there is no significant change observed.

