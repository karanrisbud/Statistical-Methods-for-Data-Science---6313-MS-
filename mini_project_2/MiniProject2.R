
1-a
#Create a bar graph of the variable Maine, which identies whether a runner is
#from Maine or from somewhere else (stated using Maine and Away). You can
#use barplot function for this. What can we conclude from the plot? Back up
#your conclusions with relevant summary statistics.

roadrace = read.csv(file="/Users/shubh/Downloads/STATS/mini_project_2/roadrace.csv")
height = c(sum(roadrace$Maine=='Maine'),sum(roadrace$Maine == 'Away'))
barplot(height,names.arg=c('Maine','Away'))

# There are more runners who are from Maine
#as compared to the number of runners away from Maine. 

1-b

away = (roadrace$Time..minutes[which(roadrace$Maine=='Away')])
Maine = (roadrace$Time..minutes[which(roadrace$Maine=='Maine')])
par(mfrow=c(1,2))
hist(away,xlim=c(0,180),ylim = c(0,2000),xlab="Time in mins")
hist(Maine,xlim=c(0,180),ylim = c(0,2000),xlab="Time in mins")

summary(Maine)
IQR(Maine)
range(Maine)
sd(Maine)

summary(away)
IQR(away)
range(away)
sd(away)

# Based on the summary of both Maine and Away. We can observe that
# both the data have approximately same mean and median thus they are
# approximately normally distributed. Since deviation of away is more
# the dispersion of data is more as observed in the histogram.

1-c

boxplot(ylab ="Time in Mins",away,Maine,names=c("away","Maine"),range = 0)

1-d

male = (roadrace$Age[which(roadrace$Sex=='M')])
female = (roadrace$Age[which(roadrace$Sex=='F')])


male = as.numeric(male)
female = as.numeric(female)

boxplot(ylab ="Age",male,female,names=c("Male","Female"),range = 1.5)

summary(male)
IQR(male)
sd(male)
range(male)

summary(female)
IQR(female)
sd(female)
range(female)

# Median age of male runners is greater than that of female.
# Female runners have a lot of outliers.
# Age for female runners is slightly right skewed.

Q2

motorcycle = read.csv(file = "/Users/shubh/Downloads/STATS/mini_project_2/motorcycle.csv")
mc = motorcycle$Fatal.Motorcycle.Accidents
boxplot(mc,ylab ="Number of Accidents",xlab="County")

summary(mc)
IQR(mc)
range(mc)
sd(mc)

right.whisker = min(max(mc),quantile(mc,0.75)+IQR(mc)*1.5)
county.outliers = motorcycle$County[which(mc>right.whisker)]
county.outliers

#Insufficient Data to determine why the county has higher accidents.