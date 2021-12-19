#Q1.b.1
LifeTime.T = max(rexp(n=1,rate=0.1),rexp(n=1,rate=0.1))
LifeTime.T

#Q1.b.2
LifeTime.vector.T = replicate(n=10000,
                            max(rexp(n=1,rate=0.1),
                                rexp(n=1,rate=0.1)))
head(LifeTime.vector.T)

#Q1.b.3
#for Histogram
hist(LifeTime.vector.T,probability = T,
     xlab = "Expected Satellite Lifetime")

#For superimposing the curve
hist(LifeTime.vector.T,probability = T,
     xlab = "Expected Satellite Lifetime")
PDF.F.T = function(a) 0.2*exp(-0.1*a) - 0.2*exp(-0.2*a)
curve(PDF.F.T,add = T)

#Q1.b.4
mean(LifeTime.vector.T)

#1.b.5
Prob = sum(LifeTime.vector.T>15)/length(LifeTime.vector.T)
Prob
hist(LifeTime.vector.T,probability = T,
     xlab = "Expected Satellite Lifetime")

#1.b.6
for(i in 1:4)
{
  LifeTime.vector.T = replicate(n=10000,
                                max(rexp(n=1,rate=0.1),
                                    rexp(n=1,rate=0.1)))
  mean = mean(LifeTime.vector.T)
  Prob = sum(LifeTime.vector.T>15)/length(LifeTime.vector.T)
  print(paste("E(T) = ",mean, "Probability of Satellite lasting more than 15 years = ",Prob))

}

#Q1.c

#For n = 1000
for(i in 1:5)
{
  LifeTime.vector.T = replicate(n=1000,
                                max(rexp(n=1,rate=0.1),
                                    rexp(n=1,rate=0.1)))
  mean = mean(LifeTime.vector.T)
  Prob = sum(LifeTime.vector.T>15)/length(LifeTime.vector.T)
  print(paste("E(T) = ",mean, "Probability of Satellite lasting more than 15 years = ",Prob,"where N = ",1000))
  
}

#For n = 100000
for(i in 1:5)
{
  LifeTime.vector.T = replicate(n=100000,
                                max(rexp(n=1,rate=0.1),
                                    rexp(n=1,rate=0.1)))
  mean = mean(LifeTime.vector.T)
  Prob = sum(LifeTime.vector.T>15)/length(LifeTime.vector.T)
  print(paste("E(T) = ",mean, "Probability of Satellite lasting more than 15 years = ",Prob,"where N = ",100000))
  
}


#Q2 

#The probability that a point lies inside a circle is SA(circle)/SA(Square)
#where SA -> Surface Area. The probability turns out to be pie/4
#Thus pie = 4 * Probability.
#Equation of a Circle is given by (x-a)^2 + (y-b)^2 = r^2
#where a,b is center of circle and r is radius.
#For the point to lie inside the circle we get (x-a)^2 + (y-b)^2 <= r^2
#We generate 10000 uniform sample points between 0,1 for x & y coordinates.
#we take center = (0.5,0.5) and r = 1

#number of iterations
n=10000
#Uniformly generated n random variable from 0,1
x.coordinate = runif(n,min=0,max=1)
y.coordinate = runif(n,min=0,max=1)
points.inside.circle = length(which((x.coordinate-0.5)^2 + (y.coordinate-0.5)^2 <= 0.5^2))
#Probability is points inside circle/ total no. of points
probability = points.inside.circle/n
result = 4 * probability
result

