1-a
# First we set the population parameter theta
# Now we generate samples from the population and estimate theta hat
# Mean squared error is the square of the difference between theta and theta hat

1-b

estimator = function(n,theta)
{
  generator = runif(n,min=0,max=theta)
  mle = max(generator)
  mme = 2 * mean(generator)
  return (c(mle,mme))
}

simulations = function(n,theta)
{
  theta.hat = replicate(1000,estimator(n,theta))
  mse = (theta.hat - theta)^2
  theta.hat1.mse= mean(mse[1,])
  theta.hat2.mse = mean(mse[2,])
  return (c(theta.hat1.mse,theta.hat2.mse))
  
}
n=1
theta = 1
estimators = simulations(n,theta)
estimators

1-c

n.values = c(1,2,3,5,10,30)
theta.values = c(1,5,50,100)
k=0
mse.theta1 = c(0,0,0,0)
mse.theta2 = c(0,0,0,0)
for(i in n.values)
{
  k=1
  for(j in theta.values)
  {
    estimators = simulations(i,j)
    mse.theta1[k] = estimators[1]
    mse.theta2[k] = estimators[2]
    k=k+1
  }
  
  plot(theta.values,mse.theta1,xlab = 'theta',ylab = 'MSE',main=bquote(paste("N = ", .(i))),
      type = 'b',col='blue')
  lines(theta.values,mse.theta2,col='red',type = 'b')
  legend("topleft",legend=c("Theta1","Theta2"),col=c('blue','red'),cex = 0.5,lty=c(2,2),merge = TRUE)
}



#theta

n.values = c(1,2,3,5,10,30)
theta.values = c(1,5,50,100)
k=0
mse.theta1 = c(0,0,0,0,0,0)
mse.theta2 = c(0,0,0,0,0,0)
for(i in theta.values)
{
  k=1
  for(j in n.values)
  {
    estimators = simulations(j,i)
    mse.theta1[k] = estimators[1]
    mse.theta2[k] = estimators[2]
    k=k+1
  }
  
  plot(n.values,mse.theta1,xlab = 'N',ylab = 'MSE',main=bquote(paste("Theta = ", .(i))),
       type = 'b',col='blue')
  lines(n.values,mse.theta2,col='red',type="b")
  legend("topright",legend=c("Theta1","Theta2"),col=c('blue','red'),cex = 0.5,lty=c(2,2),merge = TRUE)
}

1-d

# We can observe that as the value of samples(n) increases the
# mean squared error decreases. But as sample size increases we can
# observe from plot that MSE for MLE is less than MME.
# Thus as n increases MLE becomes better so best estimator depends on n.
# We get same value for MSE for n=1 and different values of theta thus it does not depend on theta
# So we can say MLE is better

2-c
x = c(21.42,14.65,50.42,28.78,11.23)
neg.loglik.fn <- function(par,dat)
{
  result = length(dat)*log(par)-(par+1)*sum(log(dat))
  return(-result)
}

mle = optim(par = 0.5,dat=x, fn=neg.loglik.fn, method="L-BFGS-B", hessian = TRUE, lower = 0 )
mle

2-d

## Standard error calculation
SE= sqrt( diag(solve(mle$hessian)))
SE
## alpha 0.05 because 95% confidence interval
alpha <- 0.05
# confidence interval is mean +/- z-score*standard error 
CI = mle$par + c(-1, 1)*qnorm(1-(alpha/2))*SE
CI
