# inversion for question 1
F <- function(x) (1-exp(-2*x))/(1-exp(-6))

rf <- function(n){
  # generate uniform on [0,1]
  U <- runif(n)
  # inverions and return X~f.
  return(-1/2*log(1-(1-exp(-6))*U)) 
}

set.seed(12345)
# compute empirical cdf
empirical <- ecdf(rf(1e3))
x <- seq(0,3,0.01)
plot(empirical, pch = 20, main='Empirical CDF and Thoretical CDF')
# line for true cdf
lines(x, F(x) , col='red')

# part b)
set.seed(123456)
# monte carlo integration
n <- 1e4
x <- rf(n)
mc <- mean(cos(x))
mc

#numerical integration
df <- function(x) 1/(1-exp(-6)) *2*exp(-2*x)
integrate(function(x) cos(x)*df(x), 0, 3)

# part c)

# 95% confidence interval
mc_sd <- sd(cos(x))/sqrt(n)
CI <- mc + mc_sd* qnorm(c(0.025, 0.975))
CI
# computations for length of the confidence interval
l <- 2*mc_sd*qnorm(0.975)
l
mc_sd
qnorm(0.975)
log10(10^(-6)/l)