set.seed(123456)
#sample y
n <- 1e4
y <- rnorm(n)
w <- dgamma(y, 2,3)/dnorm(y)
# compute estimate
I1 <- mean((y>2)*w)
I2 <- mean(1/y * w)
#standard error of is, NOTE ALREADY DIVIDED BY ROOT N.
alpha <-0.05

I1_sd <- sd((y>2)*w)/sqrt(n)
I1_CI <- I1 + I1_sd * qnorm(c(alpha/2, 1-alpha/2))

I2_sd <- sd(1/y*w)/sqrt(n)
I2_CI <- I2 + I2_sd * qnorm(c(alpha/2, 1-alpha/2))

cat('n', n ,':', 'est=', I1,' (', I1_sd,') ', 'CI=(', I1_CI,')','\n')
cat('n', n ,':', 'est=', I2,' (', I2_sd,') ', 'CI=(', I2_CI,')','\n')
df <- data.frame(n=rep(10^4,2), estimate = c(I1, I2), std_err = c(I1_sd,I2_sd), Conf_int=c(I1_CI,I2_CI))
xtable(df)

# part b)
# plot the targets to know what better importance sampling
x <- seq(0,4, 0.01)
fx <- 1/x * dgamma(x, 2,3)
plot(x,fx, col='white', main='Importance Sampling Distribution and phi(x)f(x)', ylab='y')
lines(x,fx)
lines(x,3*dexp(x, 3), col='red')

x <- seq(2,5, 0.01)
fx <- dgamma(x, 2,3)
plot(x,fx, col='white', main='Importance Sampling Distribution and phi(x)f(x)', ylab='y')
lines(x,fx)
lines(x,3*dexp(x, 3), col='red')

set.seed(123456)
#sample y
n <- 1e4
y1 <- rexp(n,3)+2
y2 <- y1-2
w1 <- dgamma(y1, 2,3)/dexp(y1-2,3)
w2 <- dgamma(y2, 2,3)/(dexp(y2,3))
# compute estimate
I1_e <- mean((y1>2)*w1)
I2_e <- mean(1/y2 * w2)
#standard error of is, NOTE ALREADY DIVIDED BY ROOT N.
alpha <-0.05

I1_sd_e <- sd((y1>2)*w1)/sqrt(n)
I1_CI_e <- I1_e + I1_sd_e * qnorm(c(alpha/2, 1-alpha/2))

I2_sd_e <- sd(1/y2*w2)/sqrt(n)
I2_CI_e <- I2_e + I2_sd_e * qnorm(c(alpha/2, 1-alpha/2))

cat('n', n ,':', 'est=', I1_e,' (', I1_sd_e,') ', 'CI=(', I1_CI_e,')','\n')
cat('n', n ,':', 'est=', I2_e,' (', I2_sd_e,') ', 'CI=(', I2_CI_e,')','\n')

