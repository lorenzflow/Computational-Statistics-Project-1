# monte carlo integration
set.seed(123456)
n <- 10^4
X <- rexp(n,2)
Z <- rnorm(n)
Y <- X+10*Z

mc <- mean(X^2*Y)
mc

# monte carlo integration with rao-blackwellisation
set.seed(123456)
n <- 10^4
X <- rexp(n,2)

mc_rb <- mean(X^3)
mc_rb

# compare standard deviations
set.seed(123456)
n <- 10^5
X <- rexp(n,2)
Z <- rnorm(n)
Y <- X+10*Z

mc_sd <- sd(X^2*Y)/sqrt(n)
rb_sd <- sd(X^3)/sqrt(n)