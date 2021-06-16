sigma <- 5
f <- function(x,y) (2/pi)^(3/2) * 1/(sigma*sqrt(3)) * 1/(1+(x-y)^2/3)^2 * exp(-1/(2*sigma^2)*(x+y)^2)

set.seed(123456)

n <- 10^4
alpha <- 0.05
#sample y
x <- rnorm(n)
y <- rnorm(n)
w <- mapply(f, x, y)/(dnorm(x)*dnorm(y))
# compute estimate
is <- mean((x+y)^2*w)

#standard error of is, NOTE ALREADY DIVIDED BY ROOT N.
is_sd <- sd((x+y)^2*w)/sqrt(n)

#asmyptotic confidence interval
CI <- is + is_sd * qnorm(c(alpha/2, 1-alpha/2))

# check tail behaviour
set.seed(123456)
N <- 10^4
sample_size <- rep(N,20)
max_weights <- rep(0, 20)
estimates <- rep(0,20)
std_errors <- rep(0,20)
counter=1
for (n in rep(N,20)){
  x <- rnorm(n)
  y <- rnorm(n)
  w <- mapply(f, x, y)/(dnorm(x)*dnorm(y))
  cat('n=', n ,':', 'est=', mean((x+y)^2*w),' (', sd((x+y)^2*w)/sqrt(n),') ', 'max(w)=', max(w),'\n')
  
  max_weights[counter] <- max(w)
  estimates[counter] <- mean((x+y)^2*w)
  std_errors[counter] <- sd((x+y)^2*w)/sqrt(n)
  counter = counter +1
  
  # numerical integration to find exact value
  Int1 <- function(x) sapply(x, function(a1) integrate(function(y) (a1+y)^2*f(a1,y),-Inf,Inf)$value)
  
  integrate(Int1, -Inf, Inf)
  
  # plot for unstability
  set.seed(123456)
  
  n <- 10^4
  alpha <- 0.05
  #sample y
  x <- rnorm(n)
  y <- rnorm(n)
  w <- mapply(f, x, y)/(dnorm(x)*dnorm(y))
  
  In <- cumsum((x+y)^2*w)/1:n
  
  alln <- round(10^seq(1,4,length.out=1000)) # reduce number of data points
  # exact value for reference line
  exact <- 25
  
  plot(alln, In[alln], xlab='n', ylab=expression(I[n]), ylim=exact+c(-20,20), log='x',main='Estimates for increasing n')
  lines(c(1,n), c(exact,exact))
  
  
  # improvement using cauchy
  set.seed(123456)
  N <- 10^4
  sample_size <- rep(N,20)
  max_weights <- rep(0, 20)
  estimates <- rep(0,20)
  std_errors <- rep(0,20)
  counter=1
  for (n in rep(N,20)){
    x <- rcauchy(n)
    y <- rcauchy(n)
    w <- mapply(f, x, y)/(dcauchy(x)*dcauchy(y))
    cat('n=', n ,':', 'est=', mean((x+y)^2*w),' (', sd((x+y)^2*w)/sqrt(n),') ', 'max(w)=', max(w),'\n')
    
    max_weights[counter] <- max(w)
    estimates[counter] <- mean((x+y)^2*w)
    std_errors[counter] <- sd((x+y)^2*w)/sqrt(n)
    counter = counter +1
  }
  
  
  
  
  
  
  
  
  