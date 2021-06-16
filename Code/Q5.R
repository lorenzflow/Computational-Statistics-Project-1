# part a)
phi <- function(x) max(x^2)-min(x^2)

genMCsamp <- function(k, nrep=1e4, r=function(n) rnorm(n)){
  I <- replicate(nrep, {
    x <- r(k)
    res=phi(x)
  })
  sterr <- sd(I)/sqrt(nrep);
  res <- c(That=mean(I), sterr=sterr)
  res
}

set.seed(123456)
k <- c(2^c(0:5))
Outputs <- sapply(k, genMCsamp)
Outputs

#part b)

#f evaluating the function to be integrated
f <- function(x)(max(x^2)-min(x^2))*prod(dnorm(x))

num_int <- function(k, nrep=10^4){
  
  n <- floor(nrep^(1/k))
  
  points <- seq(-10,10,length.out=n+1)
  midpoints <- points[1:n]+(points[2]-points[1])/2
  #list_mid <- replicate(k,midpoints)
  
  # expand.grid unfortunately does not work with list_mid hence need to adjust this manually
  if(k==1) grid <- midpoints
  if(k==2) grid <- expand.grid(midpoints,midpoints)
  if(k==4) grid <- expand.grid(midpoints,midpoints,midpoints, midpoints)
  if(k==8) grid <- expand.grid(midpoints,midpoints,midpoints, midpoints, midpoints,midpoints,midpoints, midpoints)
  
  if(k==1) sapply(midpoints, f)
  else y <- apply(grid, 1, f)
  
  w <- (points[2]-points[1])^k
  
  return(sum(y*w))
}

# computating the estimates
k_vec <- 2^(0:3)
sapply(k_vec, num_int)

