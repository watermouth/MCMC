#' Introducing Monte Carlo Methods with R
#' 
problem6.1 <- function(x0=0, rho = 0.9, eps.mean=0, eps.sd=1, step.size=100000, seed=1000) {
  set.seed(seed)
  # x[1] = rho * x0 + eps1
  # x[2] = rho *(x[1]) + eps2 = rho^2 * x0 + rho * eps1 + eps2
  # x[3] = rho *(x[2]) + eps3 = rho^3 * x0 + rho^2 * eps1 + rho * eps2 + eps3
  x <- vector(mode="double", length=(step.size+1))
  x[1] <- x0
  temp <- rnorm(n=step.size, mean=eps.mean, sd=eps.sd)
  for(i in seq(1, step.size, by=1)){
    x[i+1] <- rho * x[i] + temp[i] 
  }
  x <- x[-1]
  
  # histgram
  hist(x=x, breaks=100, freq=F)
  # stationary process => x[t] follows normal distribution
  x.points <- seq(from=min(x), to=max(x), by=0.5)
  lines(x=x.points, y=dnorm(x=x.points,mean=0, sd=sqrt(1/(1-rho*rho))), col="red")
}