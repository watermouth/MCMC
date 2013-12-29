BivariateNormal <- function(init.x=0, n, rho=0){
  x <- array(dim=c(n+1,2))
  x[1,1] <- init.x
  sd <- 1-rho*rho
  for(i in 1:n){
    x[i,2] <- rnorm(n=1, mean=(rho*x[i,1]), sd=sd)
    x[i+1, 1] <- rnorm(n=1, mean=(rho*x[i,2]), sd=sd)
  }
  x <- array(data=x[-c(1, length(x))],dim=c(n,2))
}

z <- BivariateNormal(init.x=0, n=10000, rho=0.5)
plot(x=z[,1], y=z[,2], pch=4, type="l", lty=5)
hist(x=z[,1], breaks=100, freq=F)
hist(x=z[,2], breaks=100, freq=F)