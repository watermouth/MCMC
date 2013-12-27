#' Example 6.1
#' Beta(2.7, 6.3)
#' 
Example6.1 <- function(shape1=2.7, shape2=6.3, step.size=5000){
  # MCMC(Metropolis-Hastings)
  x <- vector(mode="double", length=step.size)
  # sampling from proposal distribution(uniform)
  x[1] <- runif(n=1)
  for (i in 2:step.size){
    y <- runif(1)
    ratio <- 
      dbeta(x=y, shape1=shape1, shape2=shape2) / 
      dbeta(x=x[i-1], shape1=shape1, shape2=shape2)
    if (ratio > 1){
      x[i] <- y
      next
    }
    if (runif(1) < ratio){
      x[i] <- y
    } else {
      x[i] <- x[i-1]
    }
  }
  print("expectation")
  print(shape1 / (shape1 + shape2))
  print(mean(x))
  print("variance")
  print(shape1*shape2/((shape1+shape2)^2 * (shape1 + shape2 + 1)))
  print(var(x))
  
  # plot 
  plot(x=(4500:step.size), y=x[4500:step.size], type="l")
  title("state transition")
  
  # histgram
  hist(x=x, breaks=100,freq=F,main="")
  # rigorous plot
  x <- seq(from=0.0, to=1.0, by=0.01)
  y <- dbeta(x=x, shape1=shape1, shape2=shape2)
  lines(x=x, y=y)
  string.title <- paste("MH Beta(", shape1, ",", shape2, ")", sep="")
  title(main=string.title)
  
  # regorous sampling
  x <- rbeta(n=step.size, shape1=shape1, shape2=shape2)
  hist(x=x, breaks=100,freq=F,main="")
  # rigorous plot
  x <- seq(from=0.0, to=1.0, by=0.01)
  y <- dbeta(x=x, shape1=shape1, shape2=shape2)
  lines(x=x, y=y)
  string.title <- paste("Direct Generation(", shape1, ",", shape2, ")", sep="")
  title(main=string.title)
  
}

Example6.1()
