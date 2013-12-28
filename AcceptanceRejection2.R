#' 目的分布：Beta 分布
#' 提案分布：Beta 分布
#' 

rm(list=ls())

Example2.8 <- function(a=2.7, b=6.3, a0=2, b0=6, sample.size=10000){
  # let g be proposal density, and f be target density
  f <- function(x){dbeta(x=x, shape1=a, shape2=b)}
  g <- function(x){dbeta(x=x, shape1=a0, shape2=b0)}
  f.over.g <- function(x){
    f(x) / g(x)
  }
  # find maximum point of ratio target density over its proposal
  optimize.obj <- optimize(interval=c(0,1), maximum=T, f=f.over.g)
  M <- optimize.obj$objective
  print("maximum point of f over g")
  print(M)
  # rigorous density plot for scatter plot
  seq.x <- seq(from=0, to=1, by=0.01)
  plot(x=c(0,1), y=c(0,5), type="n")
  lines(x=seq.x, y=f(x=seq.x), col="red")
  lines(x=seq.x, y=M*g(x=seq.x), col="gray")
  
  x <- NULL
  while(length(x) < sample.size){
    candidates <- rbeta(n=sample.size * M, shape1=a0, shape2=b0)
    random.test <- runif(n=sample.size * M, min=0, max=M)
    # fの曲線より下の部分で把握したいので, 
    # U()*g(x)*M < f(x) という判定条件で考える.
    random.test <- random.test * g(candidates)
    idx.accepted <- which(x=(random.test < f(candidates)))
    x <- c(x, candidates[idx.accepted])
    points(x=candidates, y=random.test, cex=0.1, col="gray", pch=20)
    points(x=candidates[idx.accepted], cex=0.1, y=random.test[idx.accepted], col="red", pch=20)
    print("acceptance rate")
    print(length(idx.accepted) / length(candidates))
  }
  x <- x[1:sample.size]
  # histgram
  hist(x=x, breaks=100,freq=F, main="")
  # plot 
  lines(x=seq.x, y=dbeta(x=seq.x, a, b))
  title(main="Sampling from Beta distribution", sub="Acceptance-Rejecetion method")
}

Example2.8(sample.size=10000)