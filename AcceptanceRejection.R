#' 目的分布：Beta 分布
#' 提案分布：Uniform 分布

Example2.7 <- function(a=2.7, b=6.3, sample.size=1000){
  # 受容・棄却法による
  # Beta 分布からのサンプリング
  # 目的分布: Beta(2.7, 6.3)  
  # 提案分布: 一様分布
  optim.obj <- optimize(f=function(x){dbeta(x=x,shape1=a,shape2=b)},lower=0, upper=1, maximum=T)
  M <- optim.obj$objective
  print(M)
  print(1/M)
  # rigorous beta density curve
  seq.x <- seq(from=0, to=1, by=0.01)
  plot(x=seq.x, y=dbeta(x=seq.x, a, b), type="l")
  
  x <- NULL
  while(length(x) < sample.size){
    candidates <- runif(n=M*sample.size, min=0, max=1)
    f <- dbeta(x=candidates, shape1=a, shape2=b)
    rand.y <- runif(n=M*sample.size, min=0, max=M)
    passed.x <- (rand.y < f)
    passed.y <- rand.y[passed.x]
    passed.x <- candidates[passed.x]
    x <- c(x, passed.x)
    # scatter plot
    points(x=candidates, y=rand.y, type="p", col="gray", pch=20)
    points(x=passed.x, y=passed.y, col="red", pch=20)
    print(length(passed.x) / length(candidates))
  }  
  # cutoff for the following steps
  x <- x[1:sample.size]
  # histgram
  hist(x=x, breaks=100,freq=F, main="")
  # plot 
  lines(x=seq.x, y=dbeta(x=seq.x, a, b))
  title(main="Sampling from Beta distribution", sub="Acceptance-Rejecetion method")
}

Example2.7()