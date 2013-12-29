#' sampling from Gamma distribution
#' 1. Acceptance-Rejection method
#' 2. Metropolis-Hastings method
#' 
Practice6.4 <- function(a=4.85,b=1, size=5000
                        ,g = function(x){dgamma(x=x, shape=a0, rate=b0)}
                        ,rg = function(n){rgamma(n=n, shape=a0, rate=b0)}
){
  a0 <- floor(a)
  b0 <- a0 / a
  f <- function(x){dgamma(x=x, shape=a, rate=b)}
  x.seq <- seq(0, 15, by=0.01)
  # acceptance-rejection method
  print("acceptance-rejection method")
  optimize.obj <- optimize(f=function(x){f(x)/g(x)}, interval=c(0,100), maximum=T)
  M <- optimize.obj$objective
  candidates <- rg(n=size)
  # U < f/Mg => accept
  random.test <- runif(n=size,min=0, max=1)
  x.ar <-  candidates[random.test * M * g(candidates) < f(candidates)]
  print("acceptance ratio")
  print(length(x.ar) / size)
  print("mean")
  print(mean(x.ar))
  print("variance")
  print(var(x=x.ar))
  print("autocorrelation")
  acf(x=x.ar)
  hist(x=x.ar, breaks=100,freq=F)
  lines(x=x.seq, y=f(x.seq), col="red")
  
  # Metropolis-Hastings method
  print("Metropolis-Hastings method")
  # x.initial => candidate => accept or reject
  x.initial <- rgamma(n=1,shape=a0, rate=b0)
  x.mh <- vector(mode="double",length=(size + 1))
  x.mh[1] <- x.initial
  count.accept <- 0
  for(i in 1:(length(candidates))){
    candidate <- candidates[i]
    ratio <- f(candidate) * g(x.mh[i]) / (f(x.mh[i]) * g(candidate))
    if (ratio > random.test[i]){
      x.mh[i+1] <- candidate
      count.accept <- count.accept + 1
    } else {
      x.mh[i+1] <- x.mh[i]
    }
  }
  x.mh <- x.mh[-1]
  print("acceptance ratio")
  print(count.accept / size)
  print("mean")
  print(mean(x.mh))
  print("variance")
  print(var(x=x.mh))
  print("autocorrelation")
  acf(x=x.mh)
  hist(x=x.mh, breaks=100,freq=F)
  lines(x=x.seq, y=f(x.seq), col="red")
}

Practice6.4(size=100000)
Practice6.4(g=function(x){1},rg=function(n){runif(n=n, min=0, max=15)}, size=100000)
# 一様分布のMHでも, 10ステップ毎くらいにすれば自己相関が無視出来そうである.
# いずれにせよ, 受理率はMHの方が大きくなっていることを確認できる.
