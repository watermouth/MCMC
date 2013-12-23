rm(list=ls())
source("updater.R")
source("updater_unittest.R")
get.prob <- function(x,para){
  theta <- para[1]
  # 2体相互作用
  inner_vec <- theta * combn(x=x,m=2,FUN=function(y){y[1] * y[2]})
  inner_term <- sum(inner_vec)
  exp(inner_term)
}

initialize <- function(){
  c(1,1,1)
}

simulate.three.body.system <- function(iteration.number=100,parameter=0.2, selector.type="sequential"){
  # 初期化
  x <- initialize()
  # Updaterの取得
  selector <- get.selector(degreeoffreedom=length(x), type=selector.type)
  updater  <- get.updater(updater.type="Metropolis"
                          ,fun.prob=function(x){
                            get.prob(x=x,para=parameter)
                          }
  )
  # LOOP
  v <- matrix(nrow=iteration.number + 1, ncol=length(x), byrow=T)
  v[1,] <- x
  for (n in seq(1:iteration.number)){
    v[n+1,] <- updater(v[n,], selector())
  }
  v
}

iteration.number <- 100
x <- simulate.three.body.system(iteration.number=iteration.number)
matplot(x=seq(1:(iteration.number+1))
        , y=sapply(X=seq(1:3),FUN=function(i){cumsum(x=x[,i]) / cumsum(1:(iteration.number+1))})
        ,pch=c(20,20,20))
