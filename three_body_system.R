rm(list=ls())
source("updater.R")
source("updater_unittest.R")
source("statistics.R")
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

iteration.number <- 30000
burnin.num <- 100
theta <- 0.8
x <- simulate.three.body.system(iteration.number=iteration.number,parameter=theta, selector.type="random")
out.x <- seq(from=burnin.num, to=iteration.number - burnin.num, by=(ncol(x) * burnin.num))
out.y <- stat.fun(
                quantity.fun=function(x){x[1]}
              , samples=x
              , out.steps=out.x
              , burnin=burnin.num)
mcs <- out.x / ncol(x)
plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
analytic.y <- 0
lines(x=mcs, y=rep(x=analytic.y,length(mcs)), col="red")
title(main="expectation of x[1]")

out.y <- stat.fun(
                quantity.fun=function(x){x[1]*x[2]}
              , samples=x
              , out.steps=out.x
              , burnin=burnin.num)
plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
title(main="expectation of x[1]*x[2]")
analytic.y <- ((2*exp(3*theta) - 2*exp(-theta)) / (2*exp(3*theta) + 6*exp(-theta)))
lines(x=mcs, y=rep(x=analytic.y, length(mcs)), col="red")
# # burn in
# x <- out.x[-(1:(burnin.num+1)),]
# matplot(x=seq(1:nrow(x))
#         , y=sapply(X=seq(1:3),FUN=function(i){cumsum(x=x[,i]) / c(1:nrow(x))})
#         ,pch=c(20,20,20))
