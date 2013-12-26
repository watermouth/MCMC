rm(list=ls())
source("updater.R")
source("updater_unittest.R")
source("statistics.R")

simulate <- function(initial.state, fun.prob, iteration.number=100, updater.type="MetropolisRevised", selector.type="sequential"){
  # 初期化
  x <- initial.state 
  # Updaterの取得
  selector <- get.selector(degreeoffreedom=length(x), type=selector.type)
  updater  <- get.updater(updater.type=updater.type, fun.prob=fun.prob)
  # LOOP
  v <- matrix(nrow=iteration.number + 1, ncol=length(x), byrow=T)
  v[1,] <- x
  for (n in seq(1:iteration.number)){
    v[n+1,] <- updater(v[n,], selector())
  }
  v
}

# initial.state のサイズが大きいと発散して計算できない...
example <- function(initial.state=c(1,1,1)){
  source("global_two_body_coupling_system.R")
  iteration.number <- 30000
  burnin.num <- 1000
  theta <- 0.4
  x <- simulate(initial.state=initial.state
                , fun.prob=function(x){get.prob(x=x, para=theta)}
                , updater.type="Metropolis"
                , iteration.number=iteration.number
                , selector.type="random"
  )
  out.x <- seq(from=burnin.num, to=iteration.number - burnin.num, by=(ncol(x) * burnin.num))
  prepared.stat.fun <- get.stat.fun(samples=x, out.steps=out.x, burnin=burnin.num)
  
  out.y <- prepared.stat.fun(function(x){x[1]})
  mcs <- out.x / ncol(x)
  plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
  analytic.y <- 0
  lines(x=mcs, y=rep(x=analytic.y,length(mcs)), col="red")
  title(main="expectation of x[1]")
  
  out.y <- prepared.stat.fun(function(x){x[1]*x[2]})
  plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
  title(main="expectation of x[1]*x[2]")
  analytic.y <- ((2*exp(3*theta) - 2*exp(-theta)) / (2*exp(3*theta) + 6*exp(-theta)))
  lines(x=mcs, y=rep(x=analytic.y, length(mcs)), col="red")
}

example2 <- function(initial.state=c(1,1,1)){
  source("global_two_body_coupling_system.R")
  iteration.number <- 30000
  burnin.num <- 1000
  theta <- 0.4
  x <- simulate(initial.state=initial.state
                , fun.prob=function(x, index, x.candidate){
                  get.prob.ratio(x=x, index=index, x.candidate=x.candidate, para=theta)
                }
                , updater.type="MetropolisRevised"
                , iteration.number=iteration.number
                , selector.type="random"
  )
  out.x <- seq(from=burnin.num, to=iteration.number - burnin.num, by=(ncol(x) * burnin.num))
  prepared.stat.fun <- get.stat.fun(samples=x, out.steps=out.x, burnin=burnin.num)
  
  out.y <- prepared.stat.fun(function(x){x[1]})
  mcs <- out.x / ncol(x)
  plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
  analytic.y <- 0
  lines(x=mcs, y=rep(x=analytic.y,length(mcs)), col="red")
  title(main="expectation of x[1]")
  
  out.y <- prepared.stat.fun(function(x){x[1]*x[2]})
  plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
  title(main="expectation of x[1]*x[2]")
  analytic.y <- ((2*exp(3*theta) - 2*exp(-theta)) / (2*exp(3*theta) + 6*exp(-theta)))
  lines(x=mcs, y=rep(x=analytic.y, length(mcs)), col="red")
}

# example
initial.state <- rep(1,3)
set.seed(seed=1000)
example(initial.state=initial.state)
set.seed(seed=1000)
example2(initial.state=initial.state)
