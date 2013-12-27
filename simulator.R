rm(list=ls())
source("updater.R")
source("updater_unittest.R")
source("statistics.R")

#' サイズがメモリ上に乗るくらいに小さい場合はこれを使用できる.
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

simulate.sequentially <- function(initial.state
                                  , fun.prob
                                  , extractor
                                  , iteration.number=100
                                  , burnin.number=10
                                  , iteration.steps=seq(from=burnin.number, to=iteration.number,by=burnin.number)
                                  , updater.type="MetropolisRevised"
                                  , selector.type="sequential"){
  # 初期化
  x <- initial.state 
  # Updaterの取得
  selector <- get.selector(degreeoffreedom=length(x), type=selector.type)
  updater  <- get.updater(updater.type=updater.type, fun.prob=fun.prob)
  # LOOP
  s <- x
  v <- 0
  for (n in seq(1:burnin.number)){
    s <- updater(s, selector())
  }
  out <- vector(mode="double", length=length(iteration.steps))
  out.idx <- 1
  n <- 1
  for (iteration.step in iteration.steps){
    while(n < iteration.step){
      s <- updater(s, selector())
      v <- v + extractor(s)
      n <- n+1
    }
    out[out.idx] <- v / iteration.step
    out.idx <- out.idx + 1
  }
  out
}

# updater.type="Metropolis" の場合,
# initial.state のサイズが大きいと発散して計算できないケースがある.
example.exec <- function(iteration.number=30000
                         , burnin.num=1000
                         , para=list(theta=0.4, ncol=3, nrow=3)
                         , initial.state=c(1,1,1)
                         , updater.type="Metropolis"
                         , model="Global"
                         , selector.type="sequential"
                         , batch=T
){
  switch(model,
         "Global" = source("global_two_body_coupling_system.R")
         , "NN" = source("nearest_neighbor_coupling_system.R")
  )
  fun.prob <- switch(updater.type,
                     "Metropolis" = function(x){get.prob(x=x, para=para$theta)}
                     , "MetropolisRevised" = function(x,index,x.candidate){
                       get.prob.ratio(x=x,index=index,x.candidate=x.candidate,para=para)
                     })
  stopifnot(iteration.number > burnin.num)
  out.x <- seq(from=burnin.num, to=iteration.number - burnin.num, by=(length(initial.state) * burnin.num))
  extractor <- function(x){x[1]}
  
  if(batch){
    x <- simulate(initial.state=initial.state
                  , fun.prob=fun.prob
                  , updater.type=updater.type
                  , iteration.number=iteration.number
                  , selector.type=selector.type
    )
    print(summary(x))
    prepared.stat.fun <- get.stat.fun(samples=x, out.steps=out.x, burnin=burnin.num)
    out.y <- prepared.stat.fun(extractor)
  } else {
    out.y <- simulate.sequentially(initial.state=initial.state
                                   ,fun.prob=fun.prob
                                   ,extractor=extractor
                                   ,iteration.number=iteration.number
                                   ,burnin.number=burnin.num
                                   ,iteration.steps=out.x
                                   ,updater.type="MetropolisRevised"
                                   ,selector.type=selector.type
    )
  }
  mcs <- out.x / length(initial.state)
  plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
  analytic.y <- 0
  lines(x=mcs, y=rep(x=analytic.y,length(mcs)), col="red")
  title(main="expectation of x[1]")
  
  extractor <- function(x){x[1]*x[2]}
  if(batch){
    out.y <- prepared.stat.fun(extractor)
  } else {
    out.y <- simulate.sequentially(initial.state=initial.state
                                   ,fun.prob=fun.prob
                                   ,extractor=extractor
                                   ,iteration.number=iteration.number
                                   ,burnin.number=burnin.num
                                   ,iteration.steps=out.x
                                   ,updater.type="MetropolisRevised"
                                   ,selector.type=selector.type
    )
  }
  plot(x=mcs, y=out.y, type="l", ylim=c(-1,1))
  title(main="expectation of x[1]*x[2]")
  if(length(initial.state) == 3){
    analytic.y <- ((2*exp(3*theta) - 2*exp(-theta)) / (2*exp(3*theta) + 6*exp(-theta)))
    lines(x=mcs, y=rep(x=analytic.y, length(mcs)), col="red")
  }
  
  print(summary(out.y))
}

# example
initial.state <- rep(1, 100)
para <- list(theta=0.1, ncol=10, nrow=10)
set.seed(seed=1000)
example.exec(iteration.number=3000,model="NN",selector.type="random",batch=F
             ,burnin.num=3
             ,initial.state=initial.state
             ,para=para
             ,updater.type="MetropolisRevised")

example.exec(iteration.number=3000,model="NN",selector.type="random",batch=T
             ,burnin.num=3
             ,initial.state=initial.state
             ,para=para
             ,updater.type="MetropolisRevised")

