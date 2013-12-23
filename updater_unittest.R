#' test
rm(list=ls())
source("updater.R")

test.get.selector <- function(){
  f1 <- get.selector(degreeoffreedom=3, type="sequential")
  stopifnot( f1() == 1 )
  stopifnot( f1() == 2 )
  stopifnot( f1() == 3 )
  stopifnot( f1() == 1 )
  stopifnot( f1() == 2 )
  stopifnot( f1() == 3 )
}

test.candidate.sampler.reverse <- function(){
  x <- c(1:10)
  stopifnot(-x == candidate.sampler.reverse(x))
  x <- -3
  stopifnot(-x == candidate.sampler.reverse(x))
}

test.updater.metropolis <- function(){
  fun.prob <- sum
  updater <- updater.metropolis(fun.prob=fun.prob,candidate.sampler=candidate.sampler.reverse)
  x <- 1
  r <- candidate.sampler.reverse(x) / x
  stopifnot( r == -1 )
  # -1 < 0 なので必ず棄却される（本当は確率ならありえないが）
  x <- updater(x,index=1)
  stopifnot( x == 1)
  x <- updater(x,index=1)
  stopifnot( x == 1)
}

test.updater.metropolis.2 <- function(){
  fun.prob <- function(x){x[2]} 
  updater <- updater.metropolis(fun.prob=fun.prob,candidate.sampler=candidate.sampler.reverse)
  # 1回目
  x <- c(1,1)
  y <- c(-1,1) # reversed
  r <- 1 / 1 
  stopifnot( r == 1 )
  z <- updater(x,index=1)
  # runif <= 1 なので必ず採用される
  stopifnot(z[1] == y[1])
  stopifnot(z[2] == y[2])
  # 2回目
  x <- c(-1,1)
  y <- c(-1,-1) # reversed
  r <- -1 / 1 
  stopifnot( r == -1 )
  # runif > -1 なので必ず棄却される
  z <- updater(x, index=2)
  stopifnot(z[1] == x[1])
  stopifnot(z[2] == x[2])
}


test.get.selector()
test.candidate.sampler.reverse()
test.updater.metropolis()
test.updater.metropolis.2()