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
  selector <- get.selector(degreeoffreedom=1,type="sequential")
  fun.prob <- sum
  updater <- updater.metropolis(fun.prob=fun.prob,candidate.sampler=candidate.sampler.reverse)
  x <- 1
  r <- candidate.sampler.reverse(x) / x
  stopifnot( r == -1 )
  # -1 < 0 なので必ず採用される
  x <- updater(x)
  stopifnot( x == -1)
  x <- updater(x)
  stopifnot( x == 1)
}

test.updater.metropolis.2 <- function(){
  selector <- get.selector(degreeoffreedom=1,type="sequential")
  fun.prob <- function(x){sum(x)} 
  updater <- updater.metropolis(fun.prob=fun.prob,candidate.sampler=candidate.sampler.reverse)
  x <- c(1,1)
  r <- 2 / (2) 
  stopifnot( r == 1 )
  # 1 >= 1 なのでほぼ必ず棄却される
  x <- updater(x)
  stopifnot( sum(x) == 2)
  x <- updater(x)
  stopifnot( sum(x) == 2)
}


test.get.selector()
test.candidate.sampler.reverse()
