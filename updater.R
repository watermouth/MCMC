#' 
#' updater(x)
get.updater <- function(updater.type="Metropolis", variable.selector, fun.prob) {
  f <- switch(updater.type,
              "Metropolis" = updater.metropolis(fun.prob=fun.prob
                                                ,candidate.sampler=candidate.sampler.reverse)
              , "MetropolisRevised" = updater.metropolis.revised(fun.probability.ratio=fun.prob
                                                ,candidate.sampler=candidate.sampler.reverse)
  )
  f
}

get.selector <- function(degreeoffreedom, type){
  counter <- 0
  candidates <- seq(1:degreeoffreedom)
  f.sequential <- function() {
    if (counter %% degreeoffreedom == 0){
      counter <<- 1
    } else {
      counter <<- counter + 1
    }
    counter
  }
  f.random <- function() {sample(x=candidates,size=1,replace=T)}
  f <- switch(type,
              "sequential" = f.sequential,
              "random" = f.random
  )
  f
}

#' 目的分布の規格化定数を除いた部分から計算する.
#' システムサイズが大きい場合は計算できない.
updater.metropolis <- function(fun.prob, candidate.sampler){
  f <- function(x,index){
    x.old <- x
    p.this <- fun.prob(x)
    x[index] <- candidate.sampler(x[index])
    p.candidate <- fun.prob(x)
    ratio <- p.candidate / p.this
    if (ratio >= 1){
      return(x)
    }
    x <- if ( runif(n=1, min=0, max=1) < ratio ){
      x
    } else {
      x.old
    }
    x
  }
  f
}

#' 直接 比を計算する
updater.metropolis.revised <- function(fun.probability.ratio, candidate.sampler){
  f <- function(x,index){
    x.old <- x
    x[index] <- candidate.sampler(x[index])
    ratio <- fun.probability.ratio(x.old, index, x[index])
    if (ratio >= 1){
      return(x)
    }
    x <- if ( runif(n=1, min=0, max=1) < ratio ){
      x
    } else {
      x.old
    }
    x
  }
  f
}

candidate.sampler.reverse <- function(x){
  -x
}

