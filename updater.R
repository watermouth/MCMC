#' 
#' updater(x)
get.updater <- function(updater.type="Metropolis", variable.selector, fun.prob) {
  f <- switch(updater.type,
              "Metropolis" = updater.metropolis(fun.prob=fun.prob
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

updater.metropolis <- function(fun.prob, candidate.sampler){
  f <- function(x,index){
    x.old <- x
    p.this <- fun.prob(x)
    x[index] <- candidate.sampler(x[index])
    p.candidate <- fun.prob(x)
    ratio <- p.candidate / p.this
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

