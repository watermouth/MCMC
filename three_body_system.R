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

get.selector <- function(degreeoffreedom, type){
  counter <- 0
  #degreeoffreedom.add1 <- 1 + degreeoffreedom
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

updater.metropolis <- function(varable.selector, fun.prob, candidate.sampler){
  index <- variable.selector()
  f <- function(x){
    p.this <- fun.prob(x)
    x[index] <- candidate.sampler(x[index])
    p.candidate <- fun.prob(x)
    ratio <- p.candidate / p.this
    
  }
}

get.updater <- function(updater.type="Metropolis", degree.of.freedom, selector.type="sequential", fun.prob) {
  variable.selector <- get.selector(degreeoffreedom=degree.of.freedom,type=selector.type)
  f <- switch(updater.type,
              "Metropolis" = updater.metropolis(variable.selector, fun.prob)
  )
  f
}

# Initialize

# plot(x=z, y=sapply(X=z, FUN=function(i){prob_numerator(c(-1,-1,-1),para=i)}))
