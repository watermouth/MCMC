get.prob <- function(x,para){
  theta <- para[1]
  # 2体相互作用
  inner_vec <- theta * combn(x=x,m=2,FUN=function(y){y[1] * y[2]})
  inner_term <- sum(inner_vec)
  exp(inner_term)
}

get.prob.ratio <- function(x, index, x.candidate, para){
  stopifnot(length(index) == length(x.candidate))
  exp(para * sum(x[-index]) * (x.candidate - x[index]))
}

test.get.prob.ratio <- function(){
  eps <- 10^-15
  para <- 0.1
  x <- c(1,1,1)
  index <- 1
  x.candidate <- c(-1,1,1)
  ratio <- get.prob.ratio(x=x, index=index, x.candidate=x.candidate[index],para=para)
  error <- abs((get.prob(x=x.candidate, para=para) / get.prob(x=x, para=para)) - ratio) / ratio 
  stopifnot(error < eps)
  
  para <- 1 
  x <- rep(1,10) 
  index <- 5
  x.candidate <- c(1,1,1,1,-1,rep(1,5)) 
  ratio <- get.prob.ratio(x=x, index=index, x.candidate=x.candidate[index],para=para)
  error <- abs((get.prob(x=x.candidate, para=para) / get.prob(x=x, para=para)) - ratio) / ratio 
  stopifnot(error < eps) 
  
  para <- 0.4
  x <- rep(1,100) 
  index <- 5
  x.candidate <- c(1,1,1,1,-1,rep(1,95)) 
  ratio <- get.prob.ratio(x=x, index=index, x.candidate=x.candidate[index],para=para)
  error <- abs((get.prob(x=x.candidate, para=para) / get.prob(x=x, para=para)) - ratio) / ratio 
  if(!is.nan(error)){
    stopifnot(error < eps)
  } else {
    print("error is nan; get.prob() / get.prob() seems returning nan")
  }
}

test.get.prob.ratio()
