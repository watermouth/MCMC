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
