get.prob.ratio <- function(x, index, x.candidate, para){
  stopifnot(length(index) == length(x.candidate))
  x.mat <- matrix(data=x, nrow=para$nrow, ncol=para$ncol, byrow=T)
  idx.row <- 1 + ((index -1) %/% para$nrow)
  idx.col <- 1 + ((index -1) %% para$ncol)
  idx.up   <- if (idx.row - 1 < 1) para$nrow else (idx.row - 1)
  idx.down <- if (idx.row + 1 > para$nrow) 1 else (idx.row + 1)
  idx.left <- if (idx.col - 1 < 1) para$ncol else (idx.col - 1)
  idx.right<- if (idx.col + 1 > para$ncol) 1 else (idx.col + 1)
  exp(para$theta * sum(c(x.mat[idx.up, idx.col]
                   , x.mat[idx.down, idx.col]
                   , x.mat[idx.row, idx.left]
                   , x.mat[idx.row, idx.right]
                   )
                 ) * (x.candidate - x[index]))
}

test.get.prob.ratio <- function(){
  # (2,2)を更新
  #   -1        -1
  #  1 1 1 ->  1-1 1
  #    1         1
  x <- c(1,-1,1, 1,1,1, 1,1,1)
  index <- 5
  x.candidate <- -1
  para <- list(theta=0.1, ncol=3, nrow=3)
  actual <- get.prob.ratio(x=x,index=index,x.candidate=x.candidate,para=para)
  expected <- exp(para$theta * (-1 -1) * (-1 + 1 + 1 + 1))
  stopifnot(expected == actual)
  
  # (1,2)を更新
  #   -1         1
  #  1 1 1 ->  1 1 1
  #    1         1
  x <- c(1,-1,1, 1,1,1, 1,1,1)
  index <- 2
  x.candidate <- 1
  para <- list(theta=0.1, ncol=3, nrow=3)
  actual <- get.prob.ratio(x=x,index=index,x.candidate=x.candidate,para=para)
  expected <- exp(para$theta * (1 -(-1)) * (1 + 1 + 1 + 1))
  stopifnot(expected == actual)
  
  # (1,1)を更新
  #  1-1      -1-1
  #  1 1 1 ->  1 1 1
  #    1         1
  x <- c(1,-1,1, 1,1,1, 1,1,1)
  index <- 1
  x.candidate <- -1
  para <- list(theta=0.1, ncol=3, nrow=3)
  actual <- get.prob.ratio(x=x,index=index,x.candidate=x.candidate,para=para)
  expected <- exp(para$theta * (-1 -(1)) * (1 - 1 + 1 + 1))
  stopifnot(expected == actual)
  
  # (3,3)を更新
  #  1-1      -1-1
  #  1 1 1 ->  1 1 1
  #    1-1       1 1
  x <- c(1,-1,1, 1,1,1, 1,1,-1)
  index <- 9
  x.candidate <- 1
  para <- list(theta=0.1, ncol=3, nrow=3)
  actual <- get.prob.ratio(x=x,index=index,x.candidate=x.candidate,para=para)
  expected <- exp(para$theta * (1 -(-1)) * (1 + 1 + 1 + 1))
  stopifnot(expected == actual)
}

test.get.prob.ratio()
