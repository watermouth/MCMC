#' サイズがメモリ上に乗るくらいに小さい場合はこれを使用できる.

#' 
#' FUNで指定した量の統計量の計算
#' out.step 結果を返すstep（burninを除く)
#' samples matrix(row: MCMC step, column: variables)
stat.fun <- function (quantity.fun=function(x){x[1]}, samples, out.steps, burnin=(0.2 * nrow(samples))){
  if(nrow(samples) <= burnin){
    stop("burn in period is too big")
  }
  x <- matrix(data=samples[(burnin+1):nrow(samples),], nrow=nrow(samples) - burnin)
  out <- sapply(X=out.steps, FUN=
           function(i){
             mean(
               sapply(X=(1:i), FUN=function(j){ quantity.fun(x[j,]) }
               )
             )
           }
  )
  out
}

get.stat.fun <- function (samples, out.steps, burnin){
  f <- function(fun){
    stat.fun(quantity.fun=fun,samples=samples,out.steps=out.steps,burnin=burnin)
  }
  f
}

test.stat.fun <- function (){
  samples <- 1:10
  stopifnot( mean(1:10) == stat.fun(samples=matrix(data=samples, ncol=1),out.steps=10,burnin=0))
  
  out <- stat.fun(samples=matrix(data=samples, ncol=1),out.steps=c(1,5,10),burnin=0)
  stopifnot( mean(1:1) == out[1])
  stopifnot( mean(1:5) == out[2])
  stopifnot( mean(1:10) == out[3])
  
  out <- stat.fun(samples=matrix(data=samples, ncol=1),out.steps=c(1,3,5),burnin=5)
  stopifnot( mean(6:6) == out[1])
  stopifnot( mean(6:8) == out[2])
  stopifnot( mean(6:10) == out[3])
  
  samples.2 <- matrix(data=c(1:10),ncol=2, byrow=T)
  out <- stat.fun(samples=matrix(data=samples.2, ncol=2),out.steps=c(3,5),burnin=0)
  stopifnot( 3 == out[1])
  stopifnot( 5 == out[2])
}

test.stat.fun()
