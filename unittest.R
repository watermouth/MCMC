test.get.selector <- function(){
  f1 <- get.selector(degreeoffreedom=3, type="sequential")
  stopifnot( f1() == 1 )
  stopifnot( f1() == 2 )
  stopifnot( f1() == 3 )
  stopifnot( f1() == 1 )
  stopifnot( f1() == 2 )
  stopifnot( f1() == 3 )
}
