## Input: "To continue, please consult the code grid in the manual.
## Enter the code at row 2981, column 3075."

cy <- 2981
cx <- 3075

nit <- function(x,y){
  sum(seq(0,x)) + sum(seq(0,x+y-2)) - sum(seq(0,x-1))
}


code <- function(n){
  x <- 20151125
  if(n == 1){return(x)}
  for(i in 1:(n-1)){
    x <- (x * 252533 ) %% 33554393
  }
  return(x)
}

print(code(nit(cx, cy)))

