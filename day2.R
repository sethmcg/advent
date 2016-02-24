x <- readLines("day2.txt")

x <- strsplit(x, "x")

area <- function(v){
  v <- as.numeric(v)
  a <- v[c(1,2,3)] * v[c(2,3,1)]
  min(a) + 2*sum(a)
}

print(sum(sapply(x, area)))


## part2

ribbon <- function(v){
  v <- as.numeric(v)
  prod(v) + 2*min(v[c(1,2,3)] + v[c(2,3,1)])
}

print(sum(sapply(x, ribbon)))
  