library(gtools)

x <- read.table("day13.clean.txt")

guests <- levels(x[[1]])

n <- length(guests)

happiness <- matrix(nrow=n, ncol=n ,dimnames=list(guests, guests))

for(i in 1:nrow(x)){
  happiness[x[i,1],x[i,3]] <- x[i,2]
}



happy <- function(v){
  sum(happiness[cbind(v[c(1:n,2:n,1)], v[c(2:n,1,1:n)])])
}

seating <- permutations(n, n, guests)
  
print(max(apply(seating, 1, happy)))


## Part 2

happiness <- cbind(happiness, Self=rep(0,n))
happiness <- rbind(happiness, Self=rep(0,n+1))
guests <- c(guests, "Self")
n <- length(guests)

seating <- permutations(n, n, guests)
  
print(max(apply(seating, 1, happy)))
