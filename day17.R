x <- as.numeric(readLines("day17.txt"))

coc <- list()
for(m in 4:length(x)){
  coc <- c(coc, combn(x,m,simplify=FALSE))
}

print(sum(sapply(coc, sum) == 150))


## Part 2

fits <- coc[sapply(coc, sum) == 150]

minlen <- min(sapply(fits, length))

print(sum(sapply(fits, length) == minlen))