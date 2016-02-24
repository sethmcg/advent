
seed <- unlist(strsplit("1113122113",""))

looksay <- function(s){
    x <- rle(s)
    as.character(c(rbind(x[[1]],x[[2]]))) 
}

for(i in 1:40){
    seed <- looksay(seed)
}

print(length(seed))

## part 2

for(i in 1:10){
    seed <- looksay(seed)
}

print(length(seed))
