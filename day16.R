lines <- readLines("day16.txt")
data <- lapply(strsplit(lines, "[,: ]"),`[`,c(2,4,6,8,10,12,14))

kvp <- list()
for(d in data){
  kvp[[d[1]]] <- as.numeric(d[c(3,5,7)])
  names(kvp[[d[1]]]) <- d[c(2,4,6)]
}

csa <- c(children=3, cats=7, samoyeds=2, pomeranians=3, akitas=0, vizslas=0, goldfish=5, trees=3, cars=2, perfumes=1)

print(which(sapply(kvp, function(x){all(x == csa[names(x)])})))


## Part 2

less  <- c("pomeranians", "goldfish")
more  <- c("cats", "trees")
equal <- names(csa)
equal <- equal[!(equal %in% more | equal %in% less)]

print(which(sapply(kvp, function(x){
    all(x[equal] == csa[names(x[equal])], na.rm=TRUE) &
    all(x[more] > csa[more], na.rm=TRUE) &
    all(x[less] < csa[less], na.rm=TRUE)
})))

