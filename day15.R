library(partitions)

x <- readLines("day15.txt")
x <- gsub(": ","=c(",x)
x <- gsub(", ",",",x)
x <- gsub(" ","=",x)
x <- gsub("$",")",x)
x <- paste(x,collapse=",")
x <- paste0("list(",x,")")
x <- as.matrix(as.data.frame(eval(parse(text=x))))
calories <- x[nrow(x),]
x <- x[-nrow(x),]

recipes <- compositions(100, 4)

score <- function(r){
  s <- as.matrix(x) %*% r
  s <- pmax(0, s)
  prod(s)
}

print(max(apply(recipes, 2, score)))


# Part 2

cal500 <- function(r){sum(r*calories)==500}

recipes500 <- recipes[,which(apply(recipes, 2, cal500))]

print(max(apply(recipes500, 2, score)))
