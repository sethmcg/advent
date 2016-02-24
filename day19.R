
x <- readLines("day19.txt")

target <- x[length(x)]
x <- x[-(length(x)+c(0,-1))]
x <- strsplit(x, " ")
x <- t(sapply(x,`[`,-2))

nt <- nchar(target)

out <- c()
for(i in 1:nrow(x)){
  nx <- nchar(x[i,1])
  matches <- gregexpr(x[i,1], target, fixed=TRUE)[[1]]
  for(m in matches){
    if(m > 0){
      out <- c(out, paste0(substr(target,1,m-1),x[i,2],substr(target,m+nx,nt)))
    }
  }
}

print(length(unique(out)))


## Part 2

## There are three 'terminal' symbols that cannot be replaced: Y, Rn, and Ar.
## Rn and Ar are always generated in pairs.
## 1 rule adds 7 characters, with 2 Y and an Ar/Rn pair
## 8 rules add 5 characters with 1 Y and an Ar/Rn pair
## 11 rules add 3 characters with an Ar/Rn pair
## All other rules add 1 character
## Ar/Rn outnumber Y, and all ways of building the terminals add same length
## e.g., 2 Y + 4 Ar/Rn = 1*7 + 3*3 = 2*5 + 2*3 = +16
## Therefore, just count terminals & do a little arithmatic
## (Final -1 is because first step (e -> X) adds no symbols)

y <- unlist(strsplit(gsub('([[:upper:]])', ' \\1', target), " "))[-1]
nsym  <- length(y)
ny    <- sum(y == "Y")
npair <- sum(y == "Ar") - ny
nother <- nsym - 5*ny - 3*(npair)

print(nother + ny + npair -1 )

