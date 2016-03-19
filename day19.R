
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
## 1 rule adds:  2 Ys, an Ar/Rn pair, and 3 other new symbols (+7 total)
## 8 rules add:  1 Y,  an Ar/Rn pair, and 2 other new symbols (+5 total)
## 11 rules add: 0 Ys, an Ar/Rn pair, and 1 other new symbol  (+3 total)
## All other rules add 1 symbol
##
## All ways of building a given set of terminals add the same length:
## e.g., 2 Y + 4 Ar/Rn = 1*7 + 3*3 = 2*5 + 2*3 = +16 symbols
## Therefore, just count terminals & do a little arithmatic
## (Final -1 is because first step (e -> X) adds no symbols)

y <- unlist(strsplit(gsub('([[:upper:]])', ' \\1', target), " "))[-1]
nsym  <- length(y)
ny    <- sum(y == "Y")
npair <- sum(y == "Ar") - ny
nother <- nsym - 5*ny - 3*(npair)

print(nother + ny + npair -1 )

