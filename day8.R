x <- scan("day8.txt", what="character", quote=NULL, allowEscapes=FALSE)
x <- strsplit(x,"")

mlen <- function(v){
  n <- length(v)
  m <- n-2
  i <- 2

  while(i < n-1){
    if(v[i] == "\\"){
      if(v[i+1] == "\"" || v[i+1] == "\\"){
        m <- m - 1
        i <- i + 1
      } else if (v[i+1] == "x"){
        m <- m - 3
        i <- i + 3
      }
    }
    i <- i + 1    
  }
  return(m)
}

print(sum(sapply(x, length) - sapply(x, mlen)))

## Part 2

enclen <- function(v){
  length(v) + 2 + sum(v == "\\") + sum(v == "\"")
}

print(sum(sapply(x, enclen) - sapply(x, length)))

