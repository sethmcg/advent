library(digest)

salt <- "ckczppom"

i <- 1
while(TRUE){
  x <- paste0(salt, i)
  y <- digest(x, algo="md5", serialize=FALSE)
  z <- substr(y, 1, 5)
  if(z == "00000"){ break}
  i <- i+1
}

print(i)


## part 2

while(TRUE){
  x <- paste0(salt, i)
  y <- digest(x, algo="md5", serialize=FALSE)
  z <- substr(y, 1, 6)
  if(z == "000000"){ break}
  i <- i+1
}

print(i)
