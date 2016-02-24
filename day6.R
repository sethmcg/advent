L <- readLines("day6.txt")

L <- gsub(" through ", " ", L)
L <- gsub("turn ", "", L)
L <- gsub(",", " ", L)
L <- strsplit(L, " ")

x <- array(FALSE, dim=c(1000,1000))

for(i in L){
  y <- as.numeric(i[2:5])+1
  w <- y[1]; s <- y[2]; e <- y[3]; n <- y[4]
  if(i[1] == "on")     x[w:e,s:n] <- TRUE
  if(i[1] == "off")    x[w:e,s:n] <- FALSE
  if(i[1] == "toggle") x[w:e,s:n] <- !x[w:e,s:n]
}

image(x, main=sum(x))
print(sum(x))


## Part 2

x <- array(0, dim=c(1000,1000))

for(i in L){
  y <- as.numeric(i[2:5])+1
  w <- y[1]; s <- y[2]; e <- y[3]; n <- y[4]
  if(i[1] == "on")     x[w:e,s:n] <- x[w:e,s:n] + 1
  if(i[1] == "off")    x[w:e,s:n] <- x[w:e,s:n] - 1
  if(i[1] == "toggle") x[w:e,s:n] <- x[w:e,s:n] + 2
  x[x<0] <- 0
}

image(x, main=sum(x))
print(sum(x))
