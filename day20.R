factors <- function(x){
  y <- seq(sqrt(x))
  y <- y[(x %% y) ==0]
  unique(c(y, x/y))
}

presents <- function(x){
  sum(factors(x))*10
}

target <- 33100000


## Why 7e5 for the starting point: 
## I got 803880 as a programming error, and was told "too high".  
## My code was working but super-slow at that point, so I blind-guessed
## 7e5 to bracket the value, and was told "too low".  So we might
## as well start with 7e5 to save time.

x <- 7e5
while(TRUE){
  if(presents(x) >= target){
    print(x)
    break
  }
  x <- x+1
}

## Part 2

p2 <- function(x){
  y <- seq(50)
  y <- y[(x %% y) == 0]
  z <- x/y
  sum(z)*11
}

## start where we left off: x <- 776160

while(TRUE){
  if(p2(x) >= target){
    print(x)
    break
  }
  x <- x+1
}
