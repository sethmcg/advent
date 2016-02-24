x <- sapply(strsplit(readLines("day18.txt"),""),`==`,"#")

n <- nrow(x)
zero <- matrix(0,n,n)

for(i in 1:100){
  y <- zero
  y[  ,-1] <- y[  ,-1] + x[  ,-n]
  y[  ,-n] <- y[  ,-n] + x[  ,-1]
  y[-1,  ] <- y[-1,  ] + x[-n,  ]
  y[-n,  ] <- y[-n,  ] + x[-1,  ]
  y[-1,-1] <- y[-1,-1] + x[-n,-n]
  y[-n,-n] <- y[-n,-n] + x[-1,-1]
  y[-1,-n] <- y[-1,-n] + x[-n,-1]
  y[-n,-1] <- y[-n,-1] + x[-1,-n]

  z <- x
  z[!x & y==3] <- TRUE
  z[x & (y<2 | y>3)] <- FALSE

## Part 2
#  z[1,1] <- z[n,1] <- z[1,n] <- z[n,n] <- TRUE

  x <- z
}

print(sum(x))
