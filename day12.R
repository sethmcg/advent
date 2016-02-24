x <- gsub("[^0-9-]"," ", readLines("day12.txt"))
print(sum(na.omit(as.numeric(unlist(strsplit(x, " "))))))

## Alternate w/ JSON parsing:
#library(jsonlite)
#sum(na.omit(as.numeric(unlist(fromJSON(txt=readLines("day12.txt"))))))

## Part 2

x <- gsub(" ",'', paste(readLines("day12.txt"), collapse=""))

open <- c()

i <- 1

while(i < nchar(x)){
  if(unlist(strsplit(x,''))[i] == "{"){
    open <- c(open, i)
  }
  if(unlist(strsplit(x,''))[i] == "}"){
    N <- length(open)
    L <- open[N]
    open <- open[-N]
    R <- i
    width <- R-L+1
    
    if(any(grep(":\"red\"", substr(x,L,R)))){
      x <- paste0(substr(x,1,L-1),substr(x,R+1,nchar(x)))
      open[open > R] <- open[open > R] - width
      i <- i - width
    }
  }
  i <- i + 1
}


x <- gsub("[^0-9-]"," ", x)
print(sum(na.omit(as.numeric(unlist(strsplit(x, " "))))))
