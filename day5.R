input <- scan(file="day5.txt", what="character")

vowels <- c("a","e","i","o","u")
doubles <- paste0(letters, letters)
forbidden <- c("ab", "cd", "pq", "xy")


nice <- function(x){
  y <- unlist(strsplit(x, ""))
  yy <- head(paste0(y, y[-1]), -1)

  nvowels <- sum(y %in% vowels)
  ndoubles <- sum(yy %in% doubles)
  naughty <- any(yy %in% forbidden)

  nvowels > 2 && ndoubles > 0 && !naughty
}

print(sum(sapply(input, nice)))


## Part 2

aba <- outer(letters, letters, function(a,b){paste0(a,b,a)})

nice2 <- function(x){
  y <- unlist(strsplit(x, ""))
  yy <- head(paste0(y, y[-1]), -1)

  ty <- table(yy)
  ty <- ty[ty > 1]
  if(length(ty) < 1){ return(FALSE) }

  ip <- lapply(names(ty), function(a){which(yy==a)})
  ipd <- sapply(ip, function(b){abs(outer(b,b,`-`))}) 
  if(all(ipd < 2)){ return(FALSE) }
  
  yyy <- head(paste0(yy, y[-1:-2]), -1)
  return(any(yyy %in% aba))
}

print(sum(sapply(input, nice2)))
