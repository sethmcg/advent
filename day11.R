pw <- "hxbxwxba"
pw <- match(unlist(strsplit(pw,"")),letters)

inc <- function(v){
    v[8] <- v[8]+1
    while(any(v > 26)){
        i <- max(which(v > 26))
        v[i-1] <- v[i-1]+1
        v[i] <- 1
    }
    return(v)
}


bad <- c("l","i","o")
doubles <- paste0(letters,letters)
runs <- paste0(letters[-c(25,26)],letters[-c(1,26)],letters[-c(1,2)])

invalid <- function(v){
  w <- letters[v]
  nv <- length(v)
  if(any(bad %in% w)){return(TRUE)}
  ww <- paste0(w[-nv],w[-1])
  www <- paste0(ww[-nv],w[-c(1,2)])
  if(sum(doubles %in% ww) < 2){return(TRUE)}
  if(any(runs %in% www)){return(FALSE)}
  return(TRUE)
}

while(invalid(pw)){pw <- inc(pw)}

print(paste(letters[pw],collapse=""))

## Part 2

pw <- inc(pw)

while(invalid(pw)){pw <- inc(pw)}

print(paste(letters[pw],collapse=""))
