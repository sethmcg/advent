v <- scan(file="day3.txt", what="character")
v <- unlist(strsplit(v, ""))

n <- v == "^"
s <- v == "v"
e <- v == ">"
w <- v == "<"

ns <- cumsum(n-s)
ew <- cumsum(e-w)

xy <- paste(ew,ns,sep=",")
xy <- c("0,0",xy)

print(length(unique(xy)))


## Part 2

r <- seq(2,length(v),2)

xy <- c("0,0")
xy <- c(xy, paste(cumsum(e[r]-w[r]),cumsum(n[r]-s[r]),sep=","))
r <- r-1
xy <- c(xy, paste(cumsum(e[r]-w[r]),cumsum(n[r]-s[r]),sep=","))
print(length(unique(xy)))
