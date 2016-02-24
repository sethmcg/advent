df <- read.table("day14.txt",stringsAsFactors=FALSE)

df <- df[,c(1,4,7,14)]
df[5] <- list()

for(i in 1:nrow(df)){ 
  x <- c(rep(df[i,2],df[i,3]), rep(0,df[i,4]))
  df[i,5] <- sum(rep_len(x, 2503))
}

print(max(df[,5]))


## Part 2

df <- read.table("day14.txt",stringsAsFactors=FALSE)
df <- df[,c(1,4,7,14)]
colnames(df) <- c("reindeer","speed","fly","rest")

nr <- nrow(df)
nt <- 2503

speed <- array(dim=c(nr,nt))
rownames(speed) <- df$reindeer
for(r in 1:nr){
  speed[r,] <- rep(c(rep(df[r,"speed"],
                         df[r,"fly"]),
                         rep(0,df[r,"rest"])),
                   length=nt)
}

pos <- apply(speed, 1, cumsum)
## comes out transposed, but whatever...


print(max(table(df$reindeer[apply(pos,1,which.max)])))
