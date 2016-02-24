library(gtools)

df <- read.table("day9.txt", stringsAsFactors=FALSE)[,c(1,3,5)]

locs <- unique(c(df[,1],df[,2]))
n <- length(locs)

dmat <- matrix(nrow=n, ncol=n, dimnames=list(locs,locs))

for(i in 1:nrow(df)){
    dmat[df[i,1],df[i,2]] <- dmat[df[i,2],df[i,1]] <- df[i,3]
}

routes <- permutations(length(locs), length(locs), locs)

print(min(apply(routes, 1, function(r){sum(dmat[cbind(r[-n],r[-1])])})))

## Part 2:

print(max(apply(routes, 1, function(r){sum(dmat[cbind(r[-n],r[-1])])})))
