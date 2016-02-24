nbags <- 3

## part 2
# nbags <- 4

x <- unlist(read.table("day24.txt"), use.names=FALSE)
size <- sum(x) / nbags

m <- min(which(cumsum(rev(x)) >= size))

while(TRUE){

  bags <- t(combn(x,m))
  groups <- apply(bags, 1, sum) == size

  if(sum(groups) > 0){
    bags <- bags[groups, ]
    break
  }
  m <- m + 1
}

print(min(apply(bags,1,prod)))


## Wa-hey!  The smallest QE is the right answer!  (This is not
## guaranteed, but it seems to be generally the case when all 
## the inputs are prime, as given in the puzzle.  And hey, it 
## would be easy enough to test with a smidge of recursion.)
