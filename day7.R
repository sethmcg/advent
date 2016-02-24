ops <- c(AND=bitwAnd, OR=bitwOr, NOT=bitwNot, LSHIFT=bitwShiftL, RSHIFT=bitwShiftR)

lines <- readLines("day7.txt")
lines <- strsplit(lines, " -> ")


x <- sapply(lines,`[`, 2)
wires <- rep(NA, length(x))
names(wires) <- x
gates <- as.list(wires)
wires[as.character(seq(15))] <- seq(15)

for(L in lines){
    out <- L[2]
    x <- unlist(strsplit(L[1], " "))
    switch(length(x),
           "1" = {wires[out] <- as.numeric(x); gates[[out]] <- NULL; print(L)},
           "2" = {gates[[out]] <- list(op=x[1], args=x[2])},
           "3" = {gates[[out]] <- list(op=x[2], args=x[c(1,3)])}
           )        
}

## Part 2:
# wires["b"] <- 3176

while(length(gates) > 0){
    for(n in names(gates)){
        g <- gates[[n]]
        if(!any(is.na(wires[g$args]))){
            wires[n] <- do.call(ops[[g$op]], as.list(unname(wires[g$args])))
            gates[[n]] <- NULL
        }
    }
}

print(wires["lx"])


