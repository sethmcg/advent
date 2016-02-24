
x <- scan(file="day1.txt", what="character")

x <- unlist(strsplit(x, ""))

up <- sum(x == "(")
dn <- sum(x == ")")

print(up - dn)

### Part2

floor <- cumsum(x=="(") - cumsum(x==")")
print(min(which(floor == -1 )))

