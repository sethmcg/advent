program <- readLines("day23.txt")
program <- gsub(",", "", program)
program <- strsplit(program, " ")

N <- length(program)

reg <- c(a=0, b=0)
address <- 1

hlf <- function(r){
  reg[r] <<- max(0, round(reg[r] / 2))
  address + 1
}

tpl <- function(r){
  reg[r] <<- reg[r] * 3
  address + 1
}

inc <- function(r){
  reg[r] <<- reg[r] + 1
  address + 1
}

jmp <- function(r) {
  address + as.numeric(r)
}

jie <- function(r) {
  if(! reg[r[1]] %% 2 ){
    address + as.numeric(r[2])
  } else {
   address + 1
  }
}

jio <- function(r) {
  if( reg[r[1]] == 1 ){
   address + as.numeric(r[2])
  } else {
   address + 1
  }
}

instr <- list(hlf=hlf, tpl=tpl, inc=inc, jmp=jmp, jie=jie, jio=jio)

while(TRUE){
  x <- program[[address]]
  address <- instr[[x[1]]](x[-1])
  if(address < 1 | address > N) {break}
}

print(reg)


# Part 2

reg <- c(a=1, b=0)
address <- 1

while(TRUE){
  x <- program[[address]]
  address <- instr[[x[1]]](x[-1])
  if(address < 1 | address > N) {break}
}

print(reg)

