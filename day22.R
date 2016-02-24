
dmg <- 9
armor <- 7

Game <- list(moves=c(),
             state="active",
             mana=500,
             hp=50,
             boss=51,
             timer=c(poison=0, recharge=0, shield=0, drain=0, missile=0, boss=0)
             )

cost <- c(drain=73, missile=53, poison=173, recharge=229, shield=113)
spells <- names(cost)
cost["boss"] <- 0

turn <- function(g, move){
  g$moves <- c(g$moves, move)

## Part 2
#  if(move != "boss"){
#    g$hp <- g$hp-1
#    if(g$hp <= 0){g$state="lose"; return(g)}
#  }
## end part 2
  
  
  if(g$timer["poison"]){ g$boss <- g$boss - 3 }
  if(g$timer["recharge"]){ g$mana <- g$mana + 101 }
  g$timer <- pmax(g$timer - 1, 0)

  if(g$boss <= 0){g$state="win"; return(g)}
  if(g$timer[move]){g$state="illegal"; return(g)}

  if(move =="boss"){
    g$hp <- g$hp - unname(dmg - ifelse(g$timer["shield"], 7, 0))
    if(g$hp <= 0){g$state="lose"; return(g)}
  } else {
    g$mana <- g$mana - unname(cost[move])
    if(g$mana <=0){g$state="lose"; return(g)}
    if(move =="missile"){ g$boss <- g$boss - 4 }
    if(move =="drain")  { g$boss <- g$boss - 2; g$hp <- g$hp + 2 }
    if(move =="shield") { g$timer["shield"] <- 6 }
    if(move =="poison") { g$timer["poison"] <- 6 }
    if(move =="recharge") { g$timer["recharge"] <- 5 }
  }
  if(g$boss <= 0){g$state="win"; return(g)}
  return(g)
}


filter <- function(newgames){
  state <- sapply(newgames, `[[`, "state")
  active <<- newgames[state=="active"]
  for(s in c("win","lose","illegal")){
    games[[s]] <<- c(games[[s]], newgames[state == s])
  }
#  print(c(active=length(active), sapply(games, length)))
}

active=list(Game)
games <- c(win=list(), lose=list(), illegal=list())

## I didn't want to take for granted that shorter games would be 
## lower mana than longer, but it appears true, so let's use it.

i <- 0
while(length(games$win) == 0){
  i <- i + 1

#  print(paste("Player",i))
  newgames <- list()
  for(s in spells){
    newgames <- c(newgames, lapply(active, turn, s))
  }
  filter(newgames)
  
#  print(paste("Boss",i))
  newgames <- lapply(active, turn, "boss")
  filter(newgames)
}

print(min(sapply(games$win, function(x){sum(cost[x$moves])})))
