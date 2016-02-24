Boss <- list(hp=104, atk=8, def=1)

items <- read.table("day21.txt", header=TRUE, row.names=1, stringsAsFactors=FALSE)

weapon <- rownames(items)[items$type=="weapon"]
 armor <- rownames(items)[items$type=="armor"]
  ring <- rownames(items)[items$type=="ring"]

gear <- expand.grid(weapon=weapon, armor=armor, left=ring, right=ring, stringsAsFactors=FALSE)
gear <- gear[gear$left != gear$right,]

stats <- items[gear$weapon,2:4]
stats <- stats + items[gear$armor,2:4]
stats <- stats + items[gear$left, 2:4]
stats <- stats + items[gear$right,2:4]

gear <- cbind(gear, stats)
gear$hp <- rep(100, nrow(gear))

victory <- function(player, boss=Boss){
  p.ttd <- ceiling(player$hp / max(boss$atk - player$def, 1))
  b.ttd <- ceiling(boss$hp   / max(player$atk - boss$def, 1))
  return(p.ttd >= b.ttd)
}

for(r in 1:nrow(gear)){
  gear$victory[r] <- victory(gear[r,])  
}


print(min(gear[gear$victory,"cost"]))

## Part 2

print(max(gear[!gear$victory,"cost"]))
