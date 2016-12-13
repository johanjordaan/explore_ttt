library("data.table")
library("purrr")
library("rt3")

games <- lapply(c(1:1000),function(i) { return(playGame(randomMovePlayer,randomMovePlayer)) })

buildDf <- function(class,numMoves,moveNumber,move) {
  df <- data.frame(
    class = class,
    numMoves = numMoves,
    moveNumber = moveNumber,
    move = move
  )
  return(df)
} 

strip <- function(acc,game) {
  for(i in c(1:game$numMoves)) {
    class <- "d"
    if(game$winner != NONE) {
      if(game$movesP[i] == game$winner) { class <- "w" } 
      else { class <- "l" }
    }
  
    acc <- rbind(acc, buildDf(class,game$numMoves,i,game$moves[i]))  
  }
  return(acc)
}

g <- reduce(games,strip,.init=buildDf("",0,0,0)[-1,])
print(g)
