library("data.table")
library("purrr")
library("rt3")

games <- lapply(c(1:10),function(i) { return(playGame(randomMovePlayer,randomMovePlayer)) })

strip <- function(game) {
  first <- TRUE;
  retVal <- NULL;
  for(i in c(1:game$numMoves)) {
    class <- "d"
    if(game$winner != NONE) {
      if(game$movesP[i] == game$winner) {
        class <- "w"
      } else {
        class <- "l"
      }
      df <- data.frame(
        class = class,
        numMoves = game$numMoves,
        moveNumber = i,
        move = game$moves[i]
      )

      if(first) {
        retVal <- df;
        first <- FALSE;
      }
      else {
        retVal <- rbind(retVal,df)
      }
    }
  }

  return(retVal);
}

g <- lapply(games,strip)
gm <- merge_all(g)

#
#
# retVal <- NULL
# first <- TRUE
# moves <- map(games,function(game){
#
#   for(i in c(1:game$numMoves)) {
#
#     class <- "d"
#     if(game$winner != NONE) {
#       if(game$movesP[i] == game$winner) {
#         class <- "w"
#       } else {
#         class <- "l"
#       }
#     }
#
#     df <- data.frame(
#       class = class,
#       numMoves = game$numMoves,
#       moveNumber = i,
#       move = game$moves[i]
#     )
#
#     if(first) {
#       retVal <- df;
#       first <- FALSE;
#     }
#     else {
#       retVal <- rbind(retVal,df)
#     }
#   }
# })
#
#
#
#
