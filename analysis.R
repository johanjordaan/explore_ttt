library("data.table")
library("purrr")
library("ggplot2")
library("rt3")

ptm <- proc.time()
games <- lapply(c(1:5),function(i) { return(playGame(randomMovePlayer,randomMovePlayer)) })
print(proc.time() - ptm)

rows <- reduce(games,function(acc,game) { return(acc+game$numMoves) },.init=0)
dt <- data.table(
    class=rep(0,rows), 
    numMoves=rep(0,rows), 
    moveNumber=rep(0,rows),
    move=rep(0,rows)
)
mat <- as.matrix(dt)  

strip <- function(acc,game) {
  for(i in c(1:game$numMoves)) {
    class <- 0
    if(game$winner != NONE) {
      if(game$movesP[i] == game$winner) { class <- 1 } 
      else { class <- -1 }
    }
  
    mat[acc,1] <- class
    mat[acc,2] <- game$numMoves
    mat[acc,3] <- i
    mat[acc,4] <- game$moves[i]
    acc <- acc + 1
  }
  print(acc)
  print(mat)
  return(acc)
}
g <- reduce(games,strip,.init=1)
dt <- as.data.frame(mat)
saveRDS(dt,"moves100k.rds")
dt2 <- readRDS("./moves100k.rds")

g1 <- ggplot(data=dt,aes(x=moveNumber,fill=class))+
      geom_bar()+
      scale_x_discrete(name ="Move Number", limits=c(1:9))
print(g1)

g2 <- ggplot(data=dt,aes(x=move,fill=class))+
  geom_bar()+
  scale_x_discrete(name ="Move", limits=c(1:9))
print(g2)


