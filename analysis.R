library("data.table")
library("purrr")
library("ggplot2")
library("rt3")

ptm <- proc.time()
games <- lapply(c(1:100000),function(i) { return(playGame(randomMovePlayer,randomMovePlayer)) })
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
      if(game$movesP[i] == game$winner) { class <- 2 } 
      else { class <- 1 }
    }
  
    mat[acc,1] <<- class
    mat[acc,2] <<- game$numMoves
    mat[acc,3] <<- i
    mat[acc,4] <<- game$moves[i]
    acc <- acc + 1
  }
  return(acc)
}

# Reduces with side effect of updating the mat
# Save it for later use
ptm <- proc.time()
reduce(games,strip,.init=1)
print(proc.time() - ptm)
dt <- as.data.frame(mat)
saveRDS(dt,"moves100k.rds")


# Extract only the end game state
#
dt2 <- data.table(
  class=rep(0,length(games)) 
)
mat2 <- as.matrix(dt2)  
gameEndState <- function(acc,game) {
  class <- 0
  if(game$winner != NONE) {
    if(game$movesP[1] == game$winner) { class <- 2 } 
    else { class <- 1 }
  }
  mat2[acc,1] <<- class
  acc <- acc + 1
  return(acc)
}
ptm <- proc.time()
reduce(games,gameEndState,.init=1)
print(proc.time() - ptm)
dt2 <- as.data.frame(mat2)


wins <- sum(dt2$class==2)
losses <- sum(dt2$class==1)
draws <- sum(dt2$class==0)

binom.test(wins,wins+losses+draws)


dt2 <- readRDS("./moves100k.rds")

g0 <- ggplot(data=dt2,aes(x=class,fill=as.factor(class)))+ geom_bar()
print(g0)

g1 <- ggplot(data=dt,aes(x=moveNumber,fill=as.factor(class)))+
      geom_bar()+
      scale_x_discrete(name ="Move Number", limits=c(1:9))
print(g1)

g2 <- ggplot(data=dt,aes(x=move,fill=as.factor(class)))+
  geom_bar()+
  scale_x_discrete(name ="Move", limits=c(1:9))
print(g2)


