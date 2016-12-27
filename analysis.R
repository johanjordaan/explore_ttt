library("data.table")
library("purrr")
library("ggplot2")
library("rt3")

set.seed(1337)
ptm <- proc.time()
games <- lapply(c(1:100000),function(i) { return(playGame(randomMovePlayer,randomMovePlayer)) })
print(proc.time() - ptm)

rows <- reduce(games,function(acc,game) { return(acc+game$numMoves) },.init=0)
dt <- data.table(
    gameId=rep(0,rows),
    numMoves=rep(0,rows), 
    winner=rep(0,rows),
    startingPlayer=rep(0,rows),
    
    moveNumber=rep(0,rows),
    result=rep(0,rows), 
    move=rep(0,rows),
    player=rep(0,rows)
)
mat <- as.matrix(dt)  
gameIndex <- 0

strip <- function(acc,game) {
  for(i in c(1:game$numMoves)) {
    result <- 0
    if(game$winner != NONE) {
      if(game$movesP[i] == game$winner) { result <- 2 } 
      else { result <- 1 }
    }
  
    mat[acc,1] <<- gameIndex
    mat[acc,2] <<- game$numMoves
    mat[acc,3] <<- switch(game$winner,X=0,O=1,"_"=2)
    mat[acc,4] <<- switch(game$startingPlayer,X=0,O=1)

    mat[acc,5] <<- i
    mat[acc,6] <<- result
    mat[acc,7] <<- game$moves[i]
    mat[acc,8] <<- switch(game$movesP[i],X=0,O=1)
      
    acc <- acc + 1
  }
  gameIndex <<- gameIndex+1
  return(acc)
}

# Reduces with side effect of updating the mat
# Save it for later use
ptm <- proc.time()
reduce(games,strip,.init=1)
print(proc.time() - ptm)
dt <- as.data.frame(mat)
dt$result <- factor(dt$result,labels=c("Draw","Loss","Win")) 
dt$move <- factor(dt$move) 
dt$player <- factor(dt$player,label=c(X,O))
dt$winner <- factor(dt$winner,label=c(X,O,"NONE"))
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
    if(game$startingPlayer == game$winner) { class <- 2 } 
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


