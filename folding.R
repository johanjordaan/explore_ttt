library(purrr)

l = list(
  data.frame(name=c("Johan"),surname=c("Jordaan")),
  data.frame(name=c("Lorraine"),surname=c("Evert-Jordaan"))
  
)

f <- function(acc,d) {
  z <- rbind(acc,data.frame(ns=paste(d$name,d$surname)))  
  return(z)
}

r <- reduce(l,f,.init=df[-1,]) 
