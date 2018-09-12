HillClimb <- function(x0, search.space){
  #calculate the initioal point
  begining.data <- subset(dataset,select = x0)
  old.score <- nrow(distinct(begining.data))/145424 
  #cat("old.score=", old.score)
  for (i in search.space) {
    new.data <- subset(dataset, select = c(x0,i))
    new.score <- nrow(distinct(new.data))/145424
    #cat("new.score=",new.score)
    if (new.score > old.score) {
      #print("TURE")
      old.score <- new.score
      besti <- i
      best.score <- new.score
      print(besti)
      print(best.score)
    }
    #return(c(besti, best.score))
  }
}
library(foreign)
library(dplyr)
db = "~/Documents/Capstone project/LFS_Data/LFS97.SAV"
dataset = read.spss(db, to.data.frame=TRUE)

search.space <- seq(1,166)
# remove <- c(34,35,37,41,43,44,57,117)
# search.space <- search.space[!search.space %in% remove]
search.space <- search.space[!search.space == 166]
search.space
res <- HillClimb(c(34,35,37,41,43,44,57,117,129,127,120,31,115,6,109,90), search.space = search.space)
0.9952209*nrow(dataset)



search.space <- search.space[!search.space == 166]
