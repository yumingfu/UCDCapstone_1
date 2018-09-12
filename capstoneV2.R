install.packages("foreign")
install.packages("sdcMicro")
install.packages("dplyr")
install.packages("Amelia")
install.packages("arrangements")
library(arrangements)

library(foreign)
db = file.choose()
dataset = read.spss(db, to.data.frame=TRUE)
db = "~/Documents/Capstone project/LFS_Data/LFS97.SAV"
dataset = read.spss(db, to.data.frame=TRUE)
attributes(dataset)$variable.labels
dataset$ID <- row.names(dataset)
dim(dataset)
#dataset_micro = read.table(file.choose(),header = TRUE) # with .dat file

str(dataset)
head(dataset)

library("dplyr")
nrow(dataset)
nrow(distinct(test))/nrow(dataset) = 32%


test <- dataset[]

?combn
x <- 1:145
n <- 5
m <- t(combn(x,n))
m


library(foreign)
set.seed(1)
db = "/home/Yuming/LFS97.SAV"
dataset = read.spss(db, to.data.frame=TRUE)
attributes(dataset)$variable.labels
dataset$ID <- row.names(dataset)
dim(dataset)

str(dataset)
library(Amelia)
missmap(dataset)
library(sdcMicro)
?suda2()
?sdcMicro
str(dataset)
f <- freqCalc(dataset, keyVars = c(110:122), w=30)
f
?indivRisk
ind <- indivRisk(f)
plot(ind)
dim(re.id.data)
test <- subset(re.id.data, select= -c(17,18))
test
suda <- suda2(test)
suda
summary(suda)
library(arrangements)
?combinations
m <- combinations(130:166,10,replace = F)
m <-as.data.frame(m)
choose(166,10) #number of different combintation 
3.321048e+15/1000/60/60/24/365  

choose(30,10)

Unique <- function (X){
  test <- subset(dataset,)
  
}

save(m, file= "~/m.Rdata")
save(f,file= "~/f.Rdata")

load("~/m.Rdata")
load("~/f.Rdata")

### parallell 
library(parallel)
num <- detectCores()
cl <- makeCluster(num)
clusterEvalQ(cl, {
  library(sdcMicro)
  library(dplyr)
  library(foreign)
  db = "/home/Yuming/LFS97.SAV"
  dataset = read.spss(db, to.data.frame=TRUE)
})
#stopCluster(cl)

m2 <- combinations(10,5)
m2
?combinations()
Unique <- function(X) {
  test <- subset(dataset, select = c(X))
  res <- c(as.numeric(nrow(distinct(test)))/145424, as.character(X))
  return(res)
}

UniqueNA <- function(X) {
  test <- subset(dataset, select = c(X))
  res <- c(as.numeric(nrow(distinct(test,na.rm=F)))/145424, as.character(X))
  return(res)
}
res <- UniqueNA(c(1,2,3))
res
### 22 <-5  only 6% of them 
?parApply
result <- parApply(cl,m2, 1, Unique)
result
?lapply
a <- transform(m,  order =  sample(c(1,2,3,4), size = nrow(m),0.25))
str(a)
split.m <- lapply(split(a[,1:10],a[,6]), as.matrix)
print(split.m[[1]])
####################################################################################################
res <- apply(m2,1,FUN = Unique)
res <- t(res)
res <- as.data.frame(res)
str(res)
res$V1 <- as.numeric(as.character(res$V1))
res
?sort()

####################################################################################################
######################### hill climbing 
####################################################################################################
str(dataset)
all.data <- dataset
nrow(distinct(all.data))/145424 
#if we konw all, we can be 100% to identify anyone in the lis
#### COB "Country of Birth", SEX "Sex", Nationality "Nationality", MARSTAT "Martial Status", FSTAT "Employment Status", JOBTYPE "type of job", Age "age"
begining.data <- subset(dataset, select = c (34,35,37,41,43,44,57,117))
begining.data
nrow(distinct(begining.data))/145424    #<- 0.05733579

### 
""" SEX : 34    COB :35    NATIONLY: 37  MARSTAT 41  OCCUP: 43  FSTAT : 44 JOBTYPE: 57 AGE 117 """
# if attact is focused on emrigants : then RESYRAGO (residence a year ago): 38 MTHARRIV month of arrival 39, 

HillClimb <- function(x0, search.space){
  #calculate the initioal point
  begining.data <- subset(dataset,select = x0)
  old.score <- nrow(distinct(begining.data))/153381 
  #cat("old.score=", old.score)
  for (i in search.space) {
    new.data <- subset(dataset, select = c(x0,i))
    new.score <- nrow(distinct(new.data))/153381
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
nrow(dataset)
search.space <- seq(1,166)
# remove <- c(34,35,37,41,43,44,57,117)
# search.space <- search.space[!search.space %in% remove]
search.space <- search.space[!search.space == 1]
search.space
res <- HillClimb(c(34,35,37,41,43,44,57,104,105,131,93,2,92), search.space = search.space)



##### 
dataset_test <- dataset
dataset_test
dataset_test[is.na(dataset_test)] <- 0
re.id.data[is.na(re.id.data)] <- "1"
str(re.id.data)
re.id.data <- subset(dataset, select = c(34,35,37,41,43,44,57,117,129,127,120,31,115,6,109,90,167))
#DE-ANONYMIZATION CODE
re.id.data <- re.id.data[sample(nrow(re.id.data)),]

compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

list <-seq(1,145424)
list
SearchNext <- function (list,colname,n){
  new.list <- c()
  for (i in list){
    #print(re.id.data[n,colname])
    #print(dataset[i,colname])
    if (compareNA(re.id.data[n,colname],dataset[i,colname])){
      new.list <- c(new.list,i)
    }
  }
  return(new.list)}

res<- SearchNext(list,"SEX",1)
res
res <- SearchNext(res,"COB",1)
res
res <- SearchNext(res,"MARSTAT",1)
res


for (i in 1:nrow(re.id.data)) {
  if (compareNA(re.id.data$SEX[i], dataset$SEX[i])) {
    if (compareNA(re.id.data$COB[i], dataset$COB[i])) {
      if (compareNA(re.id.data$NATIONLY[i], dataset$NATIONLY[i])) {
        if (compareNA(re.id.data$MARSTAT[i], dataset$MARSTAT[i])) {
          if (compareNA(re.id.data$OCCUP[i], dataset$OCCUP[i])) {
            if (compareNA(re.id.data$FSTAT[i], dataset$FSTAT[i])) {
              if (compareNA(re.id.data$JOBTYPE[i], dataset$JOBTYPE[i])) {
                if (compareNA(re.id.data$AGE[i], dataset$AGE[i])) {
                  if (compareNA(re.id.data$GROSSING[i], dataset$GROSSING[i])) {
                    if (compareNA(re.id.data$FAMCYCLE[i], dataset$FAMCYCLE[i])) {
                      if (compareNA(re.id.data$SE[i], dataset$SE[i])) {
                        if (compareNA(re.id.data$LINE[i], dataset$LINE[i])) {
                          if (compareNA(re.id.data$YRCONST[i],dataset$YRCONST[i])) {
                            if (compareNA(re.id.data$NOPER[i],dataset$NOPER[i])) {
                              if (compareNA(re.id.data$EDUCCOM[i],dataset$EDUCCOM[i])) {
                                if (compareNA(re.id.data$YRJOBC[i] ,dataset$YRJOBC[i])) {
                                  re.id.data$NID[i] <- dataset$ID[i]
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }else {print(i)} 
            }
          }
        }
      }
    }
  }
  }}

res <- SearchNext(list,"COB",1)
res
res2 <- SearchNext(res,"COB",1)
res2
is.vector(res)

FindNext <- function (list,n) {
  res <- SearchNext(list,"SEX",n)
  print(length(res))
  res <- SearchNext(res,"COB",n)
  res <- SearchNext(res,"NATIONLY",n)
  res<- SearchNext(res,"MARSTAT",n)
  res<- SearchNext(res,"OCCUP",n)
  res<- SearchNext(res,"FSTAT",n)
  print(length(res))

  res<- SearchNext(res,"JOBTYPE",n)
  res<- SearchNext(res,"AGE",n)
  res<- SearchNext(res,"GROSSING",n)
  res<- SearchNext(res,"FAMCYCLE",n)
  res<- SearchNext(res,"SE",n)
  res<- SearchNext(res,"LINE",n)
  print(length(res))

  res<- SearchNext(res,"YRCONST",n)
  print(length(res))
  
  res<- SearchNext(res,"NOPER",n)
  print(length(res))
  
  res<- SearchNext(res,"EDUCCOM",n)
  print(length(res))
  res<- SearchNext(res,"YRJOBC",n)
  #print(res)
  return(res)
}
test <- FindNext(list,1)
test[1]
dataset[49374,]
re.id.data[1,]
SearchNext(SearchNext(list,"SEX"),"COB")

for (i in 1:10) {
  number <- FindNext(list,i)
  re.id.data$NID[i] <- number[1]
  list <- list[!list == number[1]]
}

re.id.data[1:10,]

install.packages("foreach")
install.packages("doParallel")
library(foreach)
library(doParallel)
cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)

foreach(i=1:10) %dopar% {
  number <- FindNext(list,i)
  re.id.data$NID[i] <- number[1]
  list <- list[!list == number[1]]
}

library(foreign)
dataset <- read.dta("~/Documents/Capstone project/0061-00 LFS/0061-00_Data/Stata/0061-20_lfs_2017.dta") 
dim(mydata)

######test 
re.id.data$NID <- NA
re.id.data %>% summarize(count = count(compareNA(ID,NID)))


c(1,2) + c(3)
