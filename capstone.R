library(foreign)
db = file.choose()
dataset = read.spss(db, to.data.frame=TRUE)
attributes(dataset)$variable.labels
dataset$ID <- row.names(dataset)
dim(dataset)
#dataset_micro = read.table(file.choose(),header = TRUE) # with .dat file

str(dataset)
head(dataset)


library("dplyr")
nrow(distinct(test))/nrow(dataset) = 32%


test <- dataset[]

?combn
x <- 1:145
n <- 5
m <- t(combn(x,n))
m


library(foreign)
db = "/home/yuming_fu_ucdconnect_ie/LFS97.SAV"
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
suda <- suda2()

