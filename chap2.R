setwd("D:/sony/FinEcon")
dat = read.csv("datasets/Stock_bond.csv", header=TRUE)
dim(dat)
head(dat)
names(dat)
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC)
plot(F_AC)

n = dim(dat)[1]
GM.return = GM_AC[2:n]/GM_AC[1:(n-1)] -1
F.return = F_AC[2:n]/F_AC[1:(n-1)] -1
par(mfrow=c(1,1))
plot(GM.return, F.return)
# it has a very weak positive correlation