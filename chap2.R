setwd("D:/sony/FinEcon")
dat = read.csv("datasets/Stock_bond.csv", header=TRUE)
dim(dat)
head(dat)
names(dat)
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC)
plot(F_AC)

# problem 1
n = dim(dat)[1]
GM.return = GM_AC[2:n]/GM_AC[1:(n-1)] -1
F.return = F_AC[2:n]/F_AC[1:(n-1)] -1
par(mfrow=c(1,1))
plot(GM.return, F.return)
# it has a very weak positive correlation
# there are quite a few outliers
# it looks like a black hole.

#problem 2
GM.logreturn = log(GM.return+1)
summary(GM.logreturn)
plot(GM.return, GM.logreturn)
# they are quite highly correlated
cor(GM.return, GM.logreturn)

summary(dat)

# simulations for hedge fund's problem
# The daily log returns on the stock have a mean of 0.05/year and 
# a standard deviation of .23/year. These can be converted to rates per 
# trading day by dividing 253 and sqrt(253) respectively
# 365 days but 2 out of 7 are holidays
# 365*5/7 = 260.7143 is this close enough

#problem 3 page 13
# the probability of that the value of the stock
# will be below 950,000 ater 45 trading days

niter = 1e5 # number of iterations
below = rep(0, niter) # set up storage
set.seed(2016)

for (i in 1:niter) {
  # create log normals for 45 days
  r =rnorm(45, mean =.05/253, sd = .23/sqrt(253)) 
  
  # this is the amount after 45 days
  logPrice = log(1e6) +cumsum(r) 
  
  # minimum price over next 45 days
  minlogP = min(logPrice) 
  
  #we only consider MINIMUM log price less than log(950,000)
  # only consider the minimums
  # for sure it's less than???
  below[i] = as.numeric(minlogP <log(950000))
}
mean(below)

prob = function(day, value){
  n = 1e5
  below= rep(0,n)
  for (i in 1:n) {
    r = rnorm(day, mean =.05/253, sd = .23/sqrt(253))
    logPrice = log(1e6) +cumsum(r)
    minlogP = min(logPrice)
    below[i] = as.numeric(minlogP< log(value))
  }
  return(mean(below))
}

# Problem 4, 
# probabily that the hedge fund will make a profit of at least $100,000
1 - prob(100, 1100000)
# well I got zero

#Problem 5
# so there is a good chance that the hedge fund will suffer loss
set.seed(2016)
prob(100,950000)
# approx .6438

#Problem 6




