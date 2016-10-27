library("sn")
library("fGarch")
library("Ecdat")

dim(CRSPday)
summary(CRSPday)
?CRSPday #Center for Research in Security Prices
cov(CRSPday[,4:7]) # the diagonal values shoudl be the variance of each parameter
cor(CRSPday[,4:7])
pairs(CRSPday[,4:7])
# note that crsp is the return for the CRSP value-weighted index, including dividends
# they might look ellipitcal but here we care about the correlations of variables in the data

# just some testing for now with random multivariate skewed t-distribution
omega = diag(2)
omega
omega[2,1]= omega[1,2]=.9
omega
alpha = c(1,1) # here alpha represents the skewness of two variables
?rmst # multivariate skew-t distribution
x = rmst(1000, xi = c(0,0), omega, alpha, nu =3)
# here we have
# xi: mean or location parameter
# omega: scale parameter
# alpha: skewness
# nu: degree of freedom
plot(x[,1],x[,2])
# some very large observations, not happening for normal distribution???
omega[2,1]= omega[1,2]=0
x = rmst(1000, xi = c(0,0), omega, alpha, nu =3)
plot(x[,1],x[,2])
x = rmst(1000, xi = c(0,0), omega, alpha, nu =5)
plot(x[,1],x[,2])
x = rmst(1000, xi = c(0,0), omega, alpha, nu =10)
plot(x[,1],x[,2])
# for t-distribution, as nu gets smaller, the weights of the tails increase.
# here we have two variables, not univariate distribution
# using these graphs I do not think we can conclude which one would affect their tails more
# we need to plot a single variable's desnsity graph 

?mst.mple
# mst refers to a multivariate skew-t distribution
# mple refers to maximum penalized likelihood estimation (add/substract with a function of the estimated parameter)
# but here since there is no mst.mle, so we only need to use mle or mple without penalty
fit1 = mst.mple(y =CRSPday[,4:7], penalty = NULL)
fit1
#there are about 19 parameters
# 4 location parameters
# 4+3+2+1 = 10 in scale parameters
# 4 alphas
# and 1 parameter nu of degree of freedom

# the alphas are pretty significant (much different from zeros), so they are skewed??? somewhat?
plot(density(CRSPday[,4]))
plot(density(CRSPday[,5]))
plot(density(CRSPday[,6]))
plot(density(CRSPday[,7]))

# let's try to fit a multivariate t-distribution
# so far we don't have a multivariate t-distribution function, so let me build one
dat = CRSPday[,4:7]
df = seq(5.25, 6.75, 0.01)
# from each nu, I try to estimate other parameters
n=length(df)
loglik = rep(0,n)
library(mnormt)
library(MASS)
?dmt # probability desnity function for the multivariate Student's t distribution
?cov.trob # covariance estimation for multivariate t-distribution

for (i in 1:n){
  fit = cov.trob(dat, nu =df[i]) # try to fit the covariance matrix
  loglik[i] = sum(log(dmt(dat, mean=fit$center, S=fit$cov, df =df[i])))
}
aic.t = -2*loglik + 2* (4+10) #because there are no alphas and here we have a fixed nu.
plot(df, aic.t)
df[which.min(aic.t)]

# another way is
plot(df, loglik)
df[which.max(loglik)]

cov.trob(dat, nu=5.91)

#let's build a function to test to find another way to get the best degree of freedom
f = function(df){
  fit = cov.trob(dat, nu=df)
  L = -sum(log(dmt(dat, mean=fit$center, S=fit$cov, df)))
  return(L)
}
?nlm() # non-linear minimization using a Newton-type algorithm
nlm(f,6)
# the estimated is pretty close
# what if we give a different start number
nlm(f,7)
nlm(f,4)
# still pretty good.
# when I try df =3, it produces NaNs
# nu is a shift parameter, which is hard to measure
