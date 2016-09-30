library(copula)
library(sn)
library(ks)

setwd("D:/Sony/FinEcon/datasets")
dat = read.csv("GasFlowData.csv", header=T)
summary(dat)
dat=dat/10000 # just to reduce the scale of the data

n = nrow(dat)

x1=dat$Flow1
fit1 = st.mple(matrix(1,n,1), y= x1, dp=c(mean(x1), sd(x1), 0, 10))
?st.mple # fitting functions
fit1
est1 = fit1$dp
est1
u1 = pst(x1, dp=est1)
?pst # Skew-t Distribution

x2=dat$Flow2
fit2 = st.mple(matrix(1,n,1), y= x2, dp=c(mean(x2), sd(x2), 0, 10))
est2 = fit2$dp
est2
u2 = pst(x2, dp=est2)
U.hat = cbind(u1,u2)
fhatU=kde(x=U.hat,H=Hscv(x=U.hat))
plot(fhatU, cont=seq(10,80,10)) # plot contours

?cor.test # Test for Association/Correlation Between Paired Samples
cor.test(u1, u2, method = "kendall")
# p-value is really small, I can reject the null hypothesis that true tau is 0

#Now I need to fit t-copula
omega = sin(-0.234435*pi/2)
Ct = fitCopula(copula=tCopula(dim=2), data=U.hat, method="ml", start=c(omega,10))
Ct@estimate # here I need to use @ instead of $
loglikCopula(param=Ct@estimate, u=U.hat, copula=tCopula(dim=2))
AIC.t = -2*.Last.value+2*length(Ct@estimate)
AIC.t

# Now I try Gaussian copula
Cgauss = fitCopula(copula=normalCopula(dim=2), data=U.hat, method="ml", start=c(omega))
loglikCopula(param=Cgauss@estimate, u=U.hat, copula=normalCopula(dim=2))
AIC.normal = -2*.Last.value+2*length(Cgauss@estimate)
AIC.normal

# Now try Frank copula
Cfrank = fitCopula(copula=frankCopula(1,dim=2), data=U.hat, method="ml")
loglikCopula(param=Cfrank@estimate, u=U.hat, copula=frankCopula(dim=2))
AIC.frank = -2*.Last.value+2*length(Cfrank@estimate)
AIC.frank

# so the Frank copula works the best.