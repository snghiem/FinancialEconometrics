# chapter 5 - R Lab
# 5.20.1 Earnings Data

library("Ecdat")
?CPSch3
# So this data is about earnings from the Current Population Survey
data(CPSch3)
dimnames(CPSch3)[[2]]

male.earnings = CPSch3[CPSch3[,3]=="male",2]
sqrt.male.earnings = sqrt(male.earnings)
log.male.earnings = log(male.earnings)

par(mfrow =c(2,2))
qqnorm(male.earnings, datax =T, main ="untransformed")
# datax: logical. Should data values be on the x-axis?
# main: refer to the y and x axes respectively if datax = TRUE.
qqnorm(sqrt.male.earnings, datax = T, main="square-root transformed")
qqnorm(log.male.earnings, datax = T, main="log-transformed")

# A normal Q-Q plot comparing randomly generated, 
# independent standard normal data on the vertical axis to 
# a standard normal population on the horizontal axis. 
# The linearity of the points suggests that the data are 
# normally distributed.

par(mfrow=c(2,2))
boxplot(male.earnings, main="untransformed")
boxplot(sqrt.male.earnings, main="square-root transformed")
boxplot(log.male.earnings, main ="log-transformed")

par(mfrow=c(2,2))
plot(density(male.earnings), main="untransformed")
plot(density(sqrt.male.earnings), main = "square-root transformed")
plot(density(log.male.earnings), main = "log-transformed")

# Obviously the square-root transformation provides the most sympmetric distribution.
library(MASS)
windows()
par(mfrow=c(1,1))
boxcox(male.earnings~1)
?boxcox
# Computes and optionally plots profile log-likelihoods 
# for the parameter of the Box-Cox power transformation.
boxcox(male.earnings~1,lambda = seq(.3, .45, 1/100))

# to find the MLE, we have
bc = boxcox(male.earnings~1, lambda = seq(.3, .45, by=1/100), interp=F)
ind = (bc$y==max(bc$y))
bc
ind2 = (bc$y >max(bc$y) -qchisq(.95, df=1)/2)
ind2
bc$x[ind]
bc$y[ind2]
bc$x[ind2]
?qchisq # The (non-central) Chi-Squared Distribution

#Problem 2
#(a) ind is to mark the index of the highest log-likelihood value of male.earnings within the interval
# ind2 marks the values of lambda for its 95% confidence interval???
# (c) MLE of lambda
bc$x[ind]

# Now we try to fit a skewed Gaussian or skewed t-distribution.
library(fGarch)
fit=sstdFit(male.earnings, hessian=T)
fit
?sstdFit # Skew Student-t Distribution Parameter

#Problem 3
#mean        sd        nu        xi 
#17.322933  7.492440 21.600055  1.651652

#nu: degree of freedom
#xi: skew

#Problem 4
plot(density(male.earnings))
x = seq(min(male.earnings), max(male.earnings), length.out = 100)
y = dsstd(x, mean = fit$estimate[1], sd=fit$estimate[2], nu=fit$estimate[3], xi=fit$estimate[4])
?dsstd # skew student-t distribution and parameter estimation
lines(x,y, col="red")
legend("topright", c("KDE","skew"), col=c("black","red"), lty=c(1,1))
# compare the parametric and nonparametric estimates of the pdf.
# They look quite similar. I believe that the skewed t-model
# provides an adequate  fit to male.earnings.

fit.GED = sgedFit(male.earnings)
fit.GED
y.GED = dsged(x, mean = fit.GED$par[1], sd = fit.GED$par[2], nu=fit.GED$par[3], xi = fit.GED$par[4])
lines(x,y.GED, col="blue")
# skew seems to be better
fit$minimum
fit.GED$objective
#so fit$minimum is bigger, or its log-likelihood is bigger
# so it should fit better.

#5.20.2 DAX returns
?EuStockMarkets
head(EuStockMarkets)
EuStockMarkets
