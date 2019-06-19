#script 1
# preliminaries --------------------------------------------------------------

library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl)  


#simulate a GARCH model -----------------------------------------------------

n = 10000 #number of periods
set.seed(123)
z = rnorm(n)
sig = double()
sig[1] = .5 #initial value for sigma series
omega = 0.005
alpha1 = .05
beta1 = .95
for (t in 2:n) {
  sig[t] = omega + (alpha1 * z[t-1]^2 + beta1) * sig[t-1]  
}
plot(sig,type='l')
#true series of sigma
#create the observed returns
ret = sig*z
plot(ret,type='l')

#apply rugarch package to solve this simulated series --------------------------------
specification1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model = list(armaOrder=c(0,0)),
                  distribution.model = "std")
model1 <- ugarchfit(spec = specification1, data = ret)


model1Predict <- ugarchboot(model1,n.ahead = 10, method = c("Partial","Full")[1])
plot(model1Predict,which=2)


#apply algorithm of maximum likelihood -----------------------------------------------------
x = c(omega,alpha1,beta1)

omega = 10
alpha1 = .4
beta1 = .5

garch = function(x){
  omege = x[1]
  alpha1 = x[2]
  beta1 = x[3]
  
  sig=double()
  sig[1] = mean(ret)^2
  for (t in 2:length(ret)) {
    sig[t] = omega+alpha1*ret[t-1]^2+beta1*sig[t-1]  
  }
  likelihood = sum(-log(sig^.5)-.5*(ret^2/sig))
  likelihood
}
#-196.8327 true likelihood
#ARMA simulation ---------------------------------------------------------------------------

#AR(2) ------------------------------------------------------------------------------------------

n = 100 #time periods

phi0 = 1
phi1 = .4
phi2 = .3

epsolon = rnorm(n)

y = double()

y[1] = 3 #initial value
y[2] = 2


for (t in 3:n) {
  y[t] = phi0 + phi1*y[t-1] + phi2*y[t-2] + epsolon[t]
}

plot(y,type='l')


#AR(3) ------------------------------------------------------------------------------------------

n = 100 #time periods

phi0 = 1
phi1 = .4
phi2 = .3
phi3 = .2

epsolon = rnorm(n)

y = double()

y[1] = 3 #initial value
y[2] = 2
y[3] = 2


for (t in 4:n) {
  y[t] = phi0 + phi1*y[t-1] + phi2*y[t-2] + phi3*y[t-3] + epsolon[t]
}

plot(y,type='l')

#MA(1) ------------------------------------------------------------------------------------------

n = 100 #time periods

phi0 = 1
theta1 = .6

epsolon = rnorm(n)

y = double()

for (t in 1:n) {
  y[t] = phi0 + epsolon[t] + theta1*epsolon[t-1]
}

plot(y,type='l')

#MA(2) ------------------------------------------------------------------------------------------

n = 100 #time periods

phi0 = 1
theta1 = .6
theta2 = .5

epsolon = rnorm(n)

y = double()

for (t in 1:n) {
  y[t] = phi0 + epsolon[t] + theta1*epsolon[t-1] + theta2*epsolon[t-2]
}

plot(y,type='l')

#MA(3) ------------------------------------------------------------------------------------------

n = 100 #time periods

phi0 = 1
theta1 = .6
theta2 = .5
theta3 = .4

epsolon = rnorm(n)

y = double()

for (t in 1:n) {
  y[t] = phi0 + epsolon[t] + theta1*epsolon[t-1] + theta2*epsolon[t-2] + theta3*epsolon[t-3]
}

plot(y,type='l')



























#ARCH(1)  -----------------------------------------------------------------------------------

n <- 1000
set.seed(123)
error <- rnorm(n)

a0 <- 0.01
a1 <- 0.8

sigma <- double()

for (t in 2:n) {
  sigma[t] = a0 + a1*error[t-1]^2  
}

plot(sigma, type = 'l')

ret = sigma*error
ret <- ret[-1]
plot(ret, type = 'l')

#use rugarch package to simulate data for a ARCH(1) model)  -----------------------------------

#ugarchsim(fit, n.sim = 1000, n.start = 0, m.sim = 1,
#          startMethod = c("unconditional", "sample"), presigma = NA, prereturns = NA,
#          preresiduals = NA, rseed = NA, custom.dist = list(name = NA, distfit = NA),
#          mexsimdata = NULL, vexsimdata = NULL, ...)

specification1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,0)),
                             mean.model = list(armaOrder=c(0,0)),
                             distribution.model = "std")

model1 <- ugarchfit(spec = specification1, data = ret)

archSim1 <- ugarchsim(fit=model1, n.sim = n, n.start = 0, m.sim = 1,
          startMethod = c("unconditional"))
class(archSim1)
slotNames(archSim1)
sim1 <- archSim1@simulation
sim1 %>% class
sim1 %>% length
sim1[[1]] %>% class
sim1[[1]] %>% dim
sim2 <- as.numeric(sim1[[1]])
plot(sim2,type='l')
sim3 = as.numeric(sim1[[2]])
plot(sim3,type='l')
sim1[[3]]
names(sim1)
aux_sim1 = as.numeric(sim1[[3]])  
plot(aux_sim1,type='l')

identical(sim3, aux_sim1)
sim3 %>% head
aux_sim1 %>% head

model2 = archSim1@model
model2 %>% class
length(model2)
model2[[1]]
model2[[2]]



#example uGARCHfit from ugarch package --------------------------------------------------------
## Not run:
# Basic GARCH(1,1) Spec
data(dmbp)
class(dmbp)
plot(dmbp$V1,type='l')
spec = ugarchspec()
fit = ugarchfit(data = dmbp[,1], spec = spec)
fit
# object fit:
slotNames(fit)
# sublist fit@fit
names(fit@fit)
coef(fit)
infocriteria(fit)
likelihood(fit)
nyblom(fit)
signbias(fit)
head(sigma(fit))
head(residuals(fit))
head(fitted(fit))
gof(fit,c(20,30,40,50))
uncmean(fit)
uncvariance(fit)
#plot(fit,which="all")
# news impact example
spec = ugarchspec(variance.model=list(model="apARCH"))
fit = ugarchfit(data = dmbp[,1], spec = spec)
# note that newsimpact does not require the residuals (z) as it
# will discover the relevant range to plot against by using the min/max
# of the fitted residuals.
ni=newsimpact(z = NULL, fit)
#plot(ni$zx, ni$zy, ylab=ni$yexpr, xlab=ni$xexpr, type="l", main = "News Impact Curve")
## End(Not run)

#







#finis -----------------------------------------------------------------------------------















