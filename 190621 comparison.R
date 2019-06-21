#preliminaries --------------------------------------------------------------
library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl)  
library(quantmod)


#objective:
#comparison 
#compare estimates of rugarch package and doing GARCH estimation "by hand"

#GARCH(1,1) simulation------------------------------------------------------------------------------------
n = 500 #time periods
a0 = 1
a1 = .20
b1 = .80
eps = rnorm(n,0,1)
ret = double()
len_aux1 = 1
sig2 <- c(5)
for (t in 2:n) {
  sig2[t] = a0 + a1*eps[t-1]^2 + b1*sig2[t-1]  
}
plot(sig2, type='l')
ret = sig2^.5*eps
plot(ret,type='l')
#likelihood applied to ARCH(2
sum(-log((sig2)^.5)-.5*(ret^2)/(sig2))
#
#maximum likelihood estimation of simulated GARCH(1,1) -------------------------------
parameters <- c(a0, a1, b1)
likefunc <- function(parameters){
  a0 <- parameters[1]
  a1 <- parameters[2]
  b1 <- parameters[3]
  sig2 <- c(1)
  epsv2 <- c(0)
  for (t in 2:n) {
    epsv2[t-1] = ret[t-1]/sig2[t-1]^.5
    sig2[t] = a0 + a1*epsv2[t-1]^2 + b1*sig2[t-1]  
  }
  -1*sum(-log((sig2)^.5)-.5*(ret^2)/(sig2))
}
likefunc(parameters)

inicparm <- c(0.1,0.1,0.1) #for thr optimx function this is the starting values
likefunc(inicparm)
optimx(inicparm,likefunc,lower=-Inf, upper=Inf, method=c("Nelder-Mead","BFGS"))
optim(inicparm,likefunc,
      method = c("Nelder-Mead"))
optim(inicparm,likefunc,
      method = c("SANN"))
optim(inicparm,likefunc,
      method = c("CG")) #
optim(inicparm,likefunc,
      method = c("L-BFGS-B"))
optim(inicparm,likefunc,
      method = c("BFGS"))

#rugarch package --------------------------------------------------------------
spec1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model = list(armaOrder=c(0,0)),
                  distribution.model = "std")
garch1 <- ugarchfit(spec = spec1, data = ret)
garch1

#Optimal Parameters
#  Estimate  Std. Error  t value Pr(>|t|)
#mu     -0.106790    0.099332  -1.0751 0.282335
#omega   0.063271    0.025246   2.5062 0.012204
#alpha1  0.015337    0.007694   1.9934 0.046214
#beta1   0.971543    0.008285 117.2640 0.000000
#shape  36.519380   44.759364   0.8159 0.414555

#
#finis -----------------------------------------------------------------------------------















