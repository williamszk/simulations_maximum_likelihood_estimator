# preliminaries --------------------------------------------------------------

library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl)  

#AR(2) simulation------------------------------------------------------------------------------------
n = 500 #time periods
phi0 = 1
phi1 = .6
phi2 = .15
sigma_u = 1
epsolon = rnorm(n,0,sigma_u)
y = double()
len_aux1 = 2
y[1:len_aux1] = 1 #initial value
for (t in (len_aux1+1):n) {
  y[t] = phi0 + phi1*y[t-1] + phi2*y[t-2] + epsolon[t]
}
plot(y,type='l')

#likelihood applied to AR(1)-----------------------------------------
phi0_lik <- phi0
phi1_lik <- phi1
phi2_lik <- phi2
sigma_u_lik <- sigma_u
Likelihood <- 0
for (t in (len_aux1+1):n) {
  Likelihood  <- Likelihood  - log(sigma_u_lik) - .5*((y[t]-(phi0_lik+phi1_lik*y[t-1]+phi2_lik*y[t-2]))^2/(sigma_u_lik^2))
}
Likelihood

#graph of likelihood function against values of sigma_u ---------------------
phi0_lik <- phi0
phi1_lik <- phi1
phi2_lik <- phi2
range_sigma_u <- seq(.2,4,by=.1)
aux_like1 <- double()
for ( sigma_u_lik in range_sigma_u) {
  Likelihood <- 0
  for (t in (len_aux1+1):n) {
    Likelihood  <- Likelihood  - log(sigma_u_lik) - .5*((y[t]-(phi0_lik+phi1_lik*y[t-1]+phi2_lik*y[t-2]))^2/(sigma_u_lik^2))
  }
  aux_like1 <- append(aux_like1, Likelihood)
}
plot(range_sigma_u[4:39], aux_like1[4:39], type='l')
max(aux_like1)
#
#graph of likelihood function against values of  phi0_lik --------------------------
phi1_lik <- phi1
phi2_lik <- phi2
sigma_u_lik <- sigma_u
range_phi0_lik <- seq(0,5,by=.1)
aux_like1 <- double()
for ( phi0_lik in range_phi0_lik) {
  Likelihood <- 0
  for (t in (len_aux1+1):n) {
    Likelihood  <- Likelihood  - log(sigma_u_lik) - .5*((y[t]-(phi0_lik+phi1_lik*y[t-1]+phi2_lik*y[t-2]))^2/(sigma_u_lik^2))
  }
  aux_like1 <- append(aux_like1, Likelihood)
}
plot(range_phi0_lik[1:50], aux_like1[1:50], type='l')
max(aux_like1)
#
#graph of likelihood function against values of phi1_lik --------------------------
phi0_lik <- phi0
phi2_lik <- phi2
sigma_u_lik <- sigma_u
range_phi1_lik <- seq(0,3,by=.05)
aux_like1 <- double()
for ( phi1_lik in range_phi1_lik) {
  Likelihood <- 0
  for (t in (len_aux1+1):n) {
    Likelihood  <- Likelihood  - log(sigma_u_lik) - .5*((y[t]-(phi0_lik+phi1_lik*y[t-1]+phi2_lik*y[t-2]))^2/(sigma_u_lik^2))
  }
  aux_like1 <- append(aux_like1, Likelihood)
}
plot(range_phi1_lik[3:50], aux_like1[3:50], type='l')
max(aux_like1)

#graph 3 dimensional graph--------------------------
range_phi0_lik <- seq(.3,3,by=.05)
range_phi1_lik <- seq(.2,3,by=.05)
range_phi2_lik <- seq(.2,3,by=.05)
sigma_u_lik <- sigma_u
phi0_lik <- phi0
phi1_lik <- phi1
phi2_lik <- phi2
Like_func <- function(phi1_lik,phi2_lik){
  Likelihood <- 0
  for (t in (len_aux1+1):n) {
    Likelihood  <- Likelihood  - log(sigma_u_lik) - .5*((y[t]-(phi0_lik+phi1_lik*y[t-1]+phi2_lik*y[t-2]))^2/(sigma_u_lik^2))
  }
  Likelihood
}
Like_func(phi1_lik,phi2_lik)
z <- outer(range_phi1_lik, range_phi2_lik, Like_func)
dim(z)
persp(range_phi1_lik, range_phi2_lik,z, theta=120, phi=20 )

#an alternative function for likelihood AR(1) -------------------------------
#the following function does not use loops
#another function for the maximum likelihood of AR(2)
ylev = y[-(1:2)]
ylag = y[c(-1,-n)]
ylag2 = y[-(n:(n-1))]

like_func2 <- function(phi0_lik, phi1_lik, phi2_lik, sigma_u_lik){
  sum(- log(sigma_u_lik) - .5*((ylev-(phi0_lik+phi1_lik*ylag+phi2_lik*ylag2))^2/(sigma_u_lik^2)))
}
sigma_u_lik <- sigma_u
phi0_lik <- phi0
phi1_lik <- phi1
phi2_lik <- phi2
like_func2(phi0_lik, phi1_lik,phi2_lik, sigma_u_lik)

#
#maximum likelihood estimation of simulated AR(1) -------------------------------
parameters <- c(phi0_lik, phi1_lik,phi2_lik, sigma_u_lik)
ylev = y[-(1:2)]
ylag = y[c(-1,-n)]
ylag2 = y[-(n:(n-1))]
like_func3 <- function(parameters){
  phi0_lik <- parameters[1]
  phi1_lik <- parameters[2]
  phi2_lik <- parameters[3]
  sigma_u_lik <- parameters[4]
  -1*sum(- log(sigma_u_lik) - .5*((ylev-(phi0_lik+phi1_lik*ylag+phi2_lik*ylag2))^2/(sigma_u_lik^2)))
}

like_func3(parameters)
inic_parm <- c(10,1,10,10) #for thr optimx function this is the starting values
optimx(inic_parm,like_func3,lower=-Inf, upper=Inf, method=c("Nelder-Mead","BFGS"))

#it worked well

#optimx(par, fn, gr=NULL, hess=NULL, lower=-Inf, upper=Inf, 
#       method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
#       control=list(),
#       ...)


optim(inic_parm,like_func3,
      method = c("Nelder-Mead"))
optim(inic_parm,like_func3,
      method = c("SANN"))
optim(inic_parm,like_func3,
      method = c("CG")) #
optim(inic_parm,like_func3,
      method = c("L-BFGS-B"))
optim(inic_parm,like_func3,
      method = c("BFGS"))

#list of possible methods
c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")

#
#finis -----------------------------------------------------------------------------------















