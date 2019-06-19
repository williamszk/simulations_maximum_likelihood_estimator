#190619 AR1 simulation and estimation
# preliminaries --------------------------------------------------------------

library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl)  

#AR(1) simulation------------------------------------------------------------------------------------

n = 10000 #time periods

phi0 = 1
phi1 = .6
sigma_u = 1


epsolon = rnorm(n,0,sigma_u)

y = double()

y[1] = 3 #initial value

for (t in 2:n) {
  y[t] = phi0 + phi1*y[t-1] + epsolon[t]
}

plot(y,type='l')

#likelihood applied to AR(1)-----------------------------------------

Likelihood <- 0
phi0_lik <- phi0
phi1_lik <- phi1
sigma_u_lik <- sigma_u
aux_like1 <- double()
range_sigma_u <- seq(.1,5,by=.1)
for (t in 2:n) {
  Likelihood  <- Likelihood  -log(sigma_u_lik) - .5*((y[t]-(phi0_lik+phi1_lik*y[t-1]))^2/(sigma_u_lik^2))
}
Likelihood

#graph of likelihood function against values of sigma_u ---------------------
phi0_lik <- phi0
phi1_lik <- phi1
for ( sigma_u_lik in range_sigma_u) {
  Likelihood <- 0
  for (t in 2:n) {
    Likelihood  <- Likelihood  -log(sigma_u_lik) - 
      .5*((y[t]-(phi0_lik + phi1_lik * y[t-1]))^2/(sigma_u_lik^2))
  }
  aux_like1 <- append(aux_like1, Likelihood)
}
plot(range_sigma_u[6:50], aux_like1[6:50], type='l')
max(aux_like1)

#graph of likelihood function against values of  phi0_lik --------------------------
range_phi0_lik <- seq(0,5,by=.1)
phi1_lik <- phi1
sigma_u_lik <- sigma_u

for ( phi0_lik in range_phi0_lik) {
  Likelihood <- 0
  for (t in 2:n) {
    Likelihood  <- Likelihood  -log(sigma_u_lik) - 
      .5*((y[t]-(phi0_lik + phi1_lik * y[t-1]))^2/(sigma_u_lik^2))
  }
  aux_like1 <- append(aux_like1, Likelihood)
}
plot(range_phi0_lik[3:50], aux_like1[3:50], type='l')
max(aux_like1)

#graph of likelihood function against values of phi1_lik --------------------------
range_phi1_lik <- seq(0,3,by=.05)
phi0_lik <- phi0
sigma_u_lik <- sigma_u

for ( phi1_lik in range_phi1_lik) {
  Likelihood <- 0
  for (t in 2:n) {
    Likelihood  <- Likelihood  -log(sigma_u_lik) - 
      .5*((y[t]-(phi0_lik + phi1_lik * y[t-1]))^2/(sigma_u_lik^2))
  }
  aux_like1 <- append(aux_like1, Likelihood)
}
plot(range_phi1_lik[3:50], aux_like1[3:50], type='l')
max(aux_like1)


#graph 3 dimensional of phi0 and phi1--------------------------
range_phi0_lik <- seq(.3,3,by=.1)
range_phi1_lik <- seq(.2,3,by=.05)

sigma_u_lik <- sigma_u
phi0_lik <- phi0
phi1_lik <- phi1


Like_func <- function(phi0_lik,phi1_lik){
  Likelihood <- 0
  for (t in 2:n) {
    Likelihood  <- Likelihood -log(sigma_u_lik) - 
      .5*((y[t]-(phi0_lik + phi1_lik * y[t-1]))^2/(sigma_u_lik^2))
  }
  Likelihood
}

Like_func(phi0_lik,phi1_lik)

z <- outer(range_phi0_lik, range_phi1_lik, Like_func)
dim(z)

persp(range_phi0_lik, range_phi1_lik,z, theta=120, phi=20 )

#surface3d(range_phi0_lik, range_phi1_lik, z, color='red') #red

#persp(x = seq(0, 1, length.out = nrow(z)),
#      y = seq(0, 1, length.out = ncol(z)),
#      z, xlim = range(x), ylim = range(y),
#      zlim = range(z, na.rm = TRUE),
#      xlab = NULL, ylab = NULL, zlab = NULL,
#      main = NULL, sub = NULL,
#      theta = 0, phi = 15, r = sqrt(3), d = 1,
#      scale = TRUE, expand = 1,
#      col = "white", border = NULL, ltheta = -135, lphi = 0,
#      shade = NA, box = TRUE, axes = TRUE, nticks = 5,
#      ticktype = "simple", ...)


#an alternative function for likelihood AR(1) -------------------------------


#a function for the maximum likelihood
Like_func <- function(phi0_lik=phi0,phi1_lik=phi1,sigma_u_lik=sigma_u){
  Likelihood <- 0
  for (t in 2:n) {
    Likelihood  <- Likelihood -log(sigma_u_lik) - 
      .5*((y[t]-(phi0_lik + phi1_lik * y[t-1]))^2/(sigma_u_lik^2))
  }
  Likelihood
}
Like_func()

#the following function does not use loops
#another function for the maximum likelihood of AR(1)

ylag = y[-n]
ylev = y[-1]
like_func2 <- function(phi0_lik, phi1_lik, sigma_u_lik){
  sum(-log(sigma_u_lik) - .5*((ylev-(phi0_lik + phi1_lik * ylag))^2/(sigma_u_lik^2)))
}

sigma_u_lik <- sigma_u
phi0_lik <- phi0
phi1_lik <- phi1

like_func2(phi0_lik, phi1_lik, sigma_u_lik)



#maximum likelihood estimation of simulated AR(1) -------------------------------

parameters <- c(phi0_lik, phi1_lik, sigma_u_lik)

ylag = y[-n]
ylev = y[-1]

like_func3 <- function(parameters){
  phi0_lik <- parameters[1]
  phi1_lik <- parameters[2]
  sigma_u_lik <- parameters[3]
  -1*(sum(-log(sigma_u_lik) - .5*((ylev-(phi0_lik + phi1_lik * ylag))^2/(sigma_u_lik^2))))
}

like_func3(parameters)

parameters <- c(10,1,10) #for thr optimx function this is the starting values

optimx(parameters,like_func3,lower=-Inf, upper=Inf, method=c("Nelder-Mead","BFGS"))

#it worked well

#optimx(par, fn, gr=NULL, hess=NULL, lower=-Inf, upper=Inf, 
#       method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
#       control=list(),
#       ...)


optim(parameters,like_func3,
      method = c("Nelder-Mead"))

optim(parameters,like_func3,
      method = c("SANN"))

optim(parameters,like_func3,
      method = c("CG"))

optim(parameters,like_func3,
      method = c("L-BFGS-B"))

optim(parameters,like_func3,
      method = c("BFGS"))

optim(parameters,like_func3,
      method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent"))


#optim(par, fn, gr = NULL, ...,
#      method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
#                 "Brent"),
#      lower = -Inf, upper = Inf,
#      control = list(), hessian = FALSE)
#







#finis -----------------------------------------------------------------------------------















