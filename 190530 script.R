#190530 script

#Maximum likelihood estimation
#https://en.wikipedia.org/wiki/Maximum_likelihood_estimation

#Multivariate Gaussian distributions
#https://www.youtube.com/watch?v=eho8xH3E6mE

#objective: simulate data and estimate using maximum likelihood

#simulate numerical optimization --------------------------------------
#by hand, do not use functions nor packages.

#Nelder Mead














#univariate estimation ---------------------------------------------
n = 30 #number of observations
mu = 2.342
sig = 1.5

set.seed(123)
x = rnorm(n,mu,sig)

hist(x,breaks=30)






#multivariate estimation -------------------------------------------
n = 30 #number of observations

b0 = 1.35 #parameters
b1 = 2.71

set.seed(123)

x = rnorm(n,10,3)
u = rnorm(n,0,1)

y = b0 + b1*x + u

plot(x,y)





