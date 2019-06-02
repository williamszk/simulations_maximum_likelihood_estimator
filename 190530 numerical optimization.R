#190530 script

#Maximum likelihood estimation
#https://en.wikipedia.org/wiki/Maximum_likelihood_estimation

#Multivariate Gaussian distributions
#https://www.youtube.com/watch?v=eho8xH3E6mE

#objective: simulate numerical optimization
#objective: simulate data and estimate using maximum likelihood


#simulate numerical optimization --------------------------------------

f = function(x,y){ x^2 + y^2 + x*sin(y) + y * sin(x) }
seq1 = seq(-5,5,by=.1)
x = seq1
y = seq1
z = f(x,y)

len_x = length(x)
len_y = length(y)

aux_matrix1 = matrix(rep(0,len_x*len_y),ncol=len_y)
for (ii in 1:len_x){
  aux_x1 = x[ii]
  for (jj in 1:len_y){
    aux_y1 = y[jj]
    aux_matrix1[ii,jj] = f(aux_x1,aux_y1)
  }
}
z = aux_matrix1

x = x*10
y = y*10

open3d()
surface3d(x, y, z, color='red',back = "lines")

# another function 
f = function(x,y){(x^2 + y - 11)^2 + (x + y^2 - 7)^2  }
seq1 = seq(-10,10,by=.1)
x = seq1
y = seq1
z = f(x,y)

len_x = length(x)
len_y = length(y)

aux_matrix1 = matrix(rep(0,len_x*len_y),ncol=len_y)
for (ii in 1:len_x){
  aux_x1 = x[ii]
  for (jj in 1:len_y){
    aux_y1 = y[jj]
    aux_matrix1[ii,jj] = f(aux_x1,aux_y1)
  }
}
z = aux_matrix1

x1 = x*1000
y1 = y*1000
#open3d()
surface3d(x1, y1, z, color='red',back = "lines")


#another function 
f = function(x,y){ (1-x)^2 + 100*(y-x^2)^2}
seq1 = seq(-5,5,by=.1)
x = seq1
y = seq1
z = f(x,y)

len_x = length(x)
len_y = length(y)

aux_matrix1 = matrix(rep(0,len_x*len_y),ncol=len_y)
for (ii in 1:len_x){
  aux_x1 = x[ii]
  for (jj in 1:len_y){
    aux_y1 = y[jj]
    aux_matrix1[ii,jj] = f(aux_x1,aux_y1)
  }
}
z = aux_matrix1

x1 = x*5000
y1 = y*5000
#open3d()
surface3d(x1, y1, z, color='red',back = "lines")


#another function
f = function(x,y){.26*(x^2+y^2)+.48*x*y}
seq1 = seq(-5,5,by=.1)
x = seq1
y = seq1
z = f(x,y)
len_x = length(x)
len_y = length(y)
aux_matrix1 = matrix(rep(0,len_x*len_y),ncol=len_y)
for (ii in 1:len_x){
  aux_x1 = x[ii]
  for (jj in 1:len_y){
    aux_y1 = y[jj]
    aux_matrix1[ii,jj] = f(aux_x1,aux_y1)
  }
}
z = aux_matrix1
x1 = x*2
y1 = y*2
#open3d()
surface3d(x1, y1, z, color='red',back = "lines")



#by hand, do not use functions nor packages. ------------------------------------



#gradient descent ---------------------------------------------------------------
#apply an gradient descent, is it the same as newton raphson?






#gradient descent and line search -----------------------------------------------
#create an algorithm for gradient descent with line search






#conjugate gradient ------------------------------------------------------------
#build an algorithm of conjugate gradient






#simulated anneling ------------------------------------------------------------
#https://en.wikipedia.org/wiki/Simulated_annealing
#simulated anneling is good for situation where we have many local optima








