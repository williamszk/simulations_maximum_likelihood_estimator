#190530 script

library(rgl)

#simulate numerical optimization --------------------------------------

#functions that we can use to test our algorithms

#univariate
f = function(x){log(1+abs(x)^(2+sin(x)))}
x = seq(-5,5,by=.1)
y = f(x)
plot(x,y,type='l')

#another function
f = function(x){(2+(sin(50*x)/20))*(atan(x))^2 }
x = seq(-5,5,by=.1)
y = f(x)
plot(x,y,type='l')
plot(x,y)

#multivariate functions ---------------------------------------------------
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

# another function ------------------------------------------
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




#another function -------------------------------------------------------
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


#another function -------------------------------------------------------
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

#simplex algorithm
#Danzig
#for linear programming with restrictions

#Apply an algorithm for Nelder-Mead
#then look for one algorithm in a package

#https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method

#other names for NM method: downhill simplex method, amoeba method, or polytope method

#The simplest approach is to replace the worst point with a point reflected 
#through the centroid of the remaining n points. 


# the univariate nelder meand algorithm -----------------------------------------------------------------------------------

#determine Nelder-Mead parameters of optimization


#the hand made Nelder-Mead optimizer function
optimizer_NM_univarite = function(function_evaluate) {
  f = function_evaluate #function to be evaluated
  alpha = 1 
  gamma = 2
  rho = .5
  tolerance = 0.0000001 #tolerance epsolon
  range1 = 50
  x1 = runif(1,-range1,range1) #an initial x
  x2 = x1+rnorm(1) #a second x as initial point
  #x1;x2
  #f(x1);f(x2)
  steps = 1
  craker=0
  while(craker==0){
    value1 = f(x1)>=f(x2) #; value1
    #paste('x1',x1,sep='=');paste('f(x1)',f(x1),sep='=');paste('x2',x2,sep='=');paste('f(x2)',f(x2),sep='=')
    if (value1){
      x1_1 = x1
      x2_2 = x2
      x1 = x2_2
      x2 = x1_1
    }
    #calculate the centroid of the problem
    #compute the reflected point
    xr = x1 + alpha * (x1 - x2)
    
    value2 = f(xr) <= f(x1) #; value2
    value4 = f(x1)<=f(x1) #; value4
    
    if (value2) { #if reflected point is better than x1, which is the best point
      #compute the expanded point
      xe = x1 + gamma * (xr - x1)
      value2_1 = f(xe) <= f(xr) #; value2_1
      value2_2 = f(xe) > f(xr) #; value2_2
      if (value2_1) {
        x2 = xe
      }
      if (value2_2) {
        x2 = xr
      }
    }  
    if(value4){ #if  f(x1)<=f(x1)
      #calculate contracted point xc
      xc = x1 + rho*(x2-x1)
      value4_1 = f(xc)<f(x2) #; value4_1
      if (value4_1) {
        x2 = xc
      }
    }
    
    value5 = abs(f(x1)-f(x2)) #; value5
    value5_bool = value5 < tolerance #; value5_bool
    if (value5_bool) {
      craker = 1
    }
    steps = steps + 1
  }
  aux_df1 = data.frame(x1, f(x1),steps)
  names(aux_df1) = c('optimal x','lowest value','number of steps')
  aux_df1
}


#tests with the optimization function...:
f = function(x){(x-pi)^2} #the absolute minimum lies in pi=3.1415
x = seq(-10,10,by=.1);y = f(x);plot(x,y,type='l') #to visualize the graph

optimizer_NM_univarite(f) #use the function
#it is working


#a harder function to test
f = function(x){log(1+abs(x)^(2+sin(x)))}
range1 = 50
x = seq(-range1,range1,by=.1)
y = f(x)
plot(x,y,type='l')

optimizer_NM_univarite(f)

#some comments on how we can improve the estimation:
#in cases where we have many local minima, we can apply the algorithm many times
#and find in this vector which is the lowest value. 

# another harder function to optimize
f = function(x){(2+(sin(50*x)/20))*(atan(x))^2 }
range1 = 50
x = seq(-range1,range1,by=.1)
y = f(x)
plot(x,y,type='l')
optimizer_NM_univarite(f)
#for this function the algorithm takes a long time...



#build multivariate nelder mead algorithm --------------------------------------------------

#MultiGaussian Distribution
x1 = 0
x2 = 0
x = matrix(c(x1,x2)) 
#a specific value to be evaluated in the f function
k = length(x)
#no correlation between the variables, and standar deviation as 1
Sigma = diag(rep(1,k), nrow=k)  #covariance matrix
#Sigma = diag(c(1,2), nrow=k)   #covariance matrix
#Sigma = matrix(c(1,.4,.4,1),nrow=2) #covariance matrix
mu = matrix(c(0,0))
f = function(x){
  -1* (2*pi)^(-k/2)*det(Sigma)^(.5)*exp(-.5*t(x-mu)%*%solve(Sigma)%*%(x-mu))
}
f(x)

seq1 = seq(-2,2,by=.1)
x1 = seq1
x2 = seq1
len_1 = length(x1)
len_2 = length(x2)
aux_matrix1 = matrix(rep(0,len_1*len_2),ncol=len_2)
for (ii in 1:len_1){
  aux_x1 = x1[ii]
  for (jj in 1:len_2){
    aux_x2 = x2[jj]
    x = matrix(c(aux_x1,aux_x2))
    aux_matrix1[ii,jj] = f(x)
  }
}
z = aux_matrix1
x1_1 = x1/20
x2_1 = x2/20
#open3d()
surface3d(x1_1, x2_1, z, color='red',back = "lines")

#build algorithm

optimizer_NM_multivariate = function(f) {
  alpha = 1 
  gamma = 2
  rho = .5
  tolerance = 1e-10 #tolerance epsolon
  range1 = 2
  
  x1 = matrix(runif(k,-range1,range1))#an initial x
  aux_matrix1 = x1+rnorm(k)
  for (kk in 1:(k-1)) { #create matrix of triangulation points
    aux_matrix1 = cbind(aux_matrix1,x1+rnorm(k))
  }
  x = cbind(x1,aux_matrix1)

  steps = 1
  craker=0
  while(craker==0){
    #order the vectors
    
    f_values = double()
    for (kk in 1:(k+1)) { #create auxiliary vector 
      f_values[kk] = as.numeric(f(x[,kk]))
    }
    aux_order1 = order(f_values)
    matrix_x2 = x[,aux_order1] #colocar vetores em ordem
    x = matrix_x2
    
    #calculate the centroid of the problem x0
    aux_centroid = double()
    for (kk in 1:k) {
      aux_centroid[kk] = mean(x[kk,])
    }
    x0 = matrix(aux_centroid)
    
    #compute the reflected point
    xr = x0 + alpha * (x0 - x[,k+1])
    
    value1 = f(x[,1]) <= f(xr)  & f(xr) <= f(x[,k]) #reflected point is between the best and worst
    value2 = f(xr) <= f(x[,1]) #reflected point is the best
    value3 = f(x[,k]) <= f(xr) 
    
    if (value1) {
      x[,k+1] = xr
    }
    if (value2) { #if reflected point is better than x1, which is the best point
      #compute the expanded point
      xe = x0 + gamma * (xr - x0)
      value2_1 = f(xe) <= f(xr) #; value2_1
      value2_2 = f(xe) > f(xr) #; value2_2
      if (value2_1) {
        x[,k+1] = xe
      }
      if (value2_2) {
        x[,k+1] = xr
      }
    }  
    if(value3){ #if  f(x1)<f(xr)
      xc = x0 + rho*(x[,k+1]-x0) #calculate contracted point xc
      value3_1 = f(xc)<f(x[,k+1]) # contracted point is the best now
      if (value3_1) {
        x[,k+1] = xc
      }
    }
    value5 = abs(f(x[,1])-f(x[,2])) #; value5
    value5_bool = value5 < tolerance #; value5_bool
    if (value5_bool) {
      craker = 1
    }
    steps = steps + 1
    #print(as.numeric(f(x[,1]))  )
  }
  output = list(x[,1],f(x[,1]),steps)
  names(output) = c('optimal x','lowest value','steps')
  output
}

optimizer_NM_multivariate(f) #test the optimizer function






#end ---------------------------------------------------
