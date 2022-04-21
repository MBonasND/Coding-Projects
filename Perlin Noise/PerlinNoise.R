###############################
###############################
### Perlin Noise Simulation ###
###############################
###############################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)
library(ambient)



#############################
### Perlin Noise Function ###
#############################

#this code was translated from Python code
#Python code obtained from: https://rosettacode.org/wiki/Perlin_noise

perlin = function(x, y, z, p)
{
  #interpolation functions
  fade = function(t)
  {
    return(t^3 * (t*(t*6 - 15) + 10))
  }
  
  lerp = function(t, a, b)
  {
    return(a + t*(b - a))
  }
  
  #gradient functions
  grad = function(hash, x, y, z)
  {
    h = bitwAnd(hash, 15)
    if(h < 8)
    {
      u = x
    } else {
      u = y
    }
    
    if(h < 4)
    {
      v = y
    } else if(h %in% c(12,14)) {
      v = x
    } else {
      v = z
    }
    
    if(bitwAnd(h, 1) == 0){
      result_u = u
    } else {
      result_u = -u
    }
    
    if(bitwAnd(h, 2) == 0){
      result_v = v
    } else {
      result_v = -v
    }
    
    return(result_u + result_v)
    
  }
  
  #Generate noise
  X = bitwAnd(floor(x), 255)                
  Y = bitwAnd(floor(y), 255)                
  Z = bitwAnd(floor(z), 255)
  x = x - floor(x)                               
  y = y - floor(y)                             
  z = z - floor(z)
  u = fade(x)                                
  v = fade(y)                                
  w = fade(z)
  A = p[X+1]+Y; AA = p[A+1]+Z; AB = p[A+2]+Z      
  B = p[X+2]+Y; BA = p[B+1]+Z; BB = p[B+2]+Z      
  
  return (lerp(w, lerp(v, lerp(u, grad(p[AA+1], x  , y  , z),
                              grad(p[BA+1], x-1, y  , z)),
                      lerp(u, grad(p[AB+1], x  , y-1, z),
                           grad(p[BB+1], x-1, y-1, z))),
              lerp(v, lerp(u, grad(p[AA+2], x, y, z-1),
                           grad(p[BA+2], x-1, y, z-1)),
                   lerp(u, grad(p[AB+2], x, y-1, z-1),
                        grad(p[BB+2], x-1, y-1, z-1)))))
  
}


########################################
### Creating a Perlin Noise 2D Field ###
########################################

set.seed(2022)
x = seq(1, 10, length.out = 100)
y = seq(1, 10, length.out = 100)
grid = long_grid(x, y)
noise = rep(0, dim(grid)[1])
p = sample(255, 512, replace = TRUE)
iter = 0
for(i in 1:length(x))
{
  for(j in 1:length(y))
  {
    noise[iter] = perlin(x[i], y[j], 0, p)
    iter = iter + 1
  }
}
grid$noise = noise
plot(grid, noise)
