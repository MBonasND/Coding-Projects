############################################
############################################
### Random Walk & Levy Flight Simulation ###
############################################
############################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)
library(reshape2)


############################
### Random Walk Function ###
############################

random_walk = function(grid, steps, seed = NULL)
{
  set.seed(seed)
  height = dim(grid)[1]
  width = dim(grid)[2]
  y = width/2
  x = height/2
  for(t in 1:steps)
  {
    #determine movement direction
    xy_choice = sample(c('x', 'y'), 1)
    if(xy_choice == 'x')
    {
      move = sample(c(-1,1), 1)
      x = x + move
    } else {
      move = sample(c(-1,1), 1)
      y = y + move
    }
    
    #wrap around edges
    if (x > width) {
      x = 1
    } else if (x < 1) {
      x = width
    }
    if (y > height) {
      y = 1
    } else if (y < 1) {
      y = height
    }
    
    grid[x,y] = 1
  }
  
  return(grid)
  
}


############################
### Levy Flight Function ###
############################

levy = function(x0, y0, steps, seed, distribution = 'cauchy')
{
  set.seed(seed)
  x = x0
  y = y0
  output = c(x, y)
  for(t in 1:steps)
  {
    #determine stepsize
    if(distribution == 'cauchy')
    {
      stepsize = rcauchy(1)
    } else if(distribution == 'normal'){
      stepsize = rnorm(1)
    } else if (distribution == 'gamma'){
      stepsize = rgamma(1, shape = 2, scale = 2)
    }
    
    #determine movement direction
    xy_choice = sample(c('x', 'y'), 1)
    if(xy_choice == 'x')
    {
      move = sample(c(-1,1), 1)
      x = round(x + move*stepsize)
    } else {
      move = sample(c(-1,1), 1)
      y = round(y + move*stepsize)
    }
    
    output = rbind(output, c(x,y))
  }
  
  return(output)
}


########################
### Plot Random Walk ###
########################

dim = 500
grid = matrix(0, nrow = dim, ncol = dim)
steps = 100000
seed = 2022

grid = random_walk(grid, steps = steps, seed = seed)

image(1:nrow(grid), 1:ncol(grid), t(grid),
      col = gray.colors(2, start = 1, end = 0),
      axes = FALSE, xlab = '', ylab = '')



#######################################
### Plot Random Walk w/ Levy Flight ###
#######################################

x0 = 0
y0 = 0
steps = 5000
seed = 1997


par(mfrow = c(1,3))
distribution = 'cauchy'
output = levy(x0, y0, steps, seed, distribution)
plot(output, type = 'l', axes = FALSE,
     xlab = '', ylab = '', main = 'Cauchy(0,1)')

distribution = 'normal'
output = levy(x0, y0, steps, seed, distribution)
plot(output, type = 'l', axes = FALSE,
     xlab = '', ylab = '', main = 'Normal(0,1)')

distribution = 'gamma'
output = levy(x0, y0, steps, seed, distribution)
plot(output, type = 'l', axes = FALSE,
     xlab = '', ylab = '', main = 'Gamma(2,2)')



