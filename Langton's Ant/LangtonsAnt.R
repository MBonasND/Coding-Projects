################################
################################
### Langton's Ant Simulation ###
################################
################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)


#Parallel Libraries
library(doParallel)
library(parallel)
library(foreach)

#Specify Cores
options(cores = 4)


##############################
### Langton's Ant Function ###
##############################

langtons = function(grid, time)
{
  x = dim(grid)[2]/2
  y = dim(grid)[1]/2
  direction = 0
  width = dim(grid)[2]
  height = dim(grid)[1]
  
  right = function(direction)
  {
    direction = direction + 1
    if(direction > 3)
    {
      direction = 0
    }
    return(direction)
  }
  
  left = function(direction){
    direction = direction - 1
    if(direction < 0)
    {
      direction = 3
    }
    return(direction)
  }
  
  forward = function(direction, x, y){
    if (direction == 0){
      y = y - 1
    } else if (direction == 1){
      x = x + 1
    } else if (direction == 2){
      y = y + 1
    } else if (direction == 3){
      x = x - 1
    }
    
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
    
    
    return(c(x, y))
  }
  
  for(t in 1:time)
  {
    state = grid[x,y]
    if(state == 0)
    {
      direction = right(direction)
      grid[x,y] = 1
    } else if(state == 1){
      direction = left(direction)
      grid[x,y] = 0
    }
    
    newLoc = forward(direction, x, y)
    x = newLoc[1]
    y = newLoc[2]
  }
  
  return(grid)
  
}


##########################
### Plot Langton's Ant ###
##########################

dim = 100
set.seed(1997)
#starter = sample(c(1,0), dim^2, replace = TRUE, prob = c(0.001, 0.999))
starter = 0
grid = matrix(starter, nrow = dim, ncol = dim)

grid = langtons(grid, time = 11000)

image(1:nrow(grid), 1:ncol(grid), t(grid),
      col = gray.colors(2, start = 1, end = 0),
      axes = FALSE, xlab = '', ylab = '')
