#####################################
#####################################
### Brownian Snowflake Simulation ###
#####################################
#####################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)
library(spdep)


###########################
### Snowflake Generator ###
###########################

snowflake = function(dim, maxpoints, maxIter, stepsize)
{

  intersection = function(snow, current)
  {
    result = FALSE
    for(s in 1:nrow(snow))
    {
      compare = rbind(snow[s,], current)
      d = dist(compare)
      if(d < r)
      {
        result = TRUE
        break
      }
    }
    return(result)
  }
  
  finished = function(current, width)
  {
    return(current[1] < stepsize)
  }
  
  height = dim
  width = dim
  y0 = width
  x0 = height/2
  snow = matrix(c(0, x0), nrow = 1, ncol = 2)
  angle = 0
  r = 1.5
  
  p = 0
  iter = 0
  while(p != (maxpoints-stepsize))
  {
    set.seed(NULL)
    current = c(y0,x0)
    finish = FALSE
    intersect = FALSE
    while(!finish & !intersect)
    {
      #determine movement direction
      current[1] = current[1] - stepsize
      move = runif(1, -3, 3)
      current[2] = current[2] + move
      angle = atan((current[2]-x0)/(current[1]))

      #constrain in wedge
      if(angle > pi/6)
      {
        current[2] = tan(pi/6)*(current[1])
      } else if (angle < 0){
        current[2] = tan(0)*(current[1]) + x0
      }
      
      #Check for finished
      intersect = intersection(snow, current)
      finish = finished(current, width)
      
    }
    
    snow = rbind(snow, current)
    snow = unique(snow)
    p = max(snow[,1])
    print(c(p, iter))
    
    iter = iter+1
    if(iter > maxIter)
    {
      break
    }
    
  }

  return(snow)
  
}


##########################
### Generate Snowflake ###
##########################

dim = 200
maxpoints = dim
maxIter = 1000
stepsize = 1
set.seed(NULL)

snow = snowflake(dim, maxpoints, maxIter, stepsize)
plot(snow, pch = 19,
     xlim = c(-max(snow[,1]), max(snow[,1])),
     ylim = c(0, dim), 
     cex =0.6)

refsnow = cbind(snow[,1], -snow[,2]+dim)
fullsnow = rbind(snow, refsnow)
fullsnow = fullsnow - matrix(c(0,dim/2), nrow = nrow(fullsnow), ncol = 2, byrow = TRUE)
fullsnow = unique(fullsnow)

plot(fullsnow, pch = 19,
     xlim = c(-max(fullsnow[,1]),max(fullsnow[,1])),
     ylim = c(-max(fullsnow[,1]),max(fullsnow[,1])),
     cex = 0.6, axes = FALSE,
     xlab = '',
     ylab = '')
rad <- seq(0, 2*pi, l=12)
for(i in rad){
  coords.rot <- Rotation(fullsnow, i)
  points(coords.rot, pch = 19, cex = 0.6)
}

