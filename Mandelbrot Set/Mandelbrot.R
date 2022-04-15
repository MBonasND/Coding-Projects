#################################
#################################
### Mandelbrot Set Simulation ###
#################################
#################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)


#Parallel Libraries
library(doParallel)
library(parallel)
library(foreach)

#Specify Cores
options(cores = 4)


###########################
### Mandelbrot Function ###
###########################

mandelbrot = function(range = c(-2,2,-2,2), maxIter = 100, fine = 500)
{
  #Specify Grid
  xrange = seq(range[1], range[2],
               (range[2] - range[1])/fine)
  yrange = seq(range[3], range[4],
               (range[4] - range[3])/fine)
  grid = expand.grid(xrange, yrange)
  
  #Iterate and Check for Divergence
  cl = parallel::makeCluster(getOption('cores'))
  doParallel::registerDoParallel(cl)
  grid_n = foreach::foreach(i = 1:dim(grid)[1],
                            .combine = c,
                            .inorder = TRUE) %dopar%
  {
    a = grid[i,1]
    b = grid[i,2]
    ca = a
    cb = b
    
    n = 0
    while(n < maxIter)
    {
      #complex number coefficients
      aa = a^2 - b^2
      bb = 2 * a * b
      
      #specify new z() values
      a = aa + ca
      b = bb + cb
      
      #check divergence
      if(a^2 + b^2 > 4)
      {
        break
      }
      n = n + 1
    }
    
    #Output total steps
    n
  }
  
  #Return output
  parallel::stopCluster(cl)
  return(cbind(grid,grid_n))
}



###########################
### Plot Mandelbrot Set ###
###########################

par(mfrow = c(1, 1))
#Generate Set
range = c(-1.5, 1, -1.25, 1.25)
mod = mandelbrot(range, maxIter = 100, fine = 250)

#plot
#colors = gray.colors(n = (max(mod[,3])+1), start = 0, end = 1)
colors = rainbow(max(mod[,3]+1))
plot(mod[,1], mod[,2],
     col = colors[mod[,3]+1],
     pch = 19,
     cex = 1,
     xaxt = 'n', xlab = '',
     yaxt = 'n', ylab = '')


#Generate a Zoomed Set
range = c(-1,0,0,1)
mod = mandelbrot(range, maxIter = 100, fine = 300)

#plot
#colors = gray.colors(n = (max(mod[,3])+1), start = 0, end = 1)
colors = rainbow(max(mod[,3]+1))
plot(mod[,1], mod[,2],
     col = colors[mod[,3]+1],
     pch = 19,
     cex = 1,
     xaxt = 'n', xlab = '',
     yaxt = 'n', ylab = '')

