############################
############################
### Julia Set Simulation ###
############################
############################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)


#Parallel Libraries
library(doParallel)
library(parallel)
library(foreach)

#Specify Cores
options(cores = 4)


##########################
### Julia Set Function ###
##########################

julia = function(range = c(-2,2,-2,2), maxIter = 100, fine = 500, 
                 complex_a, complex_b, poly = 2)
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
      ca = complex_a
      cb = complex_b
      
      n = 0
      while(n < maxIter)
      {
        #complex number coefficients
        aa = (a^2 + b^2)^(poly/2) * cos(poly * atan2(b, a))
        bb = (a^2 + b^2)^(poly/2) * sin(poly * atan2(b, a))
        
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



######################
### Plot Julia Set ###
######################

par(mfrow = c(1,1))
#Generate Set
range = c(-1, 1, -1.25, 1.25)
ab = c(0.285, 0.01)

#others: c(0.285,0.01), c(0, -0.8), c(-0.8, 0.156), c(-0.74543, 0.11301)

mod = julia(range, maxIter = 10000, fine = 250,
            complex_a = ab[1], complex_b = ab[2], poly = 2)

#plot
colors = gray.colors(n = (max(mod[,3])+1), start = 0, end = 1)
#colors = (rainbow(max(mod[,3]+1)))
plot(mod[,1], mod[,2],
     col = colors[mod[,3]+1],
     pch = 19,
     cex = 1,
     xaxt = 'n', xlab = '',
     yaxt = 'n', ylab = '')

