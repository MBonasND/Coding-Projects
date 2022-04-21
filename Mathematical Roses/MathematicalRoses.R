####################################
####################################
### Mathematical Rose Simulation ###
####################################
####################################

#https://en.wikipedia.org/wiki/Rose_(mathematics)


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)
library(ambient)


###################################
### Mathematical Roses Function ###
###################################

roses = function(n, d, steps = 100)
{
  k = n/d
  angles = seq(0, 2*pi*d, length.out = steps)
  vertex = matrix(NaN, nrow = length(angles), ncol = 2)
  iter = 1
  for(theta in angles)
  {
    r = cos(k*theta)
    x = r * cos(theta)
    y = r * sin(theta)
    
    vertex[iter,1] = x
    vertex[iter,2] = y
    
    iter = iter + 1
  }
  
  return(vertex)
}


#######################
### Visualize Roses ###
#######################

par(mfrow = c(4, 4), font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
all_d = seq(1, 4, 1)
all_n = seq(1, 4, 1)
for(i in all_d)
{
  for(j in all_n)
  {
    output = roses(n = all_n[j], d = all_d[i], steps = 1000)
    
    if(all_d[i] == 1)
    {
      main = paste0('n = ', all_n[j])
    } else {
      main = ''
    }
    
    if(all_n[j] == 1)
    {
      y_lab = paste0('d = ', all_d[i])
    } else {
      y_lab = ''
    }
    
    plot(output, type = 'l',
         axes = FALSE,
         main = main,
         ylab = y_lab,
         xlab = '')
  }
}



