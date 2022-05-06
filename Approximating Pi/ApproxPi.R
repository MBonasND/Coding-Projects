##############################################
##############################################
### Different Methods for Approixmating Pi ###
##############################################
##############################################

rm(list = ls())
library(tidyverse)


######################
### Leibniz Method ###
######################

leibniz = function(n, plot.flag = FALSE)
{
  #calculate summation
  numerator = (-1)^((1:n)+1)
  denominator = 2*(1:n) - 1
  summation = sum(numerator/denominator)
  
  #calculate estimate for pi
  pie = 4 * summation
  
  #plot
  if(plot.flag)
  {
    output = rep(0, n)
    for(i in 1:n)
    {
      numerator = (-1)^((1:i)+1)
      denominator = 2*(1:i) - 1
      summation = sum(numerator/denominator)
      output[i] = summation * 4
    }
    plot(output, type = 'l',
         xlab = 'Total Steps',
         ylab = '', main = expression(paste('Leibniz Approximation of ', pi)))
    abline(h = pi, col = 'steelblue')
  }
  
  return(pie)
}


#Test function
leibniz(n = 10, plot.flag = TRUE)
leibniz(n = 100, plot.flag = TRUE)
leibniz(n = 1000, plot.flag = TRUE)
leibniz(n = 1e6, plot.flag = FALSE)



###############################
### "Throwing Darts" Method ###
###############################

pidarts = function(n, plot.flag = FALSE, seed = NULL)
{
  #generate random uniform variables
  set.seed(NULL)
  x = runif(n, min = -1)
  y = runif(n, min = -1)
  
  #check for points in unit circle
  in.flag = sum(x^2 + y^2 <= 1)
  
  #estimate pi
  pie = (in.flag/n) * 4
  
  
  #plot
  if(plot.flag)
  {
    z = as.numeric((x^2 + y^2 <= 1)+1)
    colors = c('red', 'green')
    plot(x, y, col = colors[z], pch = 19,
         xlim = c(-1,1), ylim = c(-1,1),
         asp = 1, main = expression(paste('"Throwing Dart" to Approximate ', pi)))
  }
  
  return(pie)
}


#Test Function
pidarts(n = 100, plot.flag = TRUE)
pidarts(n = 1000, plot.flag = TRUE)
pidarts(n = 10000, plot.flag = TRUE)
pidarts(n = 1e8, plot.flag = FALSE)



#########################
### Mandelbrot Method ###
#########################

mandelbrotpi = function(digits, verbose = TRUE)
{
  #Specify constants
  epsilon = 1/(100^(digits-1))
  iter = 0
  z = 0
  c = 0.25 + epsilon
  
  #estimate pi
  while(z < 2)
  {
    iter = iter+1
    z = z^2 + c
    if(verbose)
    {
      print(iter)
    }
  }
  
  return(iter)
}


#Test function
mandelbrotpi(digits = 2, verbose = FALSE)
mandelbrotpi(digits = 3, verbose = FALSE)
mandelbrotpi(digits = 5, verbose = FALSE)
mandelbrotpi(digits = 6, verbose = FALSE)





















