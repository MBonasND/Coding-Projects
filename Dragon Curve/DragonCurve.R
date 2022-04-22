###############################
###############################
### Dragon Curve Simulation ###
###############################
###############################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)



#############################
### Dragon Curve Function ###
#############################

dragon = function(x0, y0, generations)
{
  output = list()
  angle = pi/2
  #Turtle Drawing Function
  turtle = function(string, theta, angle)
  {
    x = x0
    y = y0
    coords = list()
    savecounter = 0
    savepoint = list()
    iter = 1
    for(c in 1:str_length(string))
    {
      current = str_split(string, '')[[1]][c]
      if(current == 'F' | current == 'G')
      {
        x_old = x
        y_old = y
        x = x_old + r*cos(theta)
        y = y_old + r*sin(theta)
        xs = c(x_old, x)
        ys = c(y_old, y)
        coords[[iter]] = rbind(xs, ys)
        iter = iter + 1
      } else if(current == '+'){
        theta = theta - angle
      } else if(current == '-'){
        theta = theta + angle
      }
    }
    return(coords)
  }
  
  #Generate sierpinski Loop
  theta = pi/2
  r = 1/generations
  variables = c('F', 'G')
  string = 'F'
  rules = list(); rules[[1]] = 'F+G'; rules[[2]] = 'F-G'
  for(g in 1:generations)
  {
    nextstring = c()
    for(c in 1:str_length(string))
    {
      current = str_split(string, '')[[1]][c]
      found = FALSE
      if(current == variables[1])
      {
        found = TRUE
        nextstring = paste0(nextstring, rules[[1]])
      } else if(current == variables[2])
      {
        found = TRUE
        nextstring = paste0(nextstring, rules[[2]])
      }
      
      if(!found)
      {
        nextstring = paste0(nextstring, current)
      }
    }
    string = nextstring
  }
  
  output[[1]] = string
  output[[2]] = turtle(string, theta, angle)
  return(output)
}



####################################
### Generate Sierpinski Triangle ###
####################################

x0 = 0
y0 = 0
generations = 13
curve = dragon(x0, y0, generations) 

plot(x0, y0,
     xlim = c(-6.5,3),
     ylim = c(-7,2),
     type = 'n', axes = FALSE,
     ylab = '',
     xlab = '', 
     main = 'Dragon Curve (n=13)')

for(i in 1:length(curve[[2]]))
{
  x = curve[[2]][[i]][1,]
  y = curve[[2]][[i]][2,]
  lines(x, y)
}




