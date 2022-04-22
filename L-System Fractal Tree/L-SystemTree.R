########################################
########################################
### L-System Fractal Tree Simulation ###
########################################
########################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)



#########################
### L-System Function ###
#########################

ltree = function(x0, y0, generations, angle)
{
  output = list()
  
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
      if(current == 'F')
      {
        x_old = x
        y_old = y
        x = x_old + r*sin(theta)
        y = y_old + r*cos(theta)
        xs = c(x_old, x)
        ys = c(y_old, y)
        coords[[iter]] = rbind(xs, ys)
        iter = iter + 1
      } else if(current == '+'){
        theta = theta + angle
      } else if(current == '-'){
        theta = theta - angle
      } else if(current == '['){
        savecounter = savecounter + 1
        savepoint[[savecounter]] = c(x, y, theta)
      } else if(current == ']'){
        x = savepoint[[savecounter]][1]
        y = savepoint[[savecounter]][2]
        theta = savepoint[[savecounter]][3]
        savecounter = savecounter - 1
      }
    }
    return(coords)
  }
  
  #Generate Tree Loop
  theta = 0
  r = 1/generations
  axiom = 'F'
  string = axiom
  rule = 'FF+[+F-F-F]-[-F+F+F]'
  for(g in 1:generations)
  {
    nextstring = c()
    for(c in 1:str_length(string))
    {
      current = str_split(string, '')[[1]][c]
      found = FALSE
      if(current == axiom)
      {
        found = TRUE
        nextstring = paste0(nextstring, rule)
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



##############################
### Generate L-System Tree ###
##############################

x0 = 0
y0 = 0
generations = 5
angle = 25*pi/180
tree = ltree(x0, y0, generations, angle) 
#4 generations takes 11 seconds
#5 generations takes 9.5 minutes

plot(x0, y0,
     xlim = c(-6,11),
     ylim = c(-1,24),
     type = 'n', axes = FALSE,
     ylab = '',
     xlab = '',
     main = expression(paste(theta,' = ', frac(25*pi, 180))))

for(i in 1:length(tree[[2]]))
{
  x = tree[[2]][[i]][1,]
  y = tree[[2]][[i]][2,]
  lines(x, y)
}



