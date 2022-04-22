######################################
######################################
### Sierpinski Triangle Simulation ###
######################################
######################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)



####################################
### Sierpinski Triangle Function ###
####################################

sierpinski = function(x0, y0, generations)
{
  output = list()
  angle = 2*pi/3
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
      } else if(current == 'G'){
        x_old = x
        y_old = y
        x = x_old + r*sin(theta)
        y = y_old + r*cos(theta)
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
  theta = pi/6
  r = 1/generations
  variables = c('F', 'G')
  string = 'F-G-G'
  rules = list(); rules[[1]] = 'F-G+F+G-F'; rules[[2]] = 'GG'
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



#####################################
### Sierpinski Arrowhead Function ###
#####################################

sierpinski_arrow = function(x0, y0, generations)
{
  output = list()
  angle = pi/3
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
      if(current == 'A')
      {
        x_old = x
        y_old = y
        x = x_old + r*sin(theta)
        y = y_old + r*cos(theta)
        xs = c(x_old, x)
        ys = c(y_old, y)
        coords[[iter]] = rbind(xs, ys)
        iter = iter + 1
      } else if(current == 'B'){
        x_old = x
        y_old = y
        x = x_old + r*sin(theta)
        y = y_old + r*cos(theta)
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
  variables = c('A', 'B')
  string = 'A'
  rules = list(); rules[[1]] = 'B-A-B'; rules[[2]] = 'A+B+A'
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
generations = 6
triangle = sierpinski(x0, y0, generations) 

plot(x0, y0,
     xlim = c(0,11),
     ylim = c(0,10),
     type = 'n', axes = FALSE,
     ylab = '',
     xlab = '', 
     main = 'Sierpinski Triangle (n=6)')

for(i in 1:length(triangle[[2]]))
{
  x = triangle[[2]][[i]][1,]
  y = triangle[[2]][[i]][2,]
  lines(x, y)
}


#####################################
### Generate Sierpinski Arrowhead ###
#####################################

x0 = 0
y0 = 0
generations = 6
arrowhead = sierpinski_arrow(x0, y0, generations) 

plot(x0, y0,
     xlim = c(0,11),
     ylim = c(0,10),
     type = 'n', axes = FALSE,
     ylab = '',
     xlab = '',
     main = 'Sierpinski Arrowhead (n=6)')

for(i in 1:length(arrowhead[[2]]))
{
  x = arrowhead[[2]][[i]][1,]
  y = arrowhead[[2]][[i]][2,]
  lines(x, y)
}


