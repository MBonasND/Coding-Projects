#######################################
#######################################
### Double Pendulum w/ Spiking ESN ####
#######################################
#######################################


#Clear Enviroment and load libraries
rm(list = ls())
library(tidyverse)
library(gganimate)
library(ggraph)

#parallel libraries
library(doParallel)
library(parallel)
library(foreach)

#specify cores
options(cores = 4)


#####################################
### Simulate Double Pendulum Data ###
#####################################

#https://www.myphysicslab.com/pendulum/double-pendulum-en.html

#constants
r1 = 100 #length of rod 1
r2 = 100 #length of rod 2
m1 = 100 #mass of rod 1
m2 = 100 #mass of rod 2
theta1 = 2*pi/3 #angle of first pendulum
theta2 = 7*pi/6 #angle of second pendulum
omega1 = 0 #angular velocity of first pendulum
omega2 = 0 #angular velocity of second pendulum
g = 1 #gravitational constant

#acceleration functions
acceleration = function(g, r1, r2, m1, m2, theta1, theta2, omega1, omega2, dt)
{
  numerator_1 = -g*(2*m1 + m2)*sin(theta1) - 
    m2*g*(sin(theta1-2*theta2)) - 
    2*sin(theta1-theta2)*m2*((omega2^2)*r2 + (omega1^2)*r1*cos(theta1-theta2))
  
  numerator_2 = 2*sin(theta1-theta2) * 
    ((omega1^2)*r1*(m1+m2) + g*(m1+m2)*cos(theta1) + (omega2^2)*r2*m2*cos(theta1-theta2))
  
  denominator_1 = r1*(2*m1+m2-m2*cos(2*theta1 - 2*theta2))
  
  denominator_2 = r2*(2*m1+m2-m2*cos(2*theta1 - 2*theta2))
  
  accel_1 = (numerator_1/denominator_1)*(dt^2)
  accel_2 = (numerator_2/denominator_2)*(dt^2)
  return(c(accel_1, accel_2))
}

#Specify initial conditions
x1 = r1 * sin(theta1)
y1 = r1 * cos(theta1)
x2 = x1 + r2 * sin(theta2)
y2 = y1 + r2 * cos(theta2)
TotalTime = 300 #300 runs fairly quickly, 1000 takes about an hour to render GIF
state = matrix(NaN, nrow = TotalTime+1, ncol = 6)
state[1,] = c(x1, y1, theta1, x2, y2, theta2)

#Loop through time to get new positions
for(t in 2:(TotalTime + 1))
{
  #calculate acceleration
  accel = acceleration(g, r1, r2, m1, m2, theta1, theta2, omega1, omega2, dt = 1)
  
  #update location of pendulum 1
  x1 = r1*sin(theta1)
  y1 = r1*cos(theta1)

  #update location of pendulum 2
  x2 = x1 + r2*sin(theta2)
  y2 = y1 + r2*cos(theta2)

  #update velocity
  omega1 = omega1 + accel[1]
  omega2 = omega2 + accel[2]
  
  #update angle
  theta1 = theta1 + omega1
  theta2 = theta2 + omega2
  
  state[t,1] = x1
  state[t,2] = y1
  state[t,3] = theta1
  state[t,4] = x2
  state[t,5] = y2
  state[t,6] = theta2 
}

#Save output as dataframe
df <- tibble(t=1:dim(state)[1], 
             x1 = state[,1],
             y1 = -state[,2],
             x2 = state[,4],
             y2 = -state[,5],
             group=1)

#Plot final path
par(mfrow = c(1,1))
plot(df$x2, df$y2, type = 'l', main = 'Final Path', xlab = '', ylab = '')


#Generate trail for GIF
tmp <- dplyr::select(df,t,x2,y2)
trail <- tibble(x=c(sapply(1:5,function(x) lag(tmp$x2,x))),
                y=c(sapply(1:5,function(x) lag(tmp$y2,x))),
                t=rep(tmp$t,5)) %>%
  dplyr::filter(!is.na(x))

#Plot GIF
ggplot(df)+
  geom_path(data=trail,aes(x,y,group = t),colour="blue",size=0.5)+
  geom_segment(aes(xend=x1,yend=y1),x=0,y=0)+
  geom_segment(aes(xend=x2,yend=y2,x=x1,y=y1))+
  geom_point(size=5,x=0,y=0)+
  geom_point(aes(x1,y1),col="red",size=5)+
  geom_point(aes(x2,y2),col="blue",size=5)+
  scale_y_continuous(limits=c(-(r1+r2),(r1+r2)))+
  scale_x_continuous(limits=c(-(r1+r2),(r1+r2)))+
  ggraph::theme_graph()+
  labs(title="{frame_time} s")+
  transition_time(t) +
  shadow_mark(colour="grey50",size=0.25,exclude_layer = 2:6)-> p
pa <- animate(p,nframes=nrow(df),fps=20)
pa


#Plot starting condition
plot(c(0,df$x1[1],df$x2[1]), c(0, df$y1[1], df$y2[2]), type = 'l',
     main = 'Starting Condition', xlab = '', ylab = '')

#Plot final outcomes
par(mfrow = c(2,3))
plot(df$x1, type = 'l',
     main = 'X Pos. R1', xlab = 'Time', ylab = '')
plot(df$y1, type = 'l',
     main = 'Y Pos. R1', xlab = 'Time', ylab = '')
plot(state[,3], type = 'l',
     main = expression(paste(theta, ' of R1')), xlab = 'Time', ylab = '')
plot(df$x2, type = 'l',
     main = 'X Pos. R2', xlab = 'Time', ylab = '')
plot(df$y2, type = 'l',
     main = 'Y Pos. R2', xlab = 'Time', ylab = '')
plot(state[,6], type = 'l',
     main = expression(paste(theta, ' of R2')), xlab = 'Time', ylab = '')




