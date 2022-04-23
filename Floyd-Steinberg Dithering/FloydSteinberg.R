############################################
############################################
### Floyd-Steinberg Dithering Simulation ###
############################################
############################################


#Clear Environment and load libraries
rm(list = ls())
library(tidyverse)
library(imager)


##########################################
### Floyd-Steinberg Dithering Function ###
##########################################

FSDithering = function(image, steps, verbose = FALSE)
{
  #Closest color on new scale
  closestStep = function(maxColor, steps, Color)
  {
    return(round((steps * Color) / maxColor) * (maxColor / steps))
  }
  
  #Distribute error to neighboring pixels
  distributeError = function(image, x, y, errorColor)
  {
    addError(image, 7/16, x+1, y  , errorColor);
    addError(image, 3/16, x-1, y+1, errorColor);
    addError(image, 5/16, x  , y+1, errorColor);
    addError(image, 1/16, x+1, y+1, errorColor);
  }
  
  #Add error function
  addError = function(image, factor, x, y, errorColor)
  {
    if(x < 1 | x >= dim(image)[1] | y < 1 | y >= dim(image)[2])
    {
      return()
    }
    Color = color.at(image, x, y, 1)
    color.at(image, x, y, 1) = Color + errorColor * factor
  }
  
  #Get maximum color scale
  maxColor = max(image[,,1,])
  
  #Loop through pixels
  dimensions = dim(image)
  if(verbose)
  {
    prog.bar = txtProgressBar(min = 0,
                              max = dimensions[1]*dimensions[2],
                              style = 3)
  }
  iter = 0
  for(y in 1:dimensions[2])
  {
    for(x in 1:dimensions[1])
    {
      Color = color.at(image, x, y, 1)
      newColor = closestStep(maxColor, steps, Color)
      color.at(image, x, y, 1) = newColor
      
      errorColor = Color - newColor
      
      distributeError(image, x, y, errorColor)
      
      iter = iter+1
      if(verbose)
      {
        setTxtProgressBar(prog.bar, iter)
      }
    }
  }
  
  return(image)

}


#############################
### Test on Giraffe Image ###
#############################


#Load Images
giraffe = load.image('giraffe.png')
# dim(giraffe)
par(mfrow = c(2,3))
plot(giraffe, axes = FALSE, main = 'Original')
stepsize = rev(c(1,2,3,5,10))
for(i in stepsize)
{
img = FSDithering(giraffe, i, verbose = TRUE) 
plot(img, axes = FALSE, main = paste('Colors =', (i+1)^3))
}


##################################
### Test on Gray Giraffe Image ###
##################################


#Load Images
giraffe = load.image('giraffe.png')
giraffe = grayscale(as.cimg(giraffe[,,,-4]))
par(mfrow = c(2,3))
plot(giraffe, axes = FALSE, main = 'Original')
stepsize = rev(c(1,2,3,5,10))
for(i in stepsize)
{
  img = FSDithering(giraffe, i, verbose = TRUE) 
  plot(img, axes = FALSE, main = paste('Colors =', (i+1)))
}

