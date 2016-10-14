#Astack.R

#Stacking 2 objects, each of which can be 2 or 3 dimensional

Astack = function(x, y, messageLevel = 0){
  
  if(messageLevel > 0) message("starting Astack function")
  
  if(sum((dim(x)[1:2] == dim(y)[1:2])) < 2){
    print(paste0("Error: The first two dimensions of the two objects must match. dim(x) = ",dim(x)[1]," , ",dim(x)[2], 
                 " and dim(y) = ", dim(y)[1]," , ",dim(y)[1]))
    return()
  } #end of if
  
  x = array(data = x, dim = dim(x)) #convert first argument to an array
  y = array(data = y, dim = dim(y)) #convert second argument to an array
  
  if(length(dim(x)) == 2){x = array(data = x, dim = c(dim(x), 1))}  #if x is 2D, make it 3d
  if(length(dim(y)) == 2){y = array(data = y, dim = c(dim(y), 1))}  #if y is 2d, make it 3d
  
  z = array(data = c(x, y), dim = dim(x) + c(0,0,dim(y)[3]))        #stack the arrays
  
  if(messageLevel > 0) message("Ending Astack function")
  
  return(z)
  
} #end of Astack function