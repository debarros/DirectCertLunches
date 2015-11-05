#VbetterComp.R

#Compare two values and return FALSE if either is NA
betterComp = function(x,y){
  z = (x == y)
  z[is.na(z)] = FALSE
  return(z)
}

#Compare two vectors and get a matrix of True/False
VbetterComp = Vectorize(FUN = betterComp, vectorize.args = "y")

