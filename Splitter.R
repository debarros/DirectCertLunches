#Splitter.R

#This function takes a character variable from a data.frame, pulls out the individual words, and returns a data.frame of the words
#It takes the following arguments:
#   SMS is the data.frame
#   Var is a character variable of length 1, containing the name of column/character variable to be used
#   n in an integer of length 1, and holds the max number of new variables to create.  If NA, no max is established.
#   Sort is a logical of length 1, indicating whether to sort the words by length (descending) before creating the new variables
#   n is the number of words to return

Splitter = function(SMS, Var, n = NA_integer_, Sort = F){
  
  z = strsplit(x = SMS[,Var], "\\s+")                          # create a list of vectors of the words in the addresses
  z = lapply(z, gsub, pattern = "\\W", replacement = "")       #drop special characters
  z = lapply(z, na.to.empty)                                   #convert NA's to empty character values
  z = lapply(z, one.drop)                                      #eliminate words that have only one character
  if(Sort){z = lapply(z, SortLength)}                          #If required, sort each vector by the length of the word, starting with the longest
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z)                             #convert the list of vectors into a data.frame
  
  if(!is.na(n)){                                               #if there is a max number of variables, use it
    maxcols = min(n, ncol(z))
    z = z[,1:maxcols]
  } 
  
  return(z)
}
