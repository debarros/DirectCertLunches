#SubsetWords.R

#This function pulls individual words out of a set of variables
#    SMS is the data set
#    Vars are the variables to examine
#    Varname is the root word for the new variables names
#    AddtlVars is the list of variables to bring in without subsetting
#    n is the max number of words to pull from each string
#    Sort indicates whether the results should be sorted by string length

SubsetWords = function(SMS, Vars, Varname, AddtlVars = c(), n = NA_integer_, Sort = T){
  
  # Narrow the list of variables to only those that are character.  This removes variables that have no data.
  whichToUse = vector(mode = "logical", length = length(Vars))
  for(i in 1:length(whichToUse)){whichToUse[i] = typeof(SMS[,Vars[i]]) == "character"}
  Vars = Vars[whichToUse]
  
  z = vector(mode = "list", length = length(Vars))  #inititalize the list to hold the split words
  
  for (i in 1:length(Vars)){z[[i]] = Splitter(SMS, Var = Vars[i], n = n, Sort = Sort)} #collect the words into the list
  
  z = cbind.data.frame(z[1:length(Vars)])                      #combine the split word results into one data frame
  if (!is.null(AddtlVars)){z = cbind.data.frame(z, SMS[,AddtlVars])} #if there are addtl variables to use, grab them too
  z = as.matrix(z)                                             #make it a matrix
  z = apply(X = z, MARGIN = c(1,2), FUN = na.to.empty)         #convert NA's to empty character values
  z = apply(X = z, FUN = unique, MARGIN = 1)                   #remove duplicates
  z = lapply(z, one.drop)                                      #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z)                             #convert the list of vectors into a data.frame
  colnames(z) = paste0(Varname,1:ncol(z))                    #add column names
  
  SMS = cbind.data.frame(SMS, z) #add the new columns to the existing SMS data.frame
  
  return(SMS)
}