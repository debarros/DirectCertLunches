#SubsetWords.R

#This function pulls individual words out of a set of variables
#    SMS is the data set
#    Vars are the variables to examine
#    Varname is the root word for the new variables names
#    AddtlVars is the list of variables to bring in without subsetting
#    n is the max number of words to pull from each string
#    Sort indicates whether the results should be sorted by string length

SubsetWords = function(SMS, Vars, Varname, AddtlVars = c(), n = NA_integer_, Sort = T, messageLevel = 0){
  
  if(messageLevel > 0) message("running SubsetWords function")
  
  if(messageLevel > 1) message("removing variables that have no data")
  
  if(messageLevel > 2) {
    print("str(Vars) = ")
    print(str(Vars))
    print("Vars = ")
    print(Vars)
  }
  
  
  # Narrow the list of variables to only those that are not all missing.  This removes variables that have no data.
  whichToUse = vector(mode = "logical", length = length(Vars))
  for(i in 1:length(whichToUse)){
    if(messageLevel > 2) message(paste0("checking for data in ",Vars[i]))
    whichToUse[i] = !all(is.na(SMS[,Vars[i]]))
  }
  Vars = Vars[whichToUse]
  
  if(messageLevel > 1) message("setting up the list to hold split words")
  
  z = vector(mode = "list", length = length(Vars))  #inititalize the list to hold the split words
  
  if(messageLevel > 1) message("collecting split words")
  
  for (i in 1:length(Vars)){z[[i]] = Splitter(SMS, Var = Vars[i], n = n, Sort = Sort)} #collect the words into the list
  
  if(messageLevel > 1) message("turning the list of split words into a data.frame")
  
  z = cbind.data.frame(z[1:length(Vars)])                      #combine the split word results into one data frame
  if (!is.null(AddtlVars)){z = cbind.data.frame(z, SMS[,AddtlVars])} #if there are addtl variables to use, grab them too
  z = as.matrix(z)                                             #make it a matrix
  z = apply(X = z, MARGIN = c(1,2), FUN = na.to.empty)         #convert NA's to empty character values
  z = apply(X = z, FUN = unique, MARGIN = 1)                   #remove duplicates
  z = lapply(z, one.drop)                                      #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z)               #convert the list of vectors into a data.frame
  colnames(z) = paste0(Varname,1:ncol(z))                    #add column names
  for(i in 1:ncol(z)){
    if(is.factor(z[,i])){z[,i] = as.character(z[,i])}
  }
  
  if(messageLevel > 1) message("add the split word variables to the existing data.frame")
  SMS = cbind.data.frame(SMS, z) #add the new columns to the existing SMS data.frame
  
  if(messageLevel > 0) message("done running SubsetWords function")
  
  return(SMS)
}