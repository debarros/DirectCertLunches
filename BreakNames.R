#BreakNames

BreakNames = function(SMS){
  z = strsplit(x = SMS$guardian, "\\s+") # create a list of vectors of the words in the name
  z = lapply(z, gsub, pattern = "\\W", replacement = "") #drop special characters
  z = lapply(z, na.to.empty) #convert NA's to empty character values
  z = lapply(z, one.drop) #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z) #convert the list of vectors into a data.frame
  SMS$guardian1 = as.character(z[,1]) #add the name words to the Student data.frame
  SMS$guardian2 = as.character(z[,2])
  SMS$guardian3 = as.character(z[,3])
  
  
  z = strsplit(x = SMS$Father, "\\s+") # create a list of vectors of the words in the name
  z = lapply(z, gsub, pattern = "\\W", replacement = "") #drop special characters
  z = lapply(z, na.to.empty) #convert NA's to empty character values
  z = lapply(z, one.drop) #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z) #convert the list of vectors into a data.frame
  SMS$father1 = as.character(z[,1]) #add the name words to the Student data.frame
  SMS$father2 = as.character(z[,2])
  SMS$father3 = as.character(z[,3])
  
  
  z = strsplit(x = SMS$Mother, "\\s+") # create a list of vectors of the words in the name
  z = lapply(z, gsub, pattern = "\\W", replacement = "") #drop special characters
  z = lapply(z, na.to.empty) #convert NA's to empty character values
  z = lapply(z, one.drop) #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z) #convert the list of vectors into a data.frame
  SMS$mother1 = as.character(z[,1]) #add the name words to the Student data.frame
  SMS$mother2 = as.character(z[,2])
  SMS$mother3 = as.character(z[,3])
  
  
  NameSet = c("Guardian_FN", "Guardian_LN", "Guardian_MN", "guardian1", "guardian2", "guardian3", "father1", "father2", "father3", "mother3", "mother2", "mother1")
  
  NameMatrix = as.matrix(SMS[,NameSet])
  NameMatrix = apply(X = NameMatrix, MARGIN = c(1,2), FUN = na.to.empty) #convert NA's to empty character values
  
  z = apply(X = NameMatrix, FUN = unique, MARGIN = 1)
  z = lapply(z, one.drop) #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z) #convert the list of vectors into a data.frame
  colnames(z) = paste0("gName",1:ncol(z)) #add column names
  
  SMS = cbind.data.frame(SMS, z) #add the new columns to the existing SMS data.frame
  
  return(SMS)  
}