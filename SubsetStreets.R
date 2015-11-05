#SubsetStreets function
#This function creates multiple versions of the student street to use for regular expression matching

SubsetStreets = function(SMS){
  
  z = strsplit(x = SMS$Mailing_Street, "\\s+") # create a list of vectors of the words in the addresses
  z = lapply(z, gsub, pattern = "\\W", replacement = "") #drop special characters
  z = lapply(z, na.to.empty) #convert NA's to empty character values
  z = lapply(z, one.drop) #eliminate words that have only one character
  z = lapply(z, SortLength) #sort each vector by the length of the word, starting with the longest
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z) #convert the list of vectors into a data.frame
  SMS$Street1A = as.character(z[,1]) #add the street words to the Student data.frame
  SMS$Street1B = as.character(z[,2])
  SMS$Street1C = as.character(z[,3])
  
  
  z = strsplit(x = SMS$X2nd_mailing_street, "\\s+")
  z = lapply(z, gsub, pattern = "\\W", replacement = "")
  z = lapply(z, na.to.empty) #convert NA's to empty character values
  z = lapply(z, one.drop)
  z = lapply(z, SortLength)
  z = lapply(z, setlength, y = max(unlist(lapply(z, length))))
  z = do.call(rbind.data.frame, z)
  SMS$Street2A = as.character(z[,1])
  SMS$Street2B = as.character(z[,2])
  SMS$Street2C = as.character(z[,3])
  
  
  z = strsplit(x = SMS$Street, "\\s+")
  z = lapply(z, gsub, pattern = "\\W", replacement = "")
  z = lapply(z, na.to.empty) #convert NA's to empty character values
  z = lapply(z, one.drop)
  z = lapply(z, SortLength)
  z = lapply(z, setlength, y = max(unlist(lapply(z, length))))
  z = do.call(rbind.data.frame, z)
  SMS$Street3A = as.character(z[,1])
  SMS$Street3B = as.character(z[,2])
  SMS$Street3C = as.character(z[,3])
  
  
  StreetSet = c("Mailing_Street","X2nd_mailing_street","Street","Street1A", "Street1B", "Street1C", "Street2A", "Street2B", "Street2C", "Street3A", "Street3B", "Street3C")
  
  StreetMatrix = as.matrix(SMS[,StreetSet])
  StreetMatrix = apply(X = StreetMatrix, MARGIN = c(1,2), FUN = na.to.empty) #convert NA's to empty character values
  
  z = apply(X = StreetMatrix, FUN = unique, MARGIN = 1)
  z = lapply(z, one.drop) #eliminate words that have only one character or are empty
  z = lapply(z, setlength, y = max(unlist(lapply(z, length)))) #increase the length of each vector to the max
  z = do.call(rbind.data.frame, z) #convert the list of vectors into a data.frame
  colnames(z) = paste0("streetPart",1:ncol(z)) #add column names
  
  SMS = cbind.data.frame(SMS, z) #add the new columns to the existing SMS data.frame
  
  
  
  return(SMS)
}
