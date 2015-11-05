#RemoveSpecialCharacters
#This function creates versions of the student names that remove special characters like spaces, hyphens, or apostrophes

RemoveSpecialCharacters = function (Students){
  #This just removes any special characters
  Students$FirstRemoveCharacters = gsub("\\W","",Students$First_Name)
  Students$LastRemoveCharacters = gsub("\\W","",Students$Last_Name)
  
  #This replaces special characters with spaces
  Students$FirstSpaceCharacters = gsub("\\W"," ",Students$First_Name)
  Students$LastSpaceCharacters = gsub("\\W"," ",Students$Last_Name)
  
  #this remove everything after any special character, and converts to all uppercase for exact matching
  Students$FirstRemoveAfterCharacters = toupper(gsub("\\W\\w+"," ",Students$First_Name)) 
  Students$LastRemoveAfterCharacters = toupper(gsub("\\W\\w+"," ",Students$Last_Name))
  
  return(Students)
}