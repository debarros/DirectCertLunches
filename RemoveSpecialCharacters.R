#RemoveSpecialCharacters
#This function creates versions of the student names that remove special characters like spaces, hyphens, or apostrophes

RemoveSpecialCharacters = function (Students, Vars, nameForms, messageLevel = 0){

  for (i in 1:length(Vars)){
    #This just removes any special characters
    Students[,paste0(names(Vars[i]),"RemoveCharacters")] = gsub("\\W","",Students[,Vars[[i]]])
    
    #This replaces special characters with spaces
    Students[,paste0(names(Vars[i]),"SpaceCharacters")] = gsub("\\W"," ",Students[,Vars[[i]]])
    
    #this remove everything after any special character, and converts to all uppercase for exact matching
    Students[,paste0(names(Vars[i]),"RemoveAfterCharacters")] = toupper(gsub("\\W\\w+"," ",Students[,Vars[[i]]])) 
  }

  return(Students)
}