#AnalyzeHandMatches.R
#This function looks through a prior output file and (if available) a table of existing case matches
#It find all of the matches identified in the prior output file
#It then puts them in the table of case matches (creating it first if necessary)
#It returns the table of case matches

HandMatcher = function(handMatch, DC.Case.Matches = NULL, messageLevel = 0){
  
  if(messageLevel > 0) message("running HandMatcher function")
  
  if(!is.null(handMatch$Student)){
  nCases = which(is.na(handMatch$Student))[1] - 2
  print("a")
  nStudents = nrow(handMatch)/(nCases+2)
  print("b")
  matchCount = 0
  print("c")
  # Code to create the DC.Case.Matches object if it doesn't exist yet
  if(is.null(DC.Case.Matches)){
  DC.Case.Matches = matrix(NA_character_,0,8)
  colnames(DC.Case.Matches) = c("Student_Number","Student","Guardian","DOB","Address","Case.Type","Case.Num","Score")
  DC.Case.Matches = as.data.frame(DC.Case.Matches, stringsAsFactors = F)
  }
  
  print("d")
  
  for(i in 1:nStudents){
    headrow = (i-1)*(nCases+2)+1
    studentInfo = handMatch[headrow,]
    matchInfo = handMatch[(headrow+1):(headrow+nCases),]
    matchInfo = matchInfo[!is.na(matchInfo$CorrectMatch),]
    if(nrow(matchInfo)>0){
      matchCount = matchCount+1
      appendRow = matchInfo
      appendRow$Student_Number = studentInfo$ID
      appendRow$StudentOrSibling = matchInfo$CorrectMatch
      appendRow$StudentOrSibling[appendRow$StudentOrSibling == "X"] = "Student"
      appendRow$StudentOrSibling[appendRow$StudentOrSibling == "S"] = "Sibling"
      appendRow = appendRow[,colnames(DC.Case.Matches)]
      DC.Case.Matches = rbind.data.frame(DC.Case.Matches, appendRow, stringsAsFactors = F)
    }
  }
  
  print("e")
  
  DC.Case.Matches$all = apply(DC.Case.Matches, 1, paste, collapse = " ")
  DC.Case.Matches = DC.Case.Matches[!duplicated(DC.Case.Matches$all),]
  DC.Case.Matches = DC.Case.Matches[,1:(ncol(DC.Case.Matches)-1)]
 
  print("f")
  
  if(messageLevel > 0) message("done running HandMatcher function")
  
  print("g")
  } else {
    DC.Case.Matches = NULL
  }
  return(DC.Case.Matches) 
}