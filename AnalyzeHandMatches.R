#AnalyzeHandMatches.R


handMatch = read.xlsx(xlsxFile = "matched records.xlsx", sheet = "Potential.Matches")


nCases = which(is.na(handMatch$Student))[1] - 2

nStudents = nrow(handMatch)/(nCases+2)

matchCount = 0

# Code to create the DC.Case.Matches object if it doesn't exist yet
#DC.Case.Matches = matrix(NA_character_,0,8)
#colnames(DC.Case.Matches) = c("Student_Number","Student","Guardian","DOB","Address","Case.Type","Case.Num","Score")
#DC.Case.Matches = as.data.frame(DC.Case.Matches, stringsAsFactors = F)



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

DC.Case.Matches$all = apply(DC.Case.Matches, 1, paste, collapse = " ")
DC.Case.Matches = DC.Case.Matches[!duplicated(DC.Case.Matches$all),]
DC.Case.Matches = DC.Case.Matches[,1:(ncol(DC.Case.Matches)-1)]
write.csv(x = DC.Case.Matches, file = "DC.Case.Matches.csv", row.names = F)

