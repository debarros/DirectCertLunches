#MatchingProcess.R

MatchingProcess = function(DC.Case.Matches, DirectCert, SMS, dcVars, VariableSets, nameForms, PotentialMatchCount = 5, messageLevel = 0){
  
  if(messageLevel > 0) message("running MatchingProcess function")
  
  if(messageLevel > 1) message("removing duplicates from direct cert file")
  #remove duplicates from direct cert file
  x = DirectCert
  x$all = apply(x, 1, paste, collapse = " ")
  x = x[!duplicated(x$all),]
  x = x[,1:(ncol(x)-1)]
  DirectCert = x
  
  if(messageLevel > 1) message("Pull out variable name sets")
  #Pull out variable name sets
  studentNameVars = VariableSets$studentNameVars 
  streetVars = VariableSets$streetVars
  guardVars = VariableSets$guardVars
  cityVars = VariableSets$cityVars
  zipVars = VariableSets$zipVars
  dobVars = VariableSets$dobVars

  if(!is.null(DC.Case.Matches)){
  if(messageLevel > 1) message("Remove students already in the DC.Case.Matches File")
  #From SMS, remove the students who are already matched in the DC.Case.Matches file
  DC.Case.Matches.current = DC.Case.Matches[DC.Case.Matches$Case.Num %in% DirectCert$Case.Num,]
  SMS$Matched = FALSE
  SMS$Matched[SMS$Student_number %in% DC.Case.Matches.current$Student_Number] = TRUE
  SMS.matched = DC.Case.Matches.current[DC.Case.Matches.current$Student_Number %in% SMS$Student_number[SMS$Matched],]
  SMS.matched[,dcVars[!(dcVars %in% c(colnames(DC.Case.Matches.current)))]] = DirectCert[match(SMS.matched$Case.Num,DirectCert$Case.Num),dcVars[!(dcVars %in% c(colnames(DC.Case.Matches.current)))]]
  SMS.matched[,colnames(SMS)[colnames(SMS) != "Student_number"]] = SMS[match(SMS.matched$Student_Number,SMS$Student_number),colnames(SMS)[colnames(SMS) != "Student_number"]]  
  SMS = SMS[!SMS$Matched,1:(ncol(SMS)-1)]
  row.names(SMS) = NULL
  } else {
    SMS.matched = NULL
  }
  
  if(messageLevel > 1) message("Format DOB variables")
  #Change the DOB variables to be stored in date format
  DirectCert$DOB = as.Date(DirectCert$DOB, format = "%m/%d/%Y")
  for(i in dobVars){SMS[,i] = as.Date(SMS[,i], format = "%m/%d/%Y")}
  
  if(messageLevel > 1) message("Format student name variables")
  #Create versions of the student names that remove special characters like spaces, hyphens, or apostrophes
  SMS = RemoveSpecialCharacters(SMS, Vars = studentNameVars, nameForms = nameForms, messageLevel = messageLevel - 1)
  
  if(messageLevel > 1) message("Format street variables")
  #Create subsets of the student street addresses
  SMS = SubsetWords(SMS, Vars = streetVars, Varname = "streetPart", messageLevel = messageLevel - 1)
  
  if(messageLevel > 1) message("Format guardian variables")
  #Create parent name search fields
  SMS = SubsetWords(SMS, Vars = guardVars, Varname = "gName")
  
  if(messageLevel > 1) message("Format city variables")
  #Create city search fields
  SMS = SubsetWords(SMS, Vars = cityVars, Varname = "muni")
  
  if(messageLevel > 1) message("Format ZIP variables")
  #Change the zip codes to be stored in character format
  for (i in zipVars){SMS[,i] = as.character(SMS[,i])}
  #Create zip search fields
  SMS = SubsetWords(SMS, Vars = zipVars, Varname = "zipped")
  
  #Create the matching array
  Matcher = array(
    dim = c(nrow(SMS), nrow(DirectCert),0), 
    dimnames = list("PS" = rownames(SMS), "DC" = rownames(DirectCert),"Type" = c()))
  
  #Build the matches
  gc()
  Matcher = Build.Primary(Matcher, SMS, DirectCert, studentNameVars, nameForms, dobVars, messageLevel = messageLevel)
  gc()
  
  
  #Score the matches
  MatchScores = Build.Secondary(Matcher, nameForms, messageLevel = messageLevel)
  gc()
  
  #Produce the output
  if(messageLevel > 1) message("Produce the output")
  outputWorkbook = Outputter(SMS.matched,DirectCert, SMS, MatchScores, studentNameVars, streetVars, guardVars, cityVars, zipVars, messageLevel = messageLevel, PotentialMatchCount)
  gc()
 
  if(messageLevel > 0) message("done MatchingProcess function")
   
  return(outputWorkbook)
}