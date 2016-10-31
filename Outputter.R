#Outputter.R

Outputter = function(SMS.matched, DirectCert, SMS, MatchScores, studentNameVars, streetVars, guardVars, cityVars, messageLevel = 0, n = 5){
  
  SMS.matched.out = SMS.matched[order(SMS.matched$Case.Type,SMS.matched$Student_Number, decreasing = T),]
  SMS.matched.out = SMS.matched.out[!duplicated(SMS.matched.out$Student_Number),]
  
  DirectCertOut = data.frame(
    Student = paste(DirectCert$First.Name, DirectCert$Last.Name, sep = " "),
    Guardian = DirectCert$Case.Name.Guardian,
    DOB = DirectCert$DOB,
    Address = paste(DirectCert$Street,DirectCert$City,DirectCert$Zip, sep = ", "),
    Case.Type = DirectCert$Case.Type,
    Case.Num = DirectCert$Case.Num,
    Score = "",
    ID = "",
    stringsAsFactors = F
    )
  row.names(DirectCertOut) = row.names(DirectCert)
  
  SMSout = data.frame(
    Student  = apply(SMS[,unlist(studentNameVars)], 1, paste, collapse = " "),
    Guardian = apply(SMS[,guardVars], 1, paste, collapse = ", "),
    DOB = SMS$DOB,
    Address =  apply(SMS[,c(streetVars, cityVars,zipVars)],1,paste,collapse = ", "),
    Case.Type = "",
    Case.Num = "",
    Score = "",
    ID = SMS$Student_number,
    stringsAsFactors = F
  )
  row.names(SMSout) = row.names(SMS)
  
  output = matrix(NA,0,ncol(DirectCertOut))
  colnames(output) = colnames(DirectCertOut)
  output = as.data.frame(output)
  for(i in colnames(output)){
    output[,i] = as.character(output[,i])
  }
  
  blankRow = data.frame(
    Student  = "",
    Guardian = "",
    DOB = as.Date(NA),
    Address =  "",
    Case.Type = "",
    Case.Num = "",
    Score = "",
    ID = "",
    stringsAsFactors = F
  )
  
  for (i in 1:nrow(SMS)){
    matchSet = rev(tail(sort(MatchScores[i,]),n))
    DC.rows = DirectCertOut[names(matchSet),]
    DC.rows$Score = as.character(matchSet)
    SMS.row = SMSout[i,]
    output = rbind.data.frame(output, SMS.row, stringsAsFactors = F) #add the SMS student info
    output = rbind.data.frame(output, DC.rows, stringsAsFactors = F) #add the potential matches from the Direct Cert file
    output = rbind.data.frame(output, blankRow, stringsAsFactors = F) #add a spacer row
  }
  
  output$CorrectMatch = ""
  output = output[,c(ncol(output),1:(ncol(output)-1))]
  
  wb = createWorkbook() # initialize the workbook
  
  addWorksheet(wb=wb, sheetName = "Potential.Matches")
  freezePane(wb, "Potential.Matches", firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, "Potential.Matches", cols = 1:ncol(output), widths = "auto", ignoreMergedCells = FALSE)
  setColWidths(wb, "Potential.Matches", cols = which(colnames(output)=="DOB"), widths = 11, ignoreMergedCells = FALSE)
  addStyle(wb, "Potential.Matches", createStyle(fontSize = 14, textDecoration = "bold"), rows = 1, cols = 1:ncol(output), gridExpand = T, stack = T)
  addStyle(wb, "Potential.Matches", createStyle(textDecoration = "bold"), rows = (1:nrow(SMSout))*(n+2) - n, cols = 1:ncol(output), gridExpand = T, stack = T)
  writeData(wb=wb, sheet = "Potential.Matches", x = output)
  
  addWorksheet(wb=wb, sheetName = "Case.Matches")
  freezePane(wb, "Case.Matches", firstActiveRow = 2, firstActiveCol = 8)
  setColWidths(wb, "Case.Matches", cols = 1:ncol(output), widths = "auto", ignoreMergedCells = FALSE)
  addStyle(wb, "Case.Matches", createStyle(textDecoration = "bold"), rows = 1, cols = 1:ncol(SMS.matched.out), gridExpand = T, stack = T)
  writeData(wb=wb, sheet = "Case.Matches", x = SMS.matched.out)
  
  saveWorkbook(wb = wb, file =  paste0("DirectCertMatches.xlsx"), overwrite = T)
}


