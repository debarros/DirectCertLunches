#BuildSecondary.R
# This needs to be modified to account for the following new paramters:
#   studentNameVars
#   nameForms


Build.Secondary = function (Matcher, nameForms, messageLevel = 0){
  
  if(messageLevel > 0){message("Beginning Build.Secondary function")}
  
  if(messageLevel > 1){message("set up variables")}
  
  matches = dimnames(Matcher)[3][[1]]
  match.DC = list()
  match.SMS = list()
  
  # Student Name section ####
  
  if(messageLevel > 1){message("create lists of first name and last name matching pairs for the direct cert file")}
  
  match.DC$Fi = grep(" First.Name",matches)
  match.DC$La = grep(" Last.Name",matches)
  
  if(messageLevel > 1){message("create lists of first name and last name matching pairs for the SMS file")}
  
  match.SMS$Fi = grep("First_Name ",matches) #set of matches involving SMS first name
  match.SMS$FiPart = Vgrep(pattern = paste0("First",nameForms),matches)
  match.SMS$La = grep("Last_Name ",matches)
  match.SMS$LaPart = Vgrep(pattern = paste0("Last",nameForms),matches)
  
  if(messageLevel > 1){message("compile list of name matches")}
  
  NameMatches = list()
  NameMatches$FF = c(intersect(match.DC$Fi, match.SMS$Fi), intersect(match.DC$Fi, match.SMS$FiPart)) #SMS any first name to DC first name
  NameMatches$LL = c(intersect(match.DC$La, match.SMS$La), intersect(match.DC$La, match.SMS$LaPart)) #SMS any last name to DC last name
  NameMatches$OF = setdiff(match.DC$Fi, NameMatches$FF) #SMS any non first name to DC first name
  NameMatches$OL = setdiff(match.DC$La, NameMatches$LL) #SMS any non last name to DC first name
  NameMatches$BC = c(intersect(match.DC$Fi, match.SMS$Fi), intersect(match.DC$La, match.SMS$La)) #SMS exact to DC exact, first and last ("Both Correct")
  rm(list = c("match.DC","match.SMS"))
  gc()
  
  # Scoring section ####
  if(messageLevel > 1){message("calculate match scores")}
  
  ##Score by guardian name: number of matches
  if(messageLevel > 2){message("guardian match scores")}
  Guardian = apply(X = Matcher[,,grep(" Case.Name.Guardian",matches)], MARGIN =c(1,2), FUN = sum) 
  
  ##Score by zip code: at least one match
  if(messageLevel > 2){message("ZIP match scores")}
  ZIPs = apply(X = Matcher[,,grep(" Zip",matches)], MARGIN =c(1,2), FUN = any) 
  
  ##Score by City: at least one match
  if(messageLevel > 2){message("City match scores")}
  City =  apply(X = Matcher[,,grep(" City",matches)], MARGIN =c(1,2), FUN = any)
  
  ##Score by Date: 1 pt for whole DOB, plus .1 pts for each matching part 
  if(messageLevel > 2){message("DOB match scores")}
  
  if(messageLevel > 3){message("DateParts")}
  DateParts = apply(X = Matcher[,,grep(" DOB",matches)], MARGIN =c(1,2), FUN = sum)
  
  if(messageLevel > 3){message("DateWhole")}
  DateWhole = apply(X = Matcher[,,grep(" DOB",matches)], MARGIN =c(1,2), FUN = all)
  
  if(messageLevel > 3){message("DOB total")}
  DOB = DateWhole + .1*DateParts
  
  rm(list = c("DateWhole","DateParts"))
  gc()
  
  
  ##Score by Name: this is complicated 
  if(messageLevel > 2){message("name match scores")}
  
  if(messageLevel > 3){message("Set points and weights")}
  ExactPoints = 1
  PartialPoints = .5
  MatchedWeight = .2
  MissedWeight = .1
  
  if(messageLevel > 3){message("First to First")}
  FF = ExactPoints*Matcher[,,NameMatches$FF[1]] + PartialPoints*apply(X = Matcher[,,NameMatches$FF[2:length(NameMatches$FF)]], MARGIN =c(1,2), FUN = sum)
  
  if(messageLevel > 3){message("Other to first")}
  OF = ExactPoints*apply(X = Matcher[,,NameMatches$OF], MARGIN =c(1,2), FUN = sum)
  
  if(messageLevel > 3){message("Last to Last")}
  LL = ExactPoints*Matcher[,,NameMatches$LL[1]] + PartialPoints*apply(X = Matcher[,,NameMatches$LL[2:length(NameMatches$LL)]], MARGIN =c(1,2), FUN = sum)
  
  if(messageLevel > 3){message("Other to Last")}
  OL = ExactPoints*apply(X = Matcher[,,NameMatches$OL], MARGIN =c(1,2), FUN = sum)
  
  if(messageLevel > 3){message("Both Correct")}
  BC = ExactPoints*apply(X = Matcher[,,NameMatches$BC], MARGIN =c(1,2), FUN = all)
  
  if(messageLevel > 3){message("Total name score")}
  NameMatch = BC + MatchedWeight*(FF + LL) + MissedWeight*(OF + OL)
  
  rm(list = c("FF","OF","LL","OL","BC"))
  gc()
  
  
  #Score by Street: number of matches 
  if(messageLevel > 2){message("street match scores")}
  Street = apply(X = Matcher[,,grep(" Street",matches)], MARGIN =c(1,2), FUN = sum) 
  
  #Add up the scores 
  if(messageLevel > 2){message("add up the match scores")}
  MatchScores = .5*Guardian + ZIPs + City + DOB + NameMatch + .2*Street
  dimnames(MatchScores) = dimnames(Matcher)[1:2]
  
  if(messageLevel > 0){message("Ending Build.Secondary function")}
  
  return(MatchScores)
}