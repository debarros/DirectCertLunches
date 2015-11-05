#BuildSecondary.R

Build.Secondary = function (Matcher){
  
  matches = dimnames(Matcher)[3][[1]]
  match.DC = list()
  match.SMS = list()
  
  match.DC$Fi = grep(" First.Name",matches)
  match.DC$La = grep(" Last.Name",matches)
  
  match.SMS$Fi = grep("First_Name ",matches)
  match.SMS$FiPart = Vgrep(pattern = c("FirstRemoveCharacters ","FirstSpaceCharacters ","FirstRemoveAfterCharacters "),matches)
  
  match.SMS$La = grep("Last_Name ",matches)
  match.SMS$LaPart = Vgrep(c("LastRemoveCharacters ","LastSpaceCharacters ","LastRemoveAfterCharacters "),matches)
  
  NameMatches = list()
  NameMatches$FF = c(intersect(match.DC$Fi, match.SMS$Fi), intersect(match.DC$Fi, match.SMS$FiPart))
  NameMatches$LL = c(intersect(match.DC$La, match.SMS$La), intersect(match.DC$La, match.SMS$LaPart))
  NameMatches$OF = setdiff(match.DC$Fi, NameMatches$FF)
  NameMatches$OL = setdiff(match.DC$La, NameMatches$LL)
  NameMatches$BC = c(intersect(match.DC$Fi, match.SMS$Fi), intersect(match.DC$La, match.SMS$La))
  
  ##Score by guardian name: number of matches
  Guardian = apply(X = Matcher[,,grep(" Case.Name.Guardian",matches)], MARGIN =c(1,2), FUN = sum) 
  
  ##Score by zip code: at least one match
  ZIPs = apply(X = Matcher[,,grep(" Zip",matches)], MARGIN =c(1,2), FUN = any) 
  
  ##Score by City: at least one match
  City =  apply(X = Matcher[,,grep(" City",matches)], MARGIN =c(1,2), FUN = any)
  
  ##Score by Date: 1 pt for whole DOB, plus .1 pts for each matching part
  DateParts = apply(X = Matcher[,,grep(" DOB",matches)], MARGIN =c(1,2), FUN = sum)
  DateWhole = apply(X = Matcher[,,grep(" DOB",matches)], MARGIN =c(1,2), FUN = all)
  DOB = DateWhole + .1*DateParts
  
  
  ##Score by Name: 1 point for full name match
  FF = Matcher[,,NameMatches$FF[1]] + .5*apply(X = Matcher[,,NameMatches$FF[2:length(NameMatches$FF)]], MARGIN =c(1,2), FUN = sum)
  OF = apply(X = Matcher[,,NameMatches$OF], MARGIN =c(1,2), FUN = sum)
  LL = Matcher[,,NameMatches$LL[1]] + .5*apply(X = Matcher[,,NameMatches$LL[2:length(NameMatches$LL)]], MARGIN =c(1,2), FUN = sum)
  OL = apply(X = Matcher[,,NameMatches$OL], MARGIN =c(1,2), FUN = sum)
  BC = apply(X = Matcher[,,NameMatches$BC], MARGIN =c(1,2), FUN = all)
  NameMatch = BC + .2*(FF + LL) + .1*(OF + OL)
  
  #Score by Street: number of matches
  Street = apply(X = Matcher[,,grep(" Street",matches)], MARGIN =c(1,2), FUN = sum) 
  
  #Add up the scores
  MatchScores = .5*Guardian + ZIPs + City + DOB + NameMatch + .2*Street
  dimnames(MatchScores) = dimnames(Matcher)[1:2]
  return(MatchScores)
}