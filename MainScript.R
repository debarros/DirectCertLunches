#Matching student information from the SIS (student information system) to exports from the Direct Certification by ZIP code export files

#This should be made into a Shiny app
#The user should be able to indicate the categories of variables
#Once that is done, it should be published to github and shinyapps.io

#load necessary functions
source("functions.R")

#Load the data sets from PowerSchool (SMS) and the Direct Certification file (DirectCert)
SMS <- read.csv("~/DirectCertLunches/GTHstudents.csv", stringsAsFactors = FALSE)
DirectCert <- read.csv("~/DirectCertLunches/DirectCertCasesNov2015.csv", stringsAsFactors = FALSE)

#remove students below minAge
minAge = 12
DirectCert = DirectCert[DirectCert$Age >= minAge,]

#Check for missing zip codes
MissingZips(DirectCert, SMS)

#Change the DOB variables to be stored in date format
DirectCert$DOB = as.Date(DirectCert$DOB, format = "%m/%d/%Y")
SMS$DOB = as.Date(SMS$DOB, format = "%m/%d/%Y")

#Create versions of the student names that remove special characters like spaces, hyphens, or apostrophes
SMS = RemoveSpecialCharacters(SMS)

#Create subsets of the student street addresses
SMS = SubsetStreets(SMS)

#Create parent name search fields
SMS = BreakNames(SMS)



#Create the matching array
Matcher = array(
  dim = c(nrow(SMS), nrow(DirectCert),0), 
  dimnames = list("PS" = rownames(SMS), "DC" = rownames(DirectCert),"Type" = c()))


#Build the primary matches
Matcher = Build.Primary(Matcher, SMS, DirectCert)
Matcher = M2

#Discard 0 matches
#MatchCounts = apply(X = Matcher, MARGIN =c(3), FUN = sum)
#M3 = Matcher[,,MatchCounts > 0]

#Build Secondary matches
MatchScores = Build.Secondary(Matcher)

#Build the output
output = data.frame(NA)
output[,colnames(SMS[,1:21])] = NA
output = output[-1,-1]
i = 1

for (i in 1:12){
  DirectCert[,i] = as.character(DirectCert[,i])
}

for (i in 1:21){
  SMS[,i] = as.character(SMS[,i])
}

q = rep("", times = 21)

for (i in 1:nrow(SMS)){
  x = DirectCert[names(tail(sort(MatchScores[i,]),5)),]
  x[,13:21] = NA
  colnames(x) = colnames(output)
  y = SMS[i,1:21]
  output = rbind(output, y)
  output = rbind(output, x)
  output = rbind(output, q)
}

write.csv(output, file = "DirectCertMatches.csv")


