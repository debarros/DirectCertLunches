#Matching student information from the SIS (student information system) to exports from the Direct Certification by ZIP code export files

#This should be made into a Shiny app
#The user should be able to indicate the categories of variables

#Right now, the DirectCertMatches.xlsx file is passed to someone who knows the students well.
#That person uses the first column on the Potential Matches tab to indicate whether each unbolded row is:
# X - the student in bold
# S - a sibling of the student in bold
#That person then return the file, which gets saved as "matched records.xlsx"
#Analyze matches is then run
#The whole program is then rerun.
#The process is iterative, so that it only stops when the person who checks the DirectCertMatches.xlsx file finds no new matches
#The Case.Matches tab of DirectCertMatches.xlsx is then used to calculate the SNAP and Medicaid numbers

#CaseMatches.csv holds the known cases that have been matched to students in the past.  
#It should be modified to include the date of extract.
#That way, a function can be written to purge it of cases that are very old.

#The AnalyzeHandMatches code should be mofied to allow for one more indicator value in the first column
# ? - a potential match that requires more research
#Then, the AnalyzeHandMatches code can automatically generate an output file that can be used by whoever does the additional research.

#load necessary functions
source("functions.R")

messageLevel = 5





#Load the data sets from PowerSchool (SMS), the Direct Certification file (DirectCert), and the known matches (DC.Case.Matches)
SMS <- read.csv("smsExport.csv", stringsAsFactors = FALSE)
DirectCert <- read.csv("DirectCertCases.csv", stringsAsFactors = FALSE)
DC.Case.Matches <- read.csv("DC.Case.Matches.csv", stringsAsFactors = FALSE)

#Verify the direct cert file
dcVars = c("Case.Name.Guardian","First.Name","Last.Name","DOB","Age","Street","City","State","Zip","Case.Type","Case.Num","Mon.YYYY") #this is just all the variables in the DirectCert file
if(!identical(colnames(DirectCert), dcVars)){
  warning("Warning!  The columns in the Direct Cert File are not what was expected.")
}


#If this is not the first time through, load the hand matches and analyze them
handMatch = read.xlsx(xlsxFile = "matched records.xlsx", sheet = "Potential.Matches")
DC.Case.Matches = HandMatcher(handMatch, DC.Case.Matches)
write.csv(x = DC.Case.Matches, file = "DC.Case.Matches.csv", row.names = F)



#remove duplicates from direct cert file
DirectCert$all = apply(DirectCert, 1, paste, collapse = " ")
DirectCert = DirectCert[!duplicated(DirectCert$all),]
DirectCert = DirectCert[,1:(ncol(DirectCert)-1)]



#Establish the sets of variables from the SMS to be used, and the modified name formats to be used
#This needs to be modified to allow vectors of first names, middle names, and last names, instead of just one SMS variable for each
nameForms = c("RemoveCharacters", "SpaceCharacters", "RemoveAfterCharacters")
studentNameVars = list("First" = "First_Name", "Last" = "Last_Name", "Middle" = "Middle_Name")
streetVars = c("Street")
guardVars = c("Father", "Mother")
cityVars = c("City")
zipVars = c("Zip")
dobVars = c("DOB")


VariableSets = list(
  "studentNameVars" = studentNameVars,
  "streetVars" = streetVars,
  "guardVars" = guardVars,
  "cityVars" = cityVars,
  "zipVars" = zipVars,
  "dobVars" = dobVars
)


wb.output = MatchingProcess(DC.Case.Matches, DirectCert, SMS, dcVars, VariableSets, nameForms)


#From SMS, remove the students who are already matched in the DC.Case.Matches file
DC.Case.Matches.current = DC.Case.Matches[DC.Case.Matches$Case.Num %in% DirectCert$Case.Num,]
SMS$Matched = FALSE
SMS$Matched[SMS$Student_number %in% DC.Case.Matches.current$Student_Number] = TRUE
SMS.matched = DC.Case.Matches.current[DC.Case.Matches.current$Student_Number %in% SMS$Student_number[SMS$Matched],]
SMS.matched[,dcVars[!(dcVars %in% c(colnames(DC.Case.Matches.current)))]] = DirectCert[match(SMS.matched$Case.Num,DirectCert$Case.Num),dcVars[!(dcVars %in% c(colnames(DC.Case.Matches.current)))]]
SMS.matched[,colnames(SMS)[colnames(SMS) != "Student_number"]] = SMS[match(SMS.matched$Student_Number,SMS$Student_number),colnames(SMS)[colnames(SMS) != "Student_number"]]  
SMS = SMS[!SMS$Matched,1:(ncol(SMS)-1)]
row.names(SMS) = NULL
write.csv(SMS.matched, file = "CaseMatches.csv")


#remove students below minAge
minAge = 0
DirectCert = DirectCert[DirectCert$Age >= minAge,]

#Check for missing zip codes
missedZips = MissingZips(DirectCert, SMS, zipVars)
if(length(missedZips) > 0){
  View(SMS[SMS$X2nd_mailing_zip %in% missedZips | SMS$Mailing_Zip %in% missedZips | SMS$Zip %in% missedZips,])
}



#Change the DOB variables to be stored in date format
DirectCert$DOB = as.Date(DirectCert$DOB, format = "%m/%d/%Y")
SMS$DOB = as.Date(SMS$DOB, format = "%m/%d/%Y")

#Change the zip codes to be stored in character format
for (i in zipVars){SMS[,i] = as.character(SMS[,i])}

#Create versions of the student names that remove special characters like spaces, hyphens, or apostrophes
SMS = RemoveSpecialCharacters(SMS, Vars = studentNameVars, nameForms = nameForms, messageLevel = messageLevel)

#Create subsets of the student street addresses
SMS = SubsetWords(SMS, Vars = streetVars, Varname = "streetPart")

#Create parent name search fields
SMS = SubsetWords(SMS, Vars = guardVars, Varname = "gName")

#Create city search fields
SMS = SubsetWords(SMS, Vars = cityVars, Varname = "muni")

#Create zip search fields
SMS = SubsetWords(SMS, Vars = zipVars, Varname = "zipped")




#Create the matching array
Matcher = array(
  dim = c(nrow(SMS), nrow(DirectCert),0), 
  dimnames = list("PS" = rownames(SMS), "DC" = rownames(DirectCert),"Type" = c()))


#Build the matches
gc()
Matcher = Build.Primary(Matcher, SMS, DirectCert, studentNameVars, nameForms, messageLevel = messageLevel)
gc()


#Score the matches
MatchScores = Build.Secondary(Matcher, nameForms, messageLevel = messageLevel)
gc()

#Produce the output
outputWorkbook = Outputter(SMS.matched,DirectCert, SMS, MatchScores, studentNameVars, streetVars, guardVars, cityVars, messageLevel = messageLevel)
saveWorkbook(wb = wb.output, file = "newTestRun.xlsx", overwrite = T)

