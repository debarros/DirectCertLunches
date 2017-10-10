# NewMethod.R
source("functions.R")
library(dBtools)
library(data.table)

# get the file of state matches downloaded from the nyssis website
# You must open the file in excel, save it, and close it again before importing it here

# Load all of them from this year and then combine them into one data.frame
NyssisFile1 = read.xlsx(xlsxFile = "\\\\stuthin2/data/2017-2018\\lunch\\nyssis_lunch (2017-10-05).xlsx", sheet = 1)
NyssisFile2 = read.xlsx(xlsxFile = "\\\\stuthin2/data/2017-2018\\lunch\\nyssis_lunch (2017-10-09).xlsx", sheet = 1)
NyssisFile3 = read.xlsx(xlsxFile = "\\\\stuthin2/data/2017-2018\\lunch\\nyssis_lunch (2017-10-10).xlsx", sheet = 1)
NyssisFile = rbindlist(l = list(NyssisFile1,NyssisFile2,NyssisFile3))
NyssisFile = NyssisFile[order(NyssisFile$Certification.Method, decreasing = T),]
rownames(NyssisFile) = NULL
NyssisFile = NyssisFile[!duplicated(NyssisFile$Local.ID),]

# generate the enrollment and student lite extracts from PowerSchool and load them here
EnrollmentExtract = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
StudentLiteExtract = read.csv(file = file.choose(), header = F, stringsAsFactors = F)

# Are there any rows that contain duplicate ID's?
sum(duplicated(NyssisFile$Local.ID))

# How many of this year's students appear in the NYSSIS extract?
sum(StudentLiteExtract$V4 %in% NyssisFile$Local.ID)

# Which of this year's students do not appear in the NYSSIS extract?
StudentsToCheck = setdiff(StudentLiteExtract$V4, NyssisFile$Local.ID)
length(StudentsToCheck)
write(x = paste0(StudentsToCheck, collapse = ","), file = "studentsToCheck.txt")

# Go to the current working directory, open the text file, and search powerschool for those students.  
# Export them with all relevant fields.
# Paste the data into an excel workbook

# This next section creates the bulk upload file to be submitted to the NYSSIS website
CheckStuData = read.xlsx(xlsxFile = "studentsToCheck (2017-10-05).xlsx")
CheckStuData$DOB = as.Date(CheckStuData$DOB, origin = "1899-12-30")
for(i in 1:ncol(CheckStuData)){
  CheckStuData[,i] = as.character(CheckStuData[,i])
  CheckStuData[,i] = dBtools::na.to.empty(CheckStuData[,i])
}

CheckStuData$guardian2 = paste0(CheckStuData$Guardian_FN," ", CheckStuData$Guardian_LN)
CheckStuData$guardian2[nchar(CheckStuData$guardian2) == 1] = ""

outputVars = c("BedsCode","StudentID","LastName","FirstName","DOB","Gender",
  "Address1","Address2","City","State","ZIP","Phone","Guardian1","Guardian2")

zipVars = c("2nd_mailing_zip","Zip","Mailing_Zip")
fnVars = c("First_Name")
lnVars = "Last_Name"
dobVars = "DOB"
addrVars = c("2nd_mailing_street", "Mailing_Street","Street")
cityVars = c("Mailing_City", "City", "2nd_mailing_city")
phonVars = c("Home_Phone","Emerg_Phone_1","Emerg_Phone_2","StudentCoreFields.emerg_3_phone","StudentCoreFields.father_home_phone",
             "StudentCoreFields.guardiandayphone","StudentCoreFields.fatherdayphone","StudentCoreFields.mother_home_phone",
             "StudentCoreFields.motherdayphone","U_STUDENT_ENROLLMENT_PAGE.emerg_cell_phone_1","U_STUDENT_ENROLLMENT_PAGE.EMERG_CELL_PHONE_2",
             "U_STUDENT_ENROLLMENT_PAGE.EMERG_CELL_PHONE_3","U_STUDENT_ENROLLMENT_PAGE.emerg_work_phone_1",
             "U_STUDENT_ENROLLMENT_PAGE.EMERG_WORK_PHONE_2","U_STUDENT_ENROLLMENT_PAGE.EMERG_WORK_PHONE_3",
             "U_STUDENT_ENROLLMENT_PAGE.father_cell_phone","U_STUDENT_ENROLLMENT_PAGE.mother_cell_phone",
             "U_STUDENT_ENROLLMENT_PAGE.step_cell_phone","U_STUDENT_ENROLLMENT_PAGE.STEP_HOME_PHONE","U_STUDENT_ENROLLMENT_PAGE.stepdayphone")
guardVars = c("Father", "guardian", "Mother", "guardian2")

studentList = vector(mode = "list", length = nrow(CheckStuData))

for(i in 1:length(studentList)){
  x = vector(mode = "list", length = length(outputVars))
  names(x) = outputVars
  x$BedsCode = "010100860907"
  x$StudentID = CheckStuData$Student_number[i]
  x$Gender = "M"
  x$State = "NY"
  x$LastName = unlist(unique(c(CheckStuData[i,lnVars])))
  x$FirstName = unlist(unique(c(CheckStuData[i,fnVars])))
  x$DOB = unlist(unique(c(CheckStuData[i,dobVars])))
  x$Address1 = unlist(unique(c(CheckStuData[i,addrVars])))
  x$City = unlist(unique(c(CheckStuData[i,cityVars])))
  x$ZIP = unlist(unique(c(CheckStuData[i,zipVars])))
  x$Phone = unlist(unique(c(CheckStuData[i,phonVars])))
  x$Guardian1 = unlist(unique(c(CheckStuData[i,guardVars])))
  for(j in 1:length(x)){
    if(!(names(x)[j] %in% c("Guardian2","Address2"))){
      allofem = x[[j]]
      allofem = allofem[nchar(allofem) > 0]
      x[[j]] = allofem  
    } 
  }
  x$Guardian2 = ""
  x$Address2 = ""
  studentList[[i]] = x
}

studentFrames = vector(mode = "list", length = length(studentList))
for(i in 1:length(studentFrames)){
  studentFrames[[i]] = expand.grid(studentList[[i]])
}

studentFrames = rbindlist(studentFrames)

write.table(studentFrames, file = "bulkupload.txt", row.names = F, col.names = F, sep = "|", dec = ".", quote = F)

# Upload the file bulkupload.txt to the NYSSIS web interface
# Wait 24 hours, then go back.  Click on the number of records next to the upload.  Download the results.
# Go through the results and find matches.  When matches are found, enter them on the Results tab.
# Once the results tab has been populated, continue:

bulkMatches = read.xlsx(xlsxFile = "\\\\stuthin2/data/2017-2018/lunch/Bulk Search Results (2017-10-10).xlsx", sheet = "Results")
NyssisMatches = NyssisFile[NyssisFile$Local.ID %in% StudentLiteExtract$V4,c("Local.ID", "P12.First.Name","P12.Last.Name", "Certification.Method","Case.Numbers")]
colnames(NyssisMatches)
allMatches = rbindlist(l = list(NyssisMatches, bulkMatches))
summary(allMatches)
summary(factor(allMatches$Certification.Method))
write.csv(x = allMatches, file = "allmatches.csv")
write.csv(x = StudentLiteExtract[StudentLiteExtract$V4 %in% setdiff(StudentLiteExtract$V4, allMatches$Local.ID),4:8], file = "unmatchedStudents.csv")

# Inform the relevant people about the number of matches.
# Send the two csv's to whoever needs them.
# The students in unmatchedStudents.csv need to submit lunch forms
