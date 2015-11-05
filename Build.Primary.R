#Build.Primary.R

#This should be broken up into those matches that require DataGrepl and those that can just use DatComp

Build.Primary = function(Matcher, SMS, DirectCert){
  
  guardnames = length(grep(pattern = "gName", x = colnames(SMS), ignore.case = T)) #how many guardian name variable in SMS?
  streetnames = length(grep(pattern = "streetPart", x = colnames(SMS), ignore.case = T)) #how many street variable in SMS?
  
  sets = list()
  sets$ParentName = list("PS" = paste0("gName",1:guardnames), "DC" = c("Case.Name.Guardian"))
  sets$Studentname = list("PS" = c("First_Name","Last_Name","Middle_Name","FirstRemoveCharacters","LastRemoveCharacters","FirstSpaceCharacters","LastSpaceCharacters", "FirstRemoveAfterCharacters","LastRemoveAfterCharacters"), "DC" = c("First.Name","Last.Name"))
  sets$Zip = list("PS" = c("X2nd_mailing_zip", "Mailing_Zip", "Zip"), "DC" = c("Zip"))
  sets$Street = list("PS" = paste0("streetPart",1:streetnames), "DC" = c("Street"))
  sets$City = list("PS" = c("Mailing_City","City","X2nd_mailing_city"), "DC" = c("City"))
  sets$DOB = list("PS" = c("Year","Month","Day"), "DC" = c("DOB"))
  
  y = data.frame(Var1 = character(), Var2 = character(), stringsAsFactors = F)
  
  for(i in 1:6){
    x = expand.grid(sets[[i]]$PS, sets[[i]]$DC, stringsAsFactors = F)
    y = rbind(y,x)
  }

  M2 = Matcher
  
  for(i in 1:(nrow(y)-3)){
    print(paste0("Match #",i))
    z = DataGrepl(Matcher, SMS, DirectCert, y[i,1], y[i,2])
    M2 = Astack(M2, z)
  }
  
  for(i in (nrow(y)-2):nrow(y)){
    print(paste0("Match #",i))
    z = DataDate(Matcher, SMS, DirectCert, "DOB", y[i,2], datePart = y[i,1])
    M2 = Astack(M2, z)
  }
  
  
  v = dimnames(Matcher)[1:2]
  v$Type = q[c(1:43,45,46,44)] = paste(y$Var1," X ", y$Var2)
  dimnames(M2) = v
  return(M2)
}