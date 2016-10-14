#Build.Primary.R

#This should be broken up into those matches that require DataGrepl and those that can just use DatComp

Build.Primary = function(Matcher, SMS, DirectCert, messageLevel = 0){
  if(messageLevel > 0) message("starting Build.Primary function")

  if(messageLevel > 1) message("set guardnames and streetnames")
  
  guardnames = length(grep(pattern = "gName", x = colnames(SMS), ignore.case = T)) #how many guardian name variable in SMS?
  streetnames = length(grep(pattern = "streetPart", x = colnames(SMS), ignore.case = T)) #how many street variable in SMS?
  
  if(messageLevel > 1) message("build sets of variables")
  
  sets = list()
  sets$ParentName = list("PS" = paste0("gName",1:guardnames), "DC" = c("Case.Name.Guardian"))
  sets$Studentname = list("PS" = c("First_Name","Last_Name","Middle_Name","FirstRemoveCharacters","LastRemoveCharacters","FirstSpaceCharacters","LastSpaceCharacters", "FirstRemoveAfterCharacters","LastRemoveAfterCharacters"), "DC" = c("First.Name","Last.Name"))
  sets$Zip = list("PS" = c("X2nd_mailing_zip", "Mailing_Zip", "Zip"), "DC" = c("Zip"))
  sets$Street = list("PS" = paste0("streetPart",1:streetnames), "DC" = c("Street"))
  sets$City = list("PS" = c("Mailing_City","City","X2nd_mailing_city"), "DC" = c("City"))
  sets$DOB = list("PS" = c("Year","Month","Day"), "DC" = c("DOB"))
  
  if(messageLevel > 1) message("make the data.frame showing all pairs of variables to be matched")
  
  y = data.frame(Var1 = character(), Var2 = character(), stringsAsFactors = F)
  
  for(i in 1:6){
    x = expand.grid(sets[[i]]$PS, sets[[i]]$DC, stringsAsFactors = F)
    y = rbind(y,x)
    gc()
  }

  M2 = Matcher
  
  if(messageLevel > 1) message("loop through all pairs of variables")
  
  for(i in 1:(nrow(y))){
    print(paste0("Match #",i))
    if(y[i,2] == "DOB") {
      z = DataDate(Matcher, SMS, DirectCert, "DOB", y[i,2], datePart = y[i,1], messageLevel = messageLevel - 1)
    } else {
      z = DataGrepl(Matcher, SMS, DirectCert, y[i,1], y[i,2], messageLevel = messageLevel - 1)
    }
    M2 = Astack(M2, z, messageLevel = messageLevel -1)
    gc()
  }
  gc()
  rm(list = "z")
  gc()
  
  if(messageLevel > 1) message("and now something with dimnames")
  
  v = dimnames(Matcher)[1:2]
  v$Type = paste(y$Var1," X ", y$Var2)
  dimnames(M2) = v
  
  if(messageLevel > 0) message("Ending Build.Primary function")
  
  return(M2)
}


