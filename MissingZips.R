#Find missing zips

MissingZips = function (DirectCert, Students, zipVars){
  
  DCzips.snap = unique(DirectCert$Zip[DirectCert$Case.Type == "SNAP"])
  DCzips.medi = unique(DirectCert$Zip[DirectCert$Case.Type == "MEDICAID"])
  PSzips = unique(unlist(c(SMS[,zipVars])))
  PSzips = PSzips[!is.na(PSzips)]
  
  missing.snap = PSzips[!(PSzips %in% DCzips.snap)]
  missing.medi = PSzips[!(PSzips %in% DCzips.medi)]
  
  missing.snap = missing.snap[order(missing.snap)]
  missing.medi = missing.medi[order(missing.medi)]
  
  
  if(length(missing.snap)+length(missing.medi) == 0){
    print("No missing zip codes")
  }else{
    if(length(missing.snap) > 0){
      print(paste0(c("The following zip codes are missing from the SNAP Direct Cert files: ", missing.snap), collapse = " "))
    }
    if(length(missing.medi) > 0){
      print(paste0(c("The following zip codes are missing from the MEDICAID Direct Cert files: ", missing.snap), collapse = " "))
    }
  }
  
  missing.all = unique(c(missing.medi, missing.snap))
  missing.all = missing.all[order(missing.all)]
  
  gc()
  
  return(missing.all)
}