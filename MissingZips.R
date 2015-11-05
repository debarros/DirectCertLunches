#Find missing zips

MissingZips = function (DirectCert, Students){
  
  DCzips = unique(DirectCert$Zip)
  PSzips = unique(c(Students$X2nd_mailing_zip, Students$Mailing_Zip, Students$Zip))
  
  missing = PSzips[!(PSzips %in% DCzips)]
  
  missing = missing[order(missing)]
  
  missing = missing[!is.na(missing)]
  
  if(length(missing) == 0){
    print("No missing zip codes")
  }else{
    print(paste0(c("The following zip codes are missing from the Direct Cert files: ", missing), collapse = " "))
  }
}