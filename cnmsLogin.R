#cnmsLogin.R
#This will eventually be code that will log into the cnms website to extract all of the needed direct cert files

library(RCurl)
library(XML)



loginurl1 = "http://portal.nysed.gov/pls/cn_blank/cn$.startup"

caLocation = character()
if(getNewCert){
  caLocation = ObtainNewCert(caLocation)
} else if(length(caLocation) == 0){
  caLocation = ObtainNewCert(caLocation)
} else if(!file.exists(caLocation)){
  caLocation = ObtainNewCert(caLocation)
}



# Set the "agent" (the info that tells web servers what browsers we are using)
agent="Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36" 

# Set RCurl parameters
Options <- curlOptions(
  cainfo = caLocation, 
  useragent = agent,
  followlocation = TRUE,
  verbose = FALSE,
  cookiejar = ""  
)
ScantronHandle = getCurlHandle()
curlSetOpt(.opts = Options, curl = ScantronHandle)  



x = getURI(url = loginurl1, curl = ScantronHandle)

htmlParse(x)

post


#ObtainNewCert.R

# This function downloads a new certification file
# If the caLocation exists, it uses that location
# Otherwise, it sets the folder path and file name to the default

ObtainNewCert = function(caLocation){
  if(length(caLocation) == 0){
    caLocation = "cacert.pem.crt"
  } 
  download.file(url = "http://curl.haxx.se/ca/cacert.pem", 
                destfile = caLocation, 
                method = "auto", 
                quiet = TRUE, 
                mode = "w")    
  return(caLocation)
}