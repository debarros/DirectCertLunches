#functions
#This loads all of the functions to be used

library(openxlsx)
library(abind)

#These functions are large, and so are stored in separate files
source("Outputter.R")
source("SubsetWords.R")
source("RemoveSpecialCharacters.R")
source("MissingZips.R")
source("Vcomp.R")
source("DataComp.R")
source("Astack.R")
source("Build.Primary.R")
source("DataGrepl.R")
source("Build.Secondary.R")
source("DataDate.R")
source("Vgrep.R")
source("Vgrepl.R")
source("Splitter.R")


#This function allows finding the intersection of several sets
intersectSeveral <- function(...) { Reduce(intersect, list(...)) } 

#This function sets the length of vector x to be y
setlength <- function(x,y){length(x) <- y
  return(x)}

#This function drops all elements of a vector that have fewer than 2 characters
one.drop = function(x){x[nchar(x) > 1]}

#This functions sorts a character vector by the lengths of the elements, longest first
SortLength = function(x){x[sort(nchar(x), decreasing = TRUE, index.return = TRUE)[[2]]]}

#This function converts all NA's in a character vector to empty values
na.to.empty = function(x){x[which(is.na(x))] = ""
return(x)}

