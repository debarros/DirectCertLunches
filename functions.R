#functions
#This loads all of the functions to be used

# Libraries ####
library(openxlsx)
library(abind)
library(shiny)

# Large Functions ####
#These functions are large, and so are stored in separate files
source("HandMatcher.R")
source("Outputter.R")
source("SubsetWords.R")
source("RemoveSpecialCharacters.R")
source("MissingZips.R")
source("Vcomp.R")
source("DataComp.R")
source("Build.Primary.R")
source("DataGrepl.R")
source("Build.Secondary.R")
source("DataDate.R")
source("Vgrep.R")
source("Vgrepl.R")
source("Splitter.R")
source("RadioGrid.R")
source("MatchingProcess.R")

# Small Functions ####
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


# Static Data and Options ####
#this is just all the variables in the DirectCert file
dcVars = c("Case.Name.Guardian","First.Name","Last.Name","DOB","Age","Street","City","State","Zip","Case.Type","Case.Num","Mon.YYYY") 
#these are the different nameForms to use
nameForms = c("RemoveCharacters", "SpaceCharacters", "RemoveAfterCharacters")
options(shiny.maxRequestSize=30*1024^2)