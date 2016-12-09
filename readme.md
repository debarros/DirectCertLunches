#DirectCertLunches
by Paul de Barros

##Intro and purpose
This repository is used for matching student information from the SMS (student management system) to exports from the Direct Certification by ZIP code export files.
In NY State, this is valuable when trying to identify students who qualify for free or reduced price lunch (FRPL).

##Current state and future plans
* This has now been converted into a shiny app, available at https://debarros.shinyapps.io/DirectCertLunches/
* Currently, it takes a really, really long time to run, particularly if the data sets are large.
The slow speed might be improved by creating compiled versions of vgrepl, datagrepl, etc.
The speed might also be improved by making use of parallel processing.
* The program tends to exceed the memory capacity of a typical computer, and is best run on a server.  This could be fixed by forcing it to break the student list down into chunks and run the process on each chunk, saving the results to disk in order to free up memory.  The results could then be combined at the end.
* It needs to be modified to allow for multiple variables associated with each of the following student variables: DOB, first name, last name
* The output should have a drop-down box or restricted input for the first column
* The user should be able to select what matching functions to use.
* The available matching functions should include phonetic algorithms (e.g. Soundex).  The R phonics package looks good for this.
* There should also be a set of exemplar files.  These could be downloaded from the first tab in the interface.
* The shiny app should also have instructions, possibly located in a separate Instructions tab.


##Some conventions
###messagelevel
I use a `messageLevel` parameter in a lot of functions.  
This is a way to specify just how much information gets sent to the console when a function is run.  
`messageLevel = 0` indicates that you don't want any messages.  
`messageLevel = 1` indicates that you want the announcement that a function is beginning or ending, and perhaps a status update in the middle of a long function.  
Long functions may have additional message levels.
A status message that occurs within a loop will generally be 1 step higher than one just outside that loop.
When a function calls another function, it generally decreases messageLevel by 1, so that subfunctions produce less console output.
The function within function decrement is something I'd like to turn into a parameter, but I don't know that it's very important.

###File structure
For the most part, I put each function in its own file.  The exceptions are 

1. very short functions, which are include in the functions.R file
1. closely related functions, which are kept together in a file

