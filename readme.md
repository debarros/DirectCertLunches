#DirectCertLunches
by Paul de Barros

##Intro and purpose
This repository is used for matching student information from the SMS (student management system) to exports from the Direct Certification by ZIP code export files.
In NY State, this is valuable when trying to identify students who qualify for free or reduced price lunch (FRPL).

##Current state and future plans
Currently, the code is set up specific to PowerSchool, and specific to the particular variables my organization uses in PowerSchool.
Ideally, I'd like to make it more flexible.
For each variable in the Direct Cert files, you should be able to select which variables in your SMS export might contain related information.

Also, I would like to eventually turn this into a Shiny App and host it on ShinyApps.io.  We'll see how that goes.

##Some conventions
###messagelevel
I use a `messageLevel` parameter in a lot of functions.  
This is a way to specify just how much information gets sent to the console when a function is run.  
`messageLevel = 0` indicates that you don't want any messages.  
`messageLevel = 1` indicates that you want the announcement that a function is beginning or ending, and perhaps a status update in the middle of a long function.  
A long functions may have additional message levels.
A status message that occurs within a loop will generally be 1 step higher than one just outside that loop.
When a function calls another function, it generally decreases messageLevel by 1, so that subfunctions produce less console output.
The function within function decrement is something I'd like to turn into a parameter, but I don't know that it's very important.

###File structure
For the most part, I put each function in its own file.  The exceptions are 

1. very short functions, which are include in the functions.R file
1. closely related functions, which are kept together in a file

