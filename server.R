#server.R

source("functions.R") #Load the necessary functions and stable data

shinyServer(function(input, output, session) {
  
  #Create variables to hold the input files
  SMS = reactive({ if(is.null(input$SMS)){return(NULL)
  } else read.csv(input$SMS$datapath, stringsAsFactors = F) }) 
  
  DirectCert = reactive({ 
    if(is.null(input$DirectCert)){
      x = NULL
    } else {
      x = read.csv(input$DirectCert$datapath, stringsAsFactors = F)
    } 
    return(x)
  }) 
  
  handMatch = reactive({ if(is.null(input$handMatch)){return(NULL)
  } else read.xlsx(xlsxFile = input$handMatch$datapath, sheet = "Potential.Matches") })
  
  DC.Case.Matches.raw = reactive({ if(is.null(input$DC.Case.Matches)){return(NULL)
  } else read.csv(input$DC.Case.Matches$datapath, stringsAsFactors = F) })
  
  
  #If there is is a handmatch file, use it to modify the case matches object
  DC.Case.Matches = reactive({
    if(is.null(handMatch)){ DC.Case.Matches.raw()
    } else HandMatcher(handMatch(), DC.Case.Matches.raw(), messageLevel())
  })
  
  
  #make the radio grid for selecting which SMS variables correspond to each DC variables
  output$VariableChooser = renderUI({tagList(MultiRadio(
    RowNames = colnames(SMS()), 
    ColumnNames = c("Unused", "First", "Last", "Middle", "Street","Guardian","City","ZIP","DOB"), 
    gridname = "Vars", 
    gridnameLast = T ))})
  
  #Assemble the names of all the variables selected
  VariableSets = reactive({
    if(messageLevel() > 2) message("creating VariableSets reactive Object")
    x = data.frame(VarName = colnames(SMS()), stringsAsFactors = F)
    x$type = NA_character_
    for(i in 1:nrow(x)){
      x$type[i] = input[[paste0(x$VarName[i],"Vars")]]
    }
    y = list(
      "studentNameVars" = list(
        "First" = x$VarName[x$type == "First"], 
        "Last" = x$VarName[x$type == "Last"], 
        "Middle" = x$VarName[x$type == "Middle"]),
      "streetVars" = x$VarName[x$type == "Street"],
      "guardVars" = x$VarName[x$type == "Guardian"],
      "cityVars" = x$VarName[x$type == "City"],
      "zipVars" = x$VarName[x$type == "ZIP"],
      "dobVars" = x$VarName[x$type == "DOB"])
    if(messageLevel() > 2) message("done creating VariableSets reactive Object")
    return(y)
  })
  
  
  
  parameter1 = reactive(sliderInput(
    inputId = "PotentialMatchCount", 
    label = "How many potential matches per student?", 
    min = 1, max = 10, value = 5))
  
  parameter2 = reactive(sliderInput(
    inputId = "messageLevel", 
    label = "What level of diagnostic messages should be generated?", 
    min = 0, max = 10, value = 2))
  
  output$ParameterSetter = renderUI(tagList(parameter1(), parameter2()))
  
  messageLevel = reactive({
    if(is.null(input$messageLevel)){
      return(5)
    } else {
      return(input$messageLevel)
    }
  })
  
  outbook = reactive({
    if(messageLevel() > 1){message("trying to create outbook reactive object")}
    if(is.null(DirectCert()) | is.null(SMS())){
      if(messageLevel() > 1){message("something is missing, not creating outbook reactive object")}
      x = NULL
    } else {
      if(messageLevel() > 1){message("creating outbook reactive object")}
      x = MatchingProcess(DC.Case.Matches(), DirectCert(), SMS(), dcVars, VariableSets(), nameForms, input$PotentialMatchCount, messageLevel = messageLevel())
    }
    if(messageLevel() > 1){message("done creating outbook reactive object")}
    return(x)
  })
  
  
  output$OutputDownloader = renderUI({
    print("e")
    if(is.null(outbook())){
      x = NULL
      print("f")
    } else {
      x = downloadButton('DirectCertMatches.downhandle', 'Download the output')
      print("g")
    }
    print("h")
    return(x)
  })
  
  
  output$DirectCertMatches.downhandle <- downloadHandler(
    filename = "DirectCertMatches.xlsx",
    content = function(file) {saveWorkbook(outbook(), file, overwrite = T)} )
  
  
})