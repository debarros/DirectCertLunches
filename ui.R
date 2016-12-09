#ui.R

shinyUI(fluidPage(
  style = "padding: 0px;",
  mainPanel(
    width = 12,
    style = "padding: 0px;",
    tabsetPanel(
      tabPanel(title = "Load Data",
               fileInput("SMS", "Table from Student Management System (csv)"),
               fileInput("DirectCert", "Export from CNMS (csv)"),
               fileInput("handMatch", "Optional: Recent export file with matches identified (xlsx)"),
               fileInput("DC.Case.Matches", "Optional: Previous export file of known matches (csv)")
      ),
      tabPanel(title = "Choose Variables",
               uiOutput("VariableChooser")
               ),
      tabPanel(title = "Set parameters",
               uiOutput("ParameterSetter")
               ),
      tabPanel(title = "Download Output",
               uiOutput("OutputDownloader")
               )
    )
  ) #end of mainPanel
)) #end of shinyUI 

