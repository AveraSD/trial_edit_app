
# the first part info and disease
fstUI <<- fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),
  br(),
  
  wellPanel(
  h5(strong("This section enables modification for trails Meta information, Disease stage and Conditions")),
  br(),
 h5("Meta Information Table"),
  div(style = "margin-top: 20px;"),
 column(4,
        h5(strong("Please Click on Save to verified information")) ),
 column(4,
        actionButton(inputId = "saveInfo",label = "Save Verfiy",icon = shiny::icon("file"),class = "btn-danger",size = "md") ),
 column(4),
  DTOutput('trlinfo_table',width = "100%"),
  br(),
  
  ),
  br(),
  br(),
  
  
  wellPanel(
    fluidRow(
      h5(strong("Document table")),
      div(style = "margin-top: 20px;"),
      column(4,
             h5(strong("Once Information verified Click on SAVE: " ))
      ),
      column(4,
             actionButton(inputId = "saveDoc",label = "Save Verfiy",icon = shiny::icon("file"),class = "btn-danger",size = "md")),
      column(4),
      br(),
      DTOutput('trldoc_table',width = "100%"),
      br()
    )),

br(),
br(),


  
  wellPanel(
    fluidRow(
   h5(strong("Disease Stage table")),
   div(style = "margin-top: 20px;"),
   column(4,
          h5(strong("Once Information verified Click on SAVE: " ))
          ),
   column(4,
          actionButton(inputId = "saveDis",label = "Save Verfiy",icon = shiny::icon("file"),class = "btn-danger",size = "md")),
   column(4),
   br(),
   br(),
   br(),
   column(10,
    textOutput("disSum")
    ),
    br(),
   br(),
    DTOutput('trldis_table',width = "100%"),
    br(),
    column(4,
    actionButton(inputId = "disAdd",label = "New Entry",icon = shiny::icon("plus"),width = '200px',class = "btn-success",size = "lg")),
    column(4)
    ))
))


secdUI <<- fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),
  br(),
  h5(strong("Section enables modification for cohort information")),
  br(),
  wellPanel(
    column(4,
           h5(strong("Once Information verified Click on SAVE:")) ),
    column(4,
           actionButton(inputId = "saveChrt",label = "Save Verify",icon = shiny::icon("file"),class = "btn-danger",size = "md") ),
    column(4),
    br(),
    br(),
    br(),
    DTOutput('trlCrt_table',width = "100%"),
    br(),
   
  ),
  
  h5(strong("Section enables modification for cohorts Biomarker")),
  wellPanel(
      column(4,
             h5(strong("Once Information verified Click on SAVE:")) ),
      column(4,
             actionButton(inputId = "saveBio",label = "Save Verify",icon = shiny::icon("file"),class = "btn-danger",size = "md")),
      column(4),
      br(),
    br(),
    br(),
    DTOutput('trlBio_table',width = "100%"),
    br(),
    actionButton(inputId = "disBio",label = "New Entry",icon = shiny::icon("plus"),width = '200px',class = "btn-success",size = "lg") 
  
)
))
