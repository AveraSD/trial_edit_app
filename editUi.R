
# the first part info and disease
fstUI <<- fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),
  br(),
  
  wellPanel(
  h5(strong("This section enables modification for trails Meta information, Disease stage and Conditions")),
  br(),
  "Meta Information Table",
  div(style = "margin-top: 20px;"),
  DTOutput('trlinfo_table',width = "100%"),
  br(),
  column(4,
  h5(strong("Please Click on Save to verified information")) ),
  column(4,
  actionButton(inputId = "saveInfo",label = "SAVE",icon = shiny::icon("plus"),class = "btn-success",size = "md") ),
  column(4)
  ),
  br(),
  br(),
  
  
  wellPanel(
    fluidRow(
      h5(strong("Document table")),
      column(4,
             h5(strong("Once Information verified Click on SAVE: " ))
      ),
      column(4,
             actionButton(inputId = "saveDoc",label = "Save Verfiy",icon = shiny::icon("file"),class = "btn-danger",size = "md")),
      column(4),
      br(),
      DTOutput('trldoc_table'),
      br()
    )),

br(),
br(),


  
  wellPanel(
    fluidRow(
   h5(strong("Disease Stage table")),
   column(4,
          h5(strong("Once Information verified Click on SAVE: " ))
          ),
   column(4,
          actionButton(inputId = "saveDis",label = "Save Verfiy",icon = shiny::icon("file"),class = "btn-danger",size = "md")),
   column(4),
   br(),
    textInput("disSum","Overall Disease Summary: ",width = "300px"),
    br(),
    DTOutput('trldis_table'),
    br(),
    column(4,
    actionButton(inputId = "disAdd",label = "New Entry",icon = shiny::icon("plus"),class = "btn-success",size = "md")),
    column(4)
    ))
))


secdUI <<- fluidRow(
  br(),
  h5(strong("Section enables modification for cohort information")),
  br(),
  wellPanel(
    br(),
    DTOutput('trlCrt_table'),
    br(),
    column(4,
           h5(strong("Please Click on Save to verified information")) ),
    column(4,
           actionButton(inputId = "saveChrt",label = "SAVE",icon = shiny::icon("plus"),class = "btn-success",size = "md") ),
    column(4)
  ),
  
  h5(strong("Section enables modification for cohorts Biomarker")),
  wellPanel(
    br(),
    
    DTOutput('trlBio_table'),
    br(),
    column(4,
           actionButton(inputId = "saveBio",label = "Save Edit",icon = shiny::icon("plus"),class = "btn-success",size = "md")),
    column(4,
    actionButton(inputId = "disBio",label = "ADD",icon = shiny::icon("plus"),class = "btn-success",size = "md") ),
    column(4)
  )

)
