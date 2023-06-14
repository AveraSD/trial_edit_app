#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(data.table)
library(shinyjs)
library(bslib)
library(here)
library(tidyverse)
library(jsonlite)
library(glue)
library(config)
library(shinydashboard)
library(dplyr)
#library(semantic.dashboard)



# all the files to open
source(here("edit_tables.R"))
source(here("global.R"))
source(here("editUi.R"))
source(here("modal_scripts.R"))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
                    dashboardHeader(
                      title=span("Avera Genomics Trials",style = "color: white; font-weight: bold;"),titleWidth = 550
                      #theme = "grey_dark"
                      #windowTitle = "trialEdit"
                    ),
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  #theme = bslib::bs_theme(version = 5, bootswatch = "simplex"),

    # Application title
    
    

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(collapsed = FALSE, width = 550,
                     br(),
                     h5(strong(span("~~~~~~~~~~~~ SELECT TRIAL TO EDIT ~~~~~~~~~~~~~",style="color:black"))),
                     br(),
          DTOutput('trl_table')
        ),

        # Show a plot of the generated distribution
        dashboardBody(tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #505050;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #505050;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #505050;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #D3D3D3;
                              color: #000000;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #D3D3D3;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #D3D3D3;
                              }
                              '))),
                      
                      tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #D3D3D3 ;  color:black; width: 300PX;} 
                                       .tabbable > .nav > li[class=active] > a {background-color: #505050; color:white} ")),
                      box(
                        width = 12, background = "black",
                        column(6,
                               h5(strong("HIT THE SUBMIT BUTTON TO SAVE THE UPDATED TRIAL TO JSON FILE: "))
                        ),
                        column(4,
                               actionButton("addIn",label = " SUBMIT ", icon = shiny::icon("file"),size = "lg",width = '300px', class = "btn-danger")),
                      ),
                       box(title = "Values to copy paste for Cell Edits",
                         width = 12, status = "warning",
                         column(3,
                                
                                #  style = "display: inline-block;",
                                  #style = "margin-top: 10px;",
                                  pickerInput(
                                    inputId = "stageView",
                                    label = "Stage Available",
                                    choices = c("Stage I","Stage II","Stage III","Stage IV","Methylated","Un-resectable","resectable",
                                                "Unmethylated","Advanced Stage","Recurrent","Metastatic","Early stage", "New diagnosis","Relapsed/Refractory","Post Cellular Therapy",
                                                "Smoldering Myeloma"),
                                    multiple = T,
                                    options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                                    width = "200px"
                                
                                )),
                         column(3,
                                #style = "display: inline-block;",
                                   # style = "margin-top: 10px;",
                                    h5("Stage selected: "), 
                                    textOutput("stage_link")
                                    
                                ),
                         column(3,
                                
                                 # style = "display: inline-block;",
                                  #style = "margin-top: 15px;",
                                  pickerInput(
                                    inputId = "LnoTView",
                                    label = "Line Of Therapy Available",
                                    choices = c("Not available", 1, 2, 3, "3+", "Neoadjuvant","Adjuvant", "Maintenance", "> 3 lines of prior treatment", "Recurrent","Registry", "Surgical", "Sequencing"),
                                    multiple = T,
                                    options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                                    width = "200px"
                                  
                                )),
                         column(3,
                                #style = "display: inline-block;",
                                   # style = "margin-top: 15px;",
                                    h5("Line Of Therapy selected: "),
                                    textOutput("lnot_link")
                                
                         ),
                        
                         # column(3,
                         # 
                         #        # style = "display: inline-block;",
                         #        # style = "margin-top: 15px;",
                         #        pickerInput(
                         #          inputId = "statusView",
                         #          label = "Status Type Available",
                         #          choices = c("Not available", "open", "on hold", "closed", "coming soon"),
                         #          multiple = F,
                         #          options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE),
                         #          width = "200px"
                         # 
                         #        )),
                         column(6,
                                #style = "display: inline-block;",
                                #style = "margin-top: 15px;",
                                h5(strong("Status Type Available: ")),
                                textOutput("stat_link",)

                         ),
                         # column(3,
                         #          pickerInput(
                         #            inputId = "locationView",
                         #            label = "Location Available",
                         #            choices = c("Sioux Fall SD","MN"),
                         #            multiple = T,
                         #            options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE),
                         #            width = "200px"
                         # 
                         #        )),
                         column(6,
                                    h5(strong("Location Available: ")),
                                    textOutput("loca_link")

                         ),
                         br(),
                         column(6,
                                pickerInput(
                                  inputId = "DocView",
                                  label = "Reference Links",
                                  choices = list("Tempus" = c("<a href=\"https://therapies.securetempus.com/\">Tempus</a>"),"Caris" = c("<a href=\"https://trialplus.carisls.com/\">Caris</a>"), "Optimal" = c("<a href=\"https://www.optimalresearchportal.com/login/\">Optimal</a>")),
                                  multiple = F,
                                  #options = pickerOptions(actionsBox = TRUE,liveSearch = TRUE),
                                  width = "200px"
                                  
                                )
                                ),
                         column(6,
                                h5(strong("Reference Link selected: ")),
                                textOutput("Doc_link")
                                )

                       ),
                      br(),
                      box(width = 12,background = "blue",
                      uiOutput("temp1" )
                      ),
                      br(),
                      tabsetPanel(id = "tabset", type = "pills",
                                  
                                  tabPanel("Trial Part 1",
                                           br(),
                                           uiOutput("firsttab")
                                           ), 
                                  tabPanel("Trial Part 2",
                                           uiOutput("secondtab")
                                           )
            #tabPanel("panel 3", uiOutput("thridtab"))
          )
         # tags$script(src = "knw_table_module.js"),
          
         
        ),
  useShinyjs()
  
    )


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  #adding trigger for automatic refresh of clinical trial data from Mongo database after editing
 #session$edittb_trigger <- reactiveVal(0)
  
  
  # for copying pasting the values onto the cell of selection 
  output$stage_link <- renderText({input$stageView})
  output$loca_link <- renderText({paste0("Sioux Falls SD")})
  output$lnot_link <- renderText({input$LnoTView})
  output$stat_link <- renderText({paste0("open, on hold, closed, closing soon, Not available, Recruiting, opening soon, Not Recruiting, Recruiting closing soon ")})
  output$Doc_link <- renderText({input$DocView})

  
  # helper function to create buttons for the biomarker section
  
   output$trl_table = renderDataTable({
     
     butns_edi <- create_btns_edit(nrow(browse_tbl))
     tbRec$picktb <- browse_tbl %>% select(NCT,Protocol) %>%
       rownames_to_column(var = "ArmID") %>% 
       bind_cols(tibble("armadd" = butns_edi)) 
     
    # picktb = browse_tbl %>% select(NCT) %>% mutate( trial_no = row_number())
     
     datatable(tbRec$picktb[,2:4],
               rownames = FALSE,
               colnames = c("NCT ID","Protocol","Action"),
               filter = list(position = 'top', clear = FALSE),
               class = "compact stripe row-border nowrap",
               # Escape the HTML in all except 1st column (which has the buttons)
              escape = FALSE,
              selection = "single",
              options = list(
                searching = TRUE, 
                pageLength = 30,dom = 'tip' )
               )
     
   })
   
   
   # reactive filter to get the picked trial 
   
 parsedOut = eventReactive(input$trl_table_rows_selected,{

   #  if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "first")) {
       selRow <- tbRec$picktb[input$trl_table_rows_selected, ]
       filNCT <- selRow[["NCT"]]
       tbRec$currTb = browse_tbl %>% filter(NCT == filNCT)
       filNCT
       output$firsttab <- renderUI({fstUI })
       output$secondtab <- renderUI({secdUI})

       #tbRec$currTb = browse_tbl %>% filter("NCT" == filNCT)

       #print(tbRec$currTb$NCT)
    # }
   return(filNCT)
   })
 
 
 # Just to display NCT id of the trial selected 
 output$temp1 = renderText({
   HTML(paste0("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t","<b>","THE TRIAL SELECTED : ",parsedOut() ,"</b>" ))
 })
 
 # Display the editable table for the Trial Part 1 TAB 
 
 
 # ----------------------------------------------------------------------------------------------------------- #
 # ----------------------------------------------------------------------------------------------------------- #
 
 # Display editable table - "Meta Information table Part 1"
 
 output$trlinfo1_table = renderDataTable({
   tbRec$infoTb1 = tbRec$currTb  %>% select(NCT, JIT, Name, Protocol, docUpdate, HoldStatus) 
   
   datatable(isolate( tbRec$infoTb1),editable = TRUE, class = "compact cell-border", options = list(
     searching = FALSE, scrollX = TRUE, pageLength = 20,dom = 'tip' ), selection = 'single',width = "auto", rownames = F )
 })
 
 observeEvent(input$trlinfo1_table_cell_edit,{
   #celldis <- input[["trldoc_table_cell_edit"]]
   tbRec$infoTb1 <<- editData(tbRec$infoTb1, input$trlinfo1_table_cell_edit, proxy = dataTableProxy("trlinfo1_table"), rownames = FALSE)
   
 })
 
 
 # save the Meta Information table Part 1 datatable with edit update 
 observeEvent(input$saveInfo1, {
   tbRec$infoUp1 = tbRec$infoTb1
   alert("Saved successfully!")
   shinyjs::disable("test")
 })
 
 
 
 
 
 # -------------------------------------------------------------------------------------------------------------------- #
 # -------------------------------------------------------------------------------------------------------------------- #
 
 # Display editable table - "Meta Information table Part 2"
 
 output$trlinfo2_table = renderDataTable({
   tbRec$infoTb2 = tbRec$currTb  %>% select( Title, Status, StatusDate, LastUpdate, Sponsor, Summary , Conditions, Phase , StudyType, MinAge,Gender, Link )
   #print(tbRec$infoTb)
   displayTb = t(tbRec$infoTb2)
   datatable(displayTb, colnames = c("Details"),editable = TRUE, class = "compact stripe row-border nowrap", options = list(
     searching = FALSE, scrollX = TRUE, pageLength = 20,dom = 'tip' ), selection = 'single',width = "auto")
   })
 
 # for case with multiple updates on meta information table
 
 # observeEvent(input$trlinfo_table_cell_edit,{
 #   displayTb = (t(tbRec$infoTb ))
 #   ferow = row.names(displayTb)
 #  cell <- input[["trlinfo_table_cell_edit"]]
 #   edtRow = ferow[[input$trlinfo_table_cell_edit$row]]
 #   #print(edtRow)
 #    print(cell)
 #   cell$row = edtRow
 #  # cell$col = input$trlinfo_table_cell_edit$row 
 #  # print(cell)
 #  # tbRec$infoUp = tbRec$infoTb
 #  # tbRec$infoTb [cell$col, cell$row] <- cell$value
 #   #displayTb = (t(tbRec$infoTb ))
 #   #displayTb[cell$row, cell$col] <- cell$value
 #   
 #   #print(nrow(displayTb))
 # # print(input$trlinfo_table_cell_edit)
 #  # DT::replaceData(dataTableProxy("trlinfo_table"),  displayTb , resetPaging = FALSE,)
 #    tbRec$infoUp<<- editData( tbRec$infoUp ,  cell, proxy = dataTableProxy("trlinfo_table") )
 #  # print(displayTb)
 #   tbRec$infoTb = (t(displayTb))
 #   print(tbRec$infoTb)
 # })
 
 observeEvent(input$saveInfo2, {
   #tbRec$infoUp <- tbRec$infoTb
   cell <- input[["trlinfo2_table_cell_edit"]]
   if (is.null(cell)) {
     tbRec$infoUp2 <- tbRec$infoTb2
   }else{
     tbRec$infoUp2 <- tbRec$infoTb2
     tbRec$infoUp2[cell$col, cell$row] <- cell$value
     tbRec$infoTb2 <-  tbRec$infoUp2
   }
   alert("Saved successfully!")
   shinyjs::disable("test")
   #df(newdf)
 })
 
 
 # -------------------------------------------------------------------------------------------------------------------- #
 # -------------------------------------------------------------------------------------------------------------------- #
 
 
 # Display editable table - "Document Information table"
 
 output$trldoc_table = renderDataTable({
   tbRec$docRec = tbRec$currTb  %>% select(Documentation, locations) 
  
   datatable(isolate(tbRec$docRec),editable = TRUE, class = "compact cell-border", options = list(
     searching = FALSE, scrollX = TRUE, pageLength = 20,dom = 'tip' ), selection = 'single',width = "auto", rownames = F )
 })
 
 observeEvent(input$trldoc_table_cell_edit,{
   #celldis <- input[["trldoc_table_cell_edit"]]
   tbRec$docRec <<- editData(tbRec$docRec, input$trldoc_table_cell_edit, proxy = dataTableProxy("trldoc_table"), rownames = FALSE)
   
 })
 
 
 # save the Document datatable with edit update 
 observeEvent(input$saveDoc, {
   tbRec$docUp = tbRec$docRec
   alert("Saved successfully!")
   shinyjs::disable("test")
 })
 
 # Displaying the Disease summary table 
 output$disSum <- renderText({
   paste0("Overall Disease Summary: ", tbRec$currTb$sumDis)
 })
 
 # Displaying the Disease table 
 output$trldis_table = renderDataTable({
   tbRec$disRec = tbRec$currTb  %>% select(disease) %>% unnest(disease) 
  #print(tbRec$disRec)
   datatable(isolate(tbRec$disRec), editable = TRUE, class = "compact cell-border", options = list(
     searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), selection = 'single',width = "auto",rownames = F )
 })
 
 
 # Add Button action for adding additional row for the Disease data
 observeEvent(input$disAdd,{
   modal_dise( OgTis ="", inExIn = "", stageIn = "") 
 })
 #proxyDis <- dataTableProxy("trldis_table")
 observeEvent(input$trldis_table_cell_edit,{
   #celldis <- input[["trldis_table_cell_edit"]]
   tbRec$disRec <<- editData(tbRec$disRec, input$trldis_table_cell_edit, proxy = dataTableProxy("trldis_table"), rownames = FALSE)
  
 })
 
 
 # save the disease datatable with edit update 
 observeEvent(input$saveDis, {
     tbRec$disUp = tbRec$disRec
     alert("Saved successfully!")
     shinyjs::disable("test")
   
 })
 
 
 # add the entries into the the disease tibble on add button action 
 
 observeEvent(input$final_dis, {
   shiny::removeModal()
   dt_row <- tibble(
     
     code = input$OgTis, 
     selection = input$inExIn,
     stage = input$stageIn)
   
   tbRec$disRec <- tbRec$disRec %>% bind_rows(dt_row) %>% distinct()
   
   output$trldis_table <- renderDT({
     datatable(isolate(tbRec$disRec), 
               editable = TRUE,
               class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), 
               selection = 'single',width = "auto", rownames = F )
               
   })
   
   proxy <- dataTableProxy("trldis_table")
   proxy %>% selectRows(NULL)
 })
 
 

 # --------------------------- TabSet Panel 2 ---------------------------------------------- #

 
 # InsrtDis = reactive({
 #   if(is.na(input[["trldis_table_cell_edit"]])){
 #     dataDf = tbRec$disRec
 #   }else{
 #     dataDF = tbRec$disUp
 #   }
 #   return(dataDF)
 # })
 
 # update the information on the arm / cohort information 
 output$trlCrt_table = renderDataTable({
   tbRec$armRec = tbRec$currTb %>% select(arms) %>% unnest(arms) %>% select(-biomarker)
 # print(tbRec$armRec)
   datatable(isolate(tbRec$armRec),
             editable = TRUE, 
             class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), 
             selection = 'single', width = "auto",rownames = FALSE)
             
 })
 
 # save the Cohort + arm datatable with edit update 
 
 observeEvent(input$trlCrt_table_cell_edit,{
   #celldis <- input[["trldoc_table_cell_edit"]]
   tbRec$armRec <<- editData(tbRec$armRec, input$trlCrt_table_cell_edit, proxy = dataTableProxy("trlCrt_table"), rownames = FALSE)
   
 })
 
 
 # save the Document datatable with edit update 
 observeEvent(input$saveChrt, {
   tbRec$armUp = tbRec$armRec 
   alert("Saved successfully!")
   shinyjs::disable("test")
 })
 
 # observeEvent(input$saveChrt, {
 #   cellChrt <- input[["trlCrt_table_cell_edit"]]
 #   print(cellChrt)
 #   if (is.null(cellChrt)) {
 #     tbRec$armUp <- tbRec$armRec 
 #   }else{
 #     tbRec$armUp <- tbRec$armRec 
 #     tbRec$armUp[cellChrt$row, cellChrt$col] <- cellChrt$value
 #   }
 #   
 #   #print(tbRec$armUp)
 # })
 
 
 # update the information on the Biomarker information 
 output$trlBio_table = renderDataTable({
   #tbRec$biomRec = tbRec$currTb %>% select(biomarker)  %>% unnest(biomarker) %>% select(biomarker)  %>% unnest(biomarker)
   tbRec$biomRec = tbRec$currTb %>% select(arms) %>% unnest(arms) %>% select(ArmID,biomarker) %>% unnest(biomarker)
    datatable(isolate(tbRec$biomRec),
             editable = TRUE,
             class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ),
             selection = 'single', width = "auto", rownames = F)
 })
 
 
 # Add Button action for adding additional row for the Biomarker data
 observeEvent(input$disBio,{
   tbRec$corh = tbRec$currTb %>% select(arms) %>% unnest(arms) %>% select(ArmID,cohortlabel)
  # print(tbRec$corh$cohortlabel)
   modal_biomarker(armId = "", cohor = "", gene1 = "", gene2 = "", typ = "", var = "" , selec = "", func = "") 
 })
 
 observeEvent(input$trlBio_table_cell_edit,{
   #celldis <- input[["trldis_table_cell_edit"]]
   # print(celldis)
   tbRec$biomRec <<- editData(tbRec$biomRec, input$trlBio_table_cell_edit, proxy = dataTableProxy("trlBio_table"), rownames = FALSE)
   print(tbRec$biomRec)
  
 })
 
 # add the entries into the the Biomarker tibble on add button action 
 
 observeEvent(input$final_edit, {
   shiny::removeModal()
   dt_bio <- tibble(
     ArmID = input$armId,
     cohort = input$cohor, 
     Gene = input$gene1, 
     Gene2 = input$gene2, 
     Type = input$typ, 
     Variant = input$var,
     Selection = input$selec, 
     Function = input$func,
     summary = "Write it"
     )
   #print(dt_bio)
   tbRec$biomRec <- tbRec$biomRec %>% bind_rows(dt_bio) %>% distinct()
  # print(tbRec$biomRec)
    output$trlBio_table <- renderDT({
      datatable(isolate(tbRec$biomRec), 
                editable = TRUE, 
                class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), 
                selection = 'single', width = "auto", rownames = F )
      
    })
   # 
    proxy <- dataTableProxy("trlBio_table")
    proxy %>% selectRows(NULL)
 })
 
 observeEvent(input$saveBio, {
  
     tbRec$biomUp <- tbRec$biomRec
     alert("Saved successfully!")
     shinyjs::disable("test")
 })
 # 
 ### remove edit modal when close or submit button is clicked
 observeEvent(input$dismiss_dis, {
   shiny::removeModal()
 })
 
 observeEvent(input$dis_modal, {
   shiny::removeModal()
 })
 
 # final Button Action on the 
 observeEvent(input$addIn,{
   
   # Display the Query Information Table
     
     # save all the variables to their appropiate values 
   #print(input$trlinfo_table)
   #infoDis = input$trlinfo_table$value
     infoDis1 <-  as_tibble(tbRec$infoUp1)
     infoDis2 <-  as_tibble(tbRec$infoUp2)
     docDis <- as_tibble(tbRec$docUp)
     

      # save the disease info entered
      tempDisease = tbRec$disUp %>% group_by(code,selection) %>%  summarise(stage = paste0(stage,collapse = ";"))
      DisTab <- as_tibble(tempDisease)
     
      
      # Enter the arm table in order 
      armTb = as_tibble(tbRec$armUp)
    #  print(armTb)
      bioMarkTb <- as_tibble(tbRec$biomUp)
     # # save the biomarker info entered
      alltoAmBK = left_join(armTb, bioMarkTb, by = c('ArmID'))
     # print(alltoAmBK)
     
     colnames(alltoAmBK) = c("ArmID", "cohortlabel", "drug" ,"arm_type" ,"line_of_therapy", "arm_hold_status",          
                              "cohort", "Gene" , "Gene2", "Type" , "Variant","Selection", "Function" ,"summary" )
     # 
      armForBioMk = alltoAmBK %>% group_by(ArmID, cohortlabel, drug ,arm_type ,line_of_therapy, arm_hold_status ) %>% nest()
      armForBioMk = setnames(armForBioMk, "data", "biomarker")
     # 
     
     # ----------------------------------------------------------------------------------------- # 
     # making the data tibble for each trial entry
#     NCT, JIT, Name, Protocol, Title, Status, StatusDate, LastUpdate, HoldStatus, Sponsor, Summary , Conditions, 
#     Phase , StudyType, Documentation, MinAge,Gender, Link 
     # final tibble to display  
     disBrw2 <<- tibble(
       info = tibble(NCT = infoDis1$NCT,
                     Protocol_No = infoDis1$Protocol,
                     jit = infoDis1$JIT,
                     trial_name = infoDis1$Name
       ),
       disease = tibble(summary = tbRec$currTb$sumDis,
                        details = list(DisTab)
       ),
       query = tibble(nct = infoDis1$NCT,
                      title = infoDis2$Title,
                      current_status = infoDis2$Status,
                      status_verif_date = infoDis2$StatusDate,
                      last_update_date = infoDis2$LastUpdate,
                      trial_hold_status = infoDis1$HoldStatus,
                      sponsor = infoDis2$Sponsor,
                      brief_summary = infoDis2$Summary,
                      conditions = infoDis2$Conditions,
                      type = infoDis2$StudyType,
                      phase = infoDis2$Phase,
                      arm = list(armForBioMk),
                      #arm = list(armTb),
                      # docs = if(input$doc_fileType == "Flat File") {
                      #   docs = input$doc
                      # } else
                      # {
                      #   tagvar = tags$a(href=input$doc,)
                      #   docs = tagvar
                      # },
                      docs = docDis$Documentation,
                     locations = docDis$locations,
                     doclastupdate =  infoDis1$docUpdate,
                      min_age = infoDis2$MinAge,
                      gender = infoDis2$Gender,
                      link = infoDis2$Link
       )
     )
     
     #"<a href=\\", input$doc, "\\", "target=\"_blank\">site-documentation</a>"
     tbRec$rsdf <- disBrw2
     editDbData()
   #outSubmit()
   #disAd$allbrws = disAd$allbrws %>% dplyr::bind_rows(disAd$rsdf) 
   alert("Submitted successfully!")
   refresh()
 #  browse_tbl <<- loadDbData()
 })
 
# browse_tbl <- reactivePoll(10000, session,
#                            checkFunc = editDbData,
 #                           valueFunc = loadDbData)
}

# Run the application 
shinyApp(ui = ui, server = server)
