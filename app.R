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
                      title=span("Avera Genomics Trials",style = "color: white; font-weight: bold;"),titleWidth = 350
                      #theme = "grey_dark"
                      #windowTitle = "trialEdit"
                    ),
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  #theme = bslib::bs_theme(version = 5, bootswatch = "simplex"),

    # Application title
    
    

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(collapsed = FALSE, width = 350,
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
                        width = 12, background = "yellow",
                        column(6,
                               h5(strong("Click on the Submit Button to Add Updates for selected trail: "))
                        ),
                        column(6,
                               actionButton("addIn",label = "SUBMIT", icon = shiny::icon("plus"),size = "lg",width = '300px', class = "btn-success")),
                      ),
                      
                      
                      tabsetPanel(id = "tabset", type = "pills",
                                  
                                  tabPanel("Trial Part 1",
                                           br(),
                                           br(),
                                           uiOutput("temp1" ),
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

  
  # helper function to create buttons for the biomarker section
  
   output$trl_table = renderDataTable({
     
     butns_edi <- create_btns_edit(nrow(browse_tbl))
     tbRec$picktb <- browse_tbl %>% select(NCT) %>%
       rownames_to_column(var = "ArmID") %>% 
       bind_cols(tibble("armadd" = butns_edi)) 
     
    # picktb = browse_tbl %>% select(NCT) %>% mutate( trial_no = row_number())
     
     datatable(tbRec$picktb[,2:3],
               rownames = FALSE,
               colnames = c("NCT ID","Action"),
               filter = list(position = 'bottom', clear = FALSE),
               class = "compact stripe row-border nowrap",
               # Escape the HTML in all except 1st column (which has the buttons)
              escape = FALSE,
              selection = "single",
              options = list(
                searching = FALSE, 
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

       #print(tbRec$currTb)
    # }
   return(filNCT)
   })
 
 
 # Just to display NCT id of the trial selected 
 output$temp1 = renderText({
   HTML(paste0("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t","<b>","THE TRIAL SELECTED : ",parsedOut() ,"</b>" ))
 })
 
 # Display the editable table for the Trial Part 1 TAB 
 
 # Display editable table - "Meta Information table"
 
 output$trlinfo_table = renderDataTable({
   tbRec$infoTb = tbRec$currTb  %>% select(NCT, JIT, Name, Protocol, Title, Status, StatusDate, LastUpdate, HoldStatus, Sponsor, Summary , Conditions, 
                             Phase , StudyType, Documentation, MinAge,Gender, Link ) %>% mutate(location = "NA")
   displayTb = (t(tbRec$infoTb))
   datatable(displayTb, colnames = c("Details") , editable = TRUE, class = "compact stripe row-border nowrap", options = list(
     searching = FALSE, scrollX = TRUE, pageLength = 20,dom = 'tip' ), selection = 'single',width = "auto" )
   #print(tbRec$infoTb$MinAge)
   #tbRec$infoUp = (t(displayTb))
  # proxy <- dataTableProxy("trlinfo_table")
  # proxy %>% selectRows(NULL)
   #print(proxy)
   })
 
 
 observeEvent(input$saveInfo, {
   cell <- input[["trlinfo_table_cell_edit"]]
  # print(nrow(as.numeric(cell)))
   if (is.null(cell)) {
     tbRec$infoUp <- tbRec$infoTb
   }else{
     tbRec$infoUp <- tbRec$infoTb
     tbRec$infoUp[cell$col, cell$row] <- cell$value
   }
  
   #df(newdf)
 })
 
 # Displaying the Disease summary table 
 output$disSum <- renderText({
   paste0( "Overall Disease Summary: ",tbRec$currTb$sumDis)
 })
 
 # Displaying the Disease table 
 output$trldis_table = renderDataTable({
   tbRec$disRec = tbRec$currTb  %>% select(disease) %>% unnest(disease) %>% mutate(stage = "NA")
  # print(tbRec$disRec)
   datatable(isolate(tbRec$disRec), editable = TRUE, class = "compact stripe row-border nowrap", options = list(
     searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), selection = 'single',width = "auto",rownames = F )
 })
 
 
 # Add Button action for adding additional row for the Disease data
 observeEvent(input$disAdd,{
#   output$sum_tb <- renderText({
#     tbRec$currTb$sumDis
#   })
   modal_dise( OgTis ="", inExIn = "", stageIn = "") 
 })
 #proxyDis <- dataTableProxy("trldis_table")
 observeEvent(input$trldis_table_cell_edit,{
   #celldis <- input[["trldis_table_cell_edit"]]
  # print(celldis)
   tbRec$disRec <<- editData(tbRec$disRec, input$trldis_table_cell_edit, proxy = dataTableProxy("trldis_table"), rownames = FALSE)
  # print(tbRec$disRec)
   #proxy <- dataTableProxy("trldis_table")
#  DT::replaceData(dataTableProxy("trldis_table"),  tbRec$disRec , resetPaging = FALSE,)
               # editable = list(target = "all"),
               # class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), 
               # selection = 'single',width = "auto" )
     
  
   
  
   #proxy %>% selectRows(NULL)
 })
 
 
 # save the disease datatable with edit update 
 observeEvent(input$saveDis, {
   #celldis <- input[["trldis_table_cell_edit"]]
   #print(celldis)
  # if (!is.null(celldis)) {
    # print(editDis())
     tbRec$disUp = tbRec$disRec
     #print(tbRec$disUp)
  # }else{
     #tbRec$disRec[celldis$row, celldis$col] <- celldis$value
  #   tbRec$disUp <- tbRec$disRec
    # tbRec$disRec[celldis$row, celldis$col] <- celldis$value
  # }
  
  
   #df(newdf)
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
   datatable(tbRec$armRec,
             editable = TRUE, 
             class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), 
             selection = 'single', width = "auto")
             
 })
 
 # save the Cohort + arm datatable with edit update 
 
 observeEvent(input$saveChrt, {
   cellChrt <- input[["trlCrt_table_cell_edit"]]
   print(cellChrt)
   if (is.null(cellChrt)) {
     tbRec$armUp <- tbRec$armRec 
   }else{
     tbRec$armUp <- tbRec$armRec 
     tbRec$armUp[cellChrt$row, cellChrt$col] <- cellChrt$value
   }
   
   #print(tbRec$armUp)
 })
 
 
 # update the information on the arm / cohort information 
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
   #proxy <- dataTableProxy("trldis_table")
   #  DT::replaceData(dataTableProxy("trldis_table"),  tbRec$disRec , resetPaging = FALSE,)
   # editable = list(target = "all"),
   # class = "compact stripe row-border nowrap", options = list(searching = FALSE, scrollX = TRUE, pageLength = 30,dom = 'tip' ), 
   # selection = 'single',width = "auto" )
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
   #cellBio <- input[["trlBio_table_cell_edit"]]
  #### if (!is.na(cellBio)) {
  # tbRec$biomUp <- tbRec$biomRec
  # tbRec$biomUp[cellBio$row, cellBio$col] <- cellBio$value
 #  } else{
     tbRec$biomUp <- tbRec$biomRec
  # }
   #df(newdf)
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
     infoDis <-  as_tibble(tbRec$infoUp)
     
     
     # # save the arm info from query output
     # armTb <- left_join(disAd$armDf, disAd$Armpt1Tb_out, by = "cohortlabel")
     # armTb <- armTb %>% rownames_to_column(var = "ArmID")
     # armTb <- tibble(
     #   ArmID = armTb$ArmID,
     #   cohortlabel = armTb$cohortlabel,
     #   drug = armTb$drug,
     #   arm_type = armTb$arm_type,
     #   line_of_therapy = armTb$lineTx,
     #   arm_hold_status = armTb$armStatus
     # )
     # 
      # save the disease info entered
      tempDisease = tbRec$disUp %>% group_by(code,selection) %>%  summarise(stage = paste0(stage,collapse = ";"))
      DisTab <- as_tibble(tempDisease)
     
      
      # Enter the arm table in order 
      
      armTb = as_tibble(tbRec$armUp)
    #  print(armTb)
      bioMarkTb <- as_tibble(tbRec$biomUp)
     # # save the biomarker info entered
      alltoAmBK = left_join(armTb, bioMarkTb, by = c('ArmID'))
      print(alltoAmBK)
     
     # 
     # 
     # 
     # # adding the biomarker tibble to the respective cohort 
     # alltoAmBK = left_join(armTb, tb_add, by = c('ArmID' = 'armID'))
     # #print(colnames(alltoAmBK))
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
       info = tibble(NCT = infoDis$NCT,
                     Protocol_No = infoDis$Protocol,
                     jit = infoDis$JIT,
                     trial_name = infoDis$Name
       ),
       disease = tibble(summary = tbRec$currTb$sumDis,
                        details = list(DisTab)
       ),
       query = tibble(nct = infoDis$NCT,
                      title = infoDis$Title,
                      current_status = infoDis$Status,
                      status_verif_date = infoDis$StatusDate,
                      last_update_date = infoDis$LastUpdate,
                      trial_hold_status = infoDis$HoldStatus,
                      sponsor = infoDis$Sponsor,
                      brief_summary = infoDis$Summary,
                      conditions = infoDis$Conditions,
                      type = infoDis$StudyType,
                      phase = infoDis$Phase,
                      arm = list(armForBioMk),
                      #arm = list(armTb),
                      # docs = if(input$doc_fileType == "Flat File") {
                      #   docs = input$doc
                      # } else
                      # {
                      #   tagvar = tags$a(href=input$doc,)
                      #   docs = tagvar
                      # },
                      docs = infoDis$Documentation,
                     locations = infoDis$location,
                      #doclastupdate = input$dt,
                      min_age = infoDis$MinAge,
                      gender = infoDis$Gender,
                      link = infoDis$Link
       )
     )
     
     #"<a href=\\", input$doc, "\\", "target=\"_blank\">site-documentation</a>"
     tbRec$rsdf <- disBrw2
     
   outSubmit()
   #disAd$allbrws = disAd$allbrws %>% dplyr::bind_rows(disAd$rsdf) 
   alert("Submitted successfully!")
   refresh()
 })
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)
