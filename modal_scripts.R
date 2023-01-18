# On every add button clicked - modal will open up All the modal are written here

# modal on disease section add button activity 

# add arm info modal - keys and values
modal_dise <- function( OgTis, inExIn, stageIn) {
  
  shiny::modalDialog(
    #h4(textInput("sum_tb",label = "Summary Info")),
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "OgTis",
          label = "Tumor Type",
          #choices = c("Not available", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
          choices = c(nested_list[[2]]),
          multiple = F,
          width = "200px",
          options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
        )
      ),
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "inExIn",
          label = "Selection",
          choices = c("include","exclude"),
          multiple = F,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "stageIn",
          label = "Stage Selection",
          choices = c("Stage I","Stage II","Stage III","Stage IV","Methylated","Un-resectable","resectable",
                      "Unmethylated","Advanced Stage","Recurrent","Metastatic","Early stage", "New diagnosis","Relapsed/Refractory","Post Cellular Therapy",
                      "Smoldering Myeloma"),
          multiple = T,
          width = "200px"
        )
      )
      
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      actionButton(
        inputId = "final_dis",
        label = "New Info",
        icon = shiny::icon("plus"),
        class = "btn-primary" 
      ),
      actionButton(
        inputId = "dismiss_dis",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% showModal()
}

# modal for the arm + biomarker  information updates 


# biomarker modal - keys and values
modal_biomarker <- function(armId, cohor, gene1, gene2, typ, var, selec, func) {
  
  modalDialog(
    div(
      class = "text-center",
      shinyjs::useShinyjs(),
      div(
      style = "display: inline-block;",
      pickerInput(
        inputId = "armId",
        label = "Arm ID",
        choices = c(tbRec$corh$ArmID),
        multiple = F,
        width = "200px"
      )
    ),
    div(
      style = "display: inline-block;",
      pickerInput(
        inputId = "cohor",
        label = "cohort",
        choices = c(tbRec$corh$cohortlabel),
        multiple = F,
        width = "200px"
      )
    ),
    # div(
    #   class = "text-center",
    #   style = "display: inline-block;",
    #   textInput("BioSum", "Biomarker summary")
    # ),
   # div(
     # class = "text-center",
     # shinyjs::useShinyjs(),
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "selec",
          label = "selection",
          choices = c("include","exclude"),
          multiple = F,
          width = "200px"
        )
      ),
      
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "gene1",
          label = "Gene", 
          choices = allgenes$x,
          multiple = F,
          width = "200px",
          options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
        )
      ),
      
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "typ",
          label = "Type",
          choices = c("Not available", "Mutation","Missense mut", "Frame Shift mut", "Splice site mut", "Skipping Mutation","Alteration", "Multiple Mutation", "Wild-Type", "Amplification", "Deletion", "Abberration" ,"Rearrangement","Class I Mutation" ,"Class II Mutation", "Class III Mutation" ,"TMB", "MSI", "MSS","Microsatellite" ,"PD-L1", "Fusion", "RNA expr","ctDNA" , "HRD", "MMR", "dMMR", "pMMR" ,"ER (IHC/FISH)", "PR (IHC/FISH)", "HER2 (IHC/FISH)"),
          multiple = F,
          width = "200px"
        )
      ),
      
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "var",
          label = "Variant",
          choices = allVar$x,
          multiple = F,
          width = "200px",
          options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
        )
      ),
      # 
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "func",
          label = "function",
          choices = c("Not available", "high", "low", "Loss","Loss of Function", "gain" ,"positive", "negative", "stable" ,"unstable", "activating", "inactivating" ),
          multiple = F,
          width = "200px"
        )
      ),
      
      div(
        style = "display: inline-block;",
        pickerInput(
          inputId = "gene2",
          label = "Gene2", 
          choices = allgenes$x,
          multiple = F,
          #selected = NULL,
          
          #options = list(`actions-box` = TRUE,`live-search` = TRUE,size=10),
          width = "200px",
          options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
        )
      )
      
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      actionButton(
        inputId = "final_edit",
        label = " Biomarker",
        icon = shiny::icon("plus"),
        class = "btn-primary"
      ),
      actionButton(
        inputId = "dis_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% showModal()
}





















