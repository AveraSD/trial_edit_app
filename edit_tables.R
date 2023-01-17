library(shinyWidgets)

tbRec <- reactiveValues(
  picktb = tibble(), #Side table selection
  currTb = tibble(), # current  tibble 
  infoTb = tibble(), # Meta Information
  infoUp = tibble(), # updated information tibble
  docRec = tibble(), # Document 
  docUp = tibble(), # updated Document tibble
  disRec = tibble(), # disease  
  disUp = tibble(),
  armRec = tibble(), # cohort 
  armUp = tibble(),
  biomRec = tibble(), # cohort + arm info + biomarker
  biomUp = tibble(),
  corh = NULL, # cohort list 
  compRec = tibble(), # cohort + biomarker nested
  add_or_edit = NULL,  # confirming the button selection
  allbrws = tibble(), # all together
  #resultsdf = tibble(), # individual 
  rsdf = tibble()
)

# Reading the json/trial files out 

parseTrials <- function(jsonfile) {
  #jsonfile <- trialsfiles[1]
  trial <- fromJSON(jsonfile)
  
  #function to create (1 line of) biomarker per cohort
  # processBiomarker <- function(x) {
  #   b <- arm_groups[x,]$biomarker[[1]] %>%
  #     select(summary) %>%
  #     unlist() %>%
  #     glue_collapse(sep = " | ")
  #   return(b)
  # }
  
  # pulling out trial arms
  arm_groups = tibble(cohortlabel = trial$query$arm[[1]]$cohortlabel,
                      drug = trial$query$arm[[1]]$drug,
                      arm_type = trial$query$arm[[1]]$arm_type,
                      line_of_therapy = trial$query$arm[[1]]$line_of_therapy,
                      arm_hold_status = trial$query$arm[[1]]$arm_hold_status,
                      biomarker = trial$query$arm[[1]]$biomarker)
  
  
  parsedTrial <- tibble(
    NCT = trial$info$NCT,
    JIT = trial$info$jit,
    Name = trial$info$trial_name,
    Protocol = trial$info$Protocol_No,
    
    # disease
    sumDis = trial$disease$summary,
    disease = list(disp_disease = trial$disease %>% select(details) %>% unnest(details)), 
    
    # query - general
    Title = trial$query$title,
    Status = trial$query$current_status,
    StatusDate = trial$query$status_verif_date,
    LastUpdate = trial$query$last_update_date,
    HoldStatus = trial$query$trial_hold_status,
    Sponsor = trial$query$sponsor,
    Summary = trial$query$brief_summary,
    Conditions = trial$query$conditions,
    Phase = trial$query$phase,
    StudyType = trial$query$type,
    Documentation = trial$query$docs,
    MinAge = if(trial$query$min_age %>% is_empty()) {
      min_age = "Not Available"
    } else{
      trial$query$min_age
    },
    Gender = trial$query$gender,
    Link = trial$query$link,
    
    arms = list(arms = trial$query$arm[[1]]),
    #biomarker = list(trial$query$arm[[1]]$biomarker)
    biomarker = list(biomarker = bind_cols(arm_groups %>% select(biomarker)))
    
  )
  return(parsedTrial)
  
}

# function to write trial.ndjson after final Submit button is clicked

outSubmit <- function() {
  #tr2 <- isolate(disAd$resultsdf)
  # print(tr)
  tr2 <- isolate(tbRec$rsdf)
  #print(tr2)
  
  outjson <- paste0(here(trial_out), 
                    paste0(tr2 %>% unnest(c(info, query)) %>% select(NCT) %>% as.character(), ".full.ndjson"))
  writeLines(tr2 %>% toJSON(pretty = T), outjson)
  message(paste0("Written to file: ", outjson))
}

# generate the edit button

create_btns_edit <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-primary action_button btn-md" id="first_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-pencil-square-o"></i>Edit</button>
                   </div>'
                     ))
}

