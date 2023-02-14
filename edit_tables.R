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
    docUpdate = trial$query$doclastupdate,
    MinAge = if(trial$query$min_age %>% is_empty()) {
      min_age = "Not Available"
    } else{
      trial$query$min_age
    },
    Gender = trial$query$gender,
    Link = trial$query$link,
    
    arms = list(arms = trial$query$arm[[1]]),
    #biomarker = list(trial$query$arm[[1]]$biomarker)
    biomarker = list(biomarker = bind_cols(arm_groups %>% select(biomarker))),
    
    location = trial$query$locations
  )
  return(parsedTrial)
  
}





#function to load json documents from Mongo database 
loadDbData <- function() {
  
  db <- mongolite::mongo(collection = "ClinicalTrials", 
                         db = "aci", 
                         url = db_url)
  
  #df1 <- db$find('{}')
  #df2<- jsonlite::toJSON(df1) 
  db_tbl <- db$aggregate()[,2:4] %>%
    
    unnest(cols = c(info, disease, query))
  
  
  db_tbl <- db_tbl %>%  rename(
    
    #   %>% rename(
    # info
    "NCT" = NCT,
    "JIT" = jit,
    "Name" = trial_name,
    "Protocol" = Protocol_No,
    
    # disease
    "sumDis" = summary,

    # query - general
    "Title" = title,
    "Status" = current_status,
    "StatusDate" = status_verif_date,
    "LastUpdate" = last_update_date,
    "HoldStatus" = trial_hold_status,
    "Sponsor" = sponsor,
    "Summary" = brief_summary,
    "Conditions" = conditions,
    # "Conditions" = conditiions,
    "location" = locations,
   "docUpdate" = doclastupdate,
    "Phase" = phase,
    "StudyType" = type,
    "Documentation" = docs,
    # "InclExclCriteria" = criteria,
    # "InclExclCriteria" = db_tbl$details[[1]][2],
    "MinAge" = min_age,
    "Gender" = gender,
    "Link" = link,
   #  "arms" = list(db_tbl$arm[[1]] %>% unnest(biomarker)),
   # "disease" =  list(disease = db_tbl$details[[1]] %>% select(code) %>% distinct() %>% unlist(code) %>% paste0(collapse = "|"))
   #"disease" =  db_tbl$details[[1]] %>% select(code)
  )
  db_tbl = db_tbl %>% mutate(disease = db_tbl$details)
  db_tbl = db_tbl %>% mutate(arms = db_tbl$arm)
 # db_tbl = db_tbl %>% mutate(biomarker = db_tbl$arm$biomarker)
  #db_tbl$arms = list(arms = db_tbl %>% select(arm) )
  
  #db_tbl$biomarker <- list(biomarker = db_tbl$arm[[1]]$biomarker)
    #                        %>% bind_rows() %>%
    # select(summary) %>% distinct() %>%
    # unlist() %>%
    # na.omit() %>%
    # paste0(collapse = "|"))
  
  
 # disease = list(disease = db_tbl$details[[1]] %>% select(code) %>% distinct() %>% unlist(code) %>% paste0(collapse = "|"))
  #disease =  db_tbl$details[[1]]
  
  # biomarker <- db_tbl$arm[[1]]$biomarker %>% bind_rows() %>%
  #   select(summary) %>% distinct() %>%
  #   unlist() %>%
  #   na.omit() %>%
  #   paste0(collapse = "|")
  return(db_tbl)
  
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

