





library(here)
# get path to trials directory
t_d_d <- config::get("trial_data_dir")

trial_data_dir <- if_else(t_d_d %>% fs::is_absolute_path(), t_d_d, t_d_d %>% here())
trialspath <- trial_data_dir


## trial data storage format
storage <- config::get("storage")

if (storage == "json") {
  # create a combined tibble
  trialsfiles <- dir(path = trial_data_dir, pattern = "*.full.ndjson", full.names = T)
  #trialsfiles = trialsfiles[7:10]
  #data <- trialsfiles %>% map_df(~fromJSON(file.path(trialsfiles, .), flatten = TRUE))
  result <- trialsfiles %>% map(parseTrials) %>% bind_rows()
  browse_tbl <<- result
}






if (storage == "db") {
  # look for active mongod process based on docker status
  docker <- config::get("docker")
  
  if (docker == "yes") {
    db_url <<- "host.docker.internal:27017,127.0.0.1:27017" 
  }
  
  if (docker == "no") {
    db_url <<- "mongodb://0.0.0.0:27017" 
    #connection to Mongo database 
    db <- mongolite::mongo(collection = "ClinicalTrials", 
                           db = "aci", 
                           url = db_url)
    
    db_tbl <- db$aggregate()[,2:4] %>%
      
      unnest(cols = c(info, disease, query))
    
    
  }
  #session$edittb_trigger()
  browse_tbl <<- loadDbData()
}


# get oncotree data
oncotree <- read.delim2(file = here("data", "oncotree", "oncotree.tsv"), 
                        header = TRUE, 
                        sep = "\t", 
                        quote = "", 
                        na.strings = 'NA')
oncotree_dup<-oncotree
oncotree_addrows<-oncotree_dup %>% add_row(level_1 = "All Cancers")
oncotree_addrows1<-oncotree_addrows %>% add_row(level_1 = "Solid Tumors")

# oncotree list 
trlOnco = data.table( oncotree_addrows1[,1:8])


melt_onc=melt(trlOnco, id.vars = 'level_1',na.rm = T ,measure.vars = list(c("metamaintype",'level_2','level_3','level_4','level_5','level_6')),  value.name = c('Levels'))
melt_onc = melt_onc %>% select(c(1,3)) %>% distinct() %>% group_by(level_1) %>% nest()

nested_list<-list()
for(i in melt_onc){nested_list<-append(nested_list,list(i))}
names(nested_list[[2]]) = c(melt_onc$level_1)

# get gene and variant lists
allgeneR <- here("data", "metadata", "allgenes.txt")
allvarR <- here("data", "metadata", "allvariants.txt")

allgenes <- read.delim2(file = allgeneR, 
                        header = TRUE, 
                        sep="\t", 
                        quote = "", 
                        na.strings = 'NA') %>% arrange(x) %>% distinct()
allgenes <- bind_rows(tibble(x = "Not available"), allgenes)

allVar <- read.delim2(file = allvarR, 
                      header = TRUE, 
                      sep="\t", 
                      quote = "", 
                      na.strings = 'NA') %>% arrange(x) %>% distinct() 
allVar <- bind_rows(tibble(x = "Not available"), allVar)

