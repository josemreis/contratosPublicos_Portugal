### Load the relevant packages
require(tidyverse)
require(rjson)

### setwd on data sub-dir
setwd(paste0(getwd(), "/getAndClean"))

### get a sample fo 20 contracts
sampled_contracts <- sample(list.files("interm_data/contract_repo"), 20) %>%
  paste0("interm_data/contract_repo/", .)



### load the sampled datasets and turn to df
wd_df <- map(sampled_contracts, read.csv, encoding = "UTF-8", stringsAsFactor = FALSE) %>%
  map(., function(df){
    result <- df %>%
      mutate_all(as.character)
    
    return(result)
  }) %>%
  map_df(., rbind)

### turn to JSON
toJSON(wd_df)
