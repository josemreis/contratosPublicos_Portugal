############################################################################

## file: 2_combineAndClean_contratosPublicos.R

### Author: J. M. Reis

### Date: 04/03/2019

### Purpose: combars of df's e limpar as variaveis relevantes

############################################################################

#### Setting things up----------------------------------------------------

### sub-directory as the main current directory
main_dir <- getwd()
setwd(paste0(main_dir, "/getAndClean"))

### loading the relevant packages
library(tidyverse)
library(openxlsx)
library(lubridate)
library(data.table)

#### Combining the dataframes--------------------------------------------

### load the load the dfs
df_list <- map(paste0("interm_data/contract_repo/", list.files("interm_data/contract_repo")), read.csv, encoding = "UTF-8", stringsAsFactor = FALSE)

### turn all values into characters (for the binding...)
df_list2 <- map(df_list, function(df){
    result <- df %>%
      mutate_all(as.character)
    return(result)
  }) 


### row bind...
combined_df <- rbindlist(df_list2) %>%
  as_tibble()


### Export as .csv
write.csv(combined_df,
          file = paste0("data/1_",str_extract(Sys.time(), "^.*?(?=\\s)"), "_cpCombinedRaw.RD"),
          encoding = "UTF-8")
