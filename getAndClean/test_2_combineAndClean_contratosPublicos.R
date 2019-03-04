############################################################################

## file: 2_combineAndClean_contratosPublicos.R

### Author: J. M. Reis

### Date: 04/03/2019

### Purpose: combars of df's e limpar as variaveis relevantes

############################################################################

#### Setting things up-------------------------------------------------------------------------------

### sub-directory as the main current directory
main_dir <- getwd()
setwd(paste0(main_dir, "/getAndClean"))

### loading the relevant packages
library(tidyverse)
library(openxlsx)
library(lubridate)
library(data.table)

### get a sample fo 20 contracts
sampled_contracts <- sample(list.files("interm_data/contract_repo"), 20) %>%
  paste0("interm_data/contract_repo/", .)



### load the sampled datasets and turn to df
combined_df <- map(paste0("interm_data/contract_repo/", list.files("interm_data/contract_repo")), read.csv, encoding = "UTF-8", stringsAsFactor = FALSE) %>%
  map(., function(df){
    result <- df %>%
      mutate_all(as.character)
    return(result)
  }) %>%
  map_df(., rbind) %>%
  as_tibble()

### turn to JSON
wd_df
