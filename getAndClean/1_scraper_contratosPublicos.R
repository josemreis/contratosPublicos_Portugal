############################################################################

## file: 1_scraper_contratosPublicos.R

### Author: J. M. Reis

### Date: 20/01/2019

### Purpose: Criar base the dados com todos os casos das relaçoes de lisboa (http://www.dgsi.pt/jtrl.nsf?OpenDatabase), Porto (http://www.dgsi.pt/jtrp.nsf?OpenDatabase), e Coimbra (http://www.dgsi.pt/jtrc.nsf?OpenDatabase)

############################################################################

#### Setting things up-------------------------------------------------------------------------------

### sub-directory as the main current directory
main_dir <- getwd()
setwd(paste0(main_dir, "/getAndClean"))

### loading the relevant packages
library(tidyverse)
library(rvest)
library(openxlsx)
library(lubridate)

### Create a data directory
if(!dir.exists("data")){
  
  dir.create("data")
  
}

## intermediary
if(!dir.exists("interm_data")){
  
  dir.create("interm_data")
  
}
