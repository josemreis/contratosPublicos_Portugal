############################################################################

## file: 1_scraper_contratosPublicos.R

### Author: J. M. Reis

### Date: 20/01/2019

### Purpose: Criar duas bases com todos os contratos publicos celebrados e anuncios para contratos publicos, assim como o respectivos metadados de ambos, em Portugal

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

## source relevant functions
source("helper_functions/maxGroupSeq_helperFunction.R")


#### Pull the concluded contracts data---------------------------------------------------------------------

#### Generate the query urls

### We first make an empty query which gives us all the contracts available in the database. Then we will move from sub-page to sub-page by adding row numbers to the url in pairs of 1200 + remaining - 1200 seems to be the maximum the page can take... The function maxGroupSeq() does just that.

## baseline url
baseline_url <- "http://www.base.gov.pt/Base/pt/ResultadosPesquisa?type=contratos&query=texto%3D%26tipo%3D0%26tipocontrato%3D0%26cpv%3D%26numeroanuncio%3D%26aqinfo%3D%26adjudicante%3D%26adjudicataria%3D%26desdeprecocontrato_false%3D%26desdeprecocontrato%3D%26ateprecocontrato_false%3D%26ateprecocontrato%3D%26desdedatacontrato%3D%26atedatacontrato%3D%26desdedatapublicacao%3D%26atedatapublicacao%3D%26desdeprazoexecucao%3D%26ateprazoexecucao%3D%26desdedatafecho%3D%26atedatafecho%3D%26desdeprecoefectivo_false%3D%26desdeprecoefectivo%3D%26ateprecoefectivo_false%3D%26ateprecoefectivo%3D%26pais%3D0%26distrito%3D0%26concelho%3D0"

## number of contracts in the database
N <- baseline_url %>%
  read_html() %>%
  html_nodes(".strong") %>%
  html_text() %>%
  subset(., str_detect(., "^[0-9]")) %>%
  as.numeric()

## Generate the sequence of rows
pair_sequence <- maxGroupSeq(N, 1200)

#### the scraper
## set the urls
static_url1 <- "http://www.base.gov.pt/Base/pt/ResultadosPesquisa?range="
static_url2 <- "&type=contratos&query=texto%3D%26tipo%3D0%26tipocontrato%3D0%26cpv%3D%26numeroanuncio%3D%26aqinfo%3D%26adjudicante%3D%26adjudicataria%3D%26desdeprecocontrato_false%3D%26desdeprecocontrato%3D%26ateprecocontrato_false%3D%26ateprecocontrato%3D%26desdedatacontrato%3D%26atedatacontrato%3D%26desdedatapublicacao%3D%26atedatapublicacao%3D%26desdeprazoexecucao%3D%26ateprazoexecucao%3D%26desdedatafecho%3D%26atedatafecho%3D%26desdeprecoefectivo_false%3D%26desdeprecoefectivo%3D%26ateprecoefectivo_false%3D%26ateprecoefectivo%3D%26pais%3D0%26distrito%3D0%26concelho%3D0&ordering=sort%28-publicationDate%29"

### Run the scraper
concluded_contracts_raw <- map_df(1:(length(pair_sequence) - 1), function(pair_id){
  
  ## set the url
  # row ranges
  range_min <- pair_sequence[pair_id]
  range_max <- pair_sequence[pair_id + 1]
  
  cat(paste0("scraping contracts ", range_min, " - ", range_max, "\n\n"))
  
  # the page
  page <- paste0(static_url1, range_min, "-", range_max, static_url2)
  
  ## scrape the metadata table and the contract page
  
  # check the internet conection, and wait if it is weak. If not, just parse the HTML page
  
  con_test <- try(parsed_sub_page <- page %>%
                    read_html(), silent = TRUE)
  
  # if true, weak connection...wait 1 minute, and then reconnect and parse the HTML page
  if(class(con_test) == "try-error") {
    
    print("reconecting in 1 minute!")
    Sys.sleep(60)
    parsed_sub_page <- page %>%
      read_html()
    
  }
  
  ## pull the table
  query_table <- try(parsed_sub_page %>%
                       html_node(xpath = "//table[@id = 'resultadosContractos']") %>%
                       html_table(fill = TRUE) %>%
                       as.data.frame() %>%
                       select(-ncol(.)) %>%
                       set_names(names(.) %>%
                                   str_to_lower() %>%
                                   str_replace_all("\\s+", "_")) %>%
                       as_tibble() %>%
                       mutate(contrato_pagina = page %>%
                                read_html() %>%
                                html_nodes(xpath = "//span[@class = 'plusSign']/a[contains(text(), '+') and @target = '_blank']") %>%
                                html_attr("href") %>%
                                unique()),
                     silent = TRUE)

  print(sample_n(query_table, 3))  
  
  ## rest time
  Sys.sleep(sample(1:5, 1))
  
  return(query_table)
  
})

### export it
save(concluded_contracts_raw,
     file = "interm_data/concluded_contracts_raw.Rdata")

write.csv(concluded_contracts_raw,
     file = "interm_data/concluded_contracts_raw.csv")