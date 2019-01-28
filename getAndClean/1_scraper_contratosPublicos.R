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
library(data.table)

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
cc_metadata <- map_df(1:(length(pair_sequence) - 1), function(pair_id){
  
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
  
  ## remove mistaken ones..
  if(class(query_table) == "try-error"){
    
    query_table <- NULL
    
    
  }

  print(query_table)  
  
  ## rest time
  Sys.sleep(sample(1:5, 1)) 
  
  return(query_table)
  
})

### add an id variable. use the id's found in the URL.
cc_metadata$id <- str_extract(cc_metadata$contrato_pagina, "(?<=a\\=).*$")

### export it
save(cc_metadata,
     file = "interm_data/concluded_contracts_raw.Rdata")

write.csv(cc_metadata,
     file = "interm_data/concluded_contracts_raw.csv")


#### scrape the contract details-----------------------------------------------

## generate a contract_repo 
if(!dir.exists("interm_data/contract_repo")){
  
  dir.create("interm_data/contract_repo")
  
}

### We repeat the steps above, but now using the "contrato_pagina" urls
map2(cc_metadata$id, cc_metadata$contrato_pagina, function(id, page){
  
  ## start
  cat(paste0("scraping contract ", id,"\n\n"))
  
  ### check if the dataset was already scraped
  # generate the file_name
  file_name <- paste0("interm_data/contract_repo/",
                  id,
                  ".csv")
  
  if(!file.exists(file_name)){
  ### Parsing the contract page
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
  
  ### scrape the table
  tables <- try(parsed_sub_page %>%
    html_nodes(xpath = "//table") %>%
    html_table() %>%
    map(., function(tab){
      
      res <- tab %>% 
        t() %>%
        as_tibble() %>% 
        set_names(., 
                  nm = .[1,] %>% 
                    str_replace_all(., "[[:punct:]]|º|\\(.*?\\)", "") %>% 
                    str_to_lower() %>% 
                    str_replace_all(., "\\s+", "_")) %>% 
        slice(-1) %>%
        mutate(matcher = row_number(),
               id = id,
               contrato_pagina = page)
      
      return(res)
      
    }),
    silent = FALSE)
  
  ## remove mistaken ones..
  if(class(tables) == "try-error"){
    
    tables <- NULL
    
  }
  
  ## if more than one table, left_join them
  
  if(length(tables) > 1){
    
    new_tab <- tables[[1]]
    
    for(i in seq_len(length(tables) - 1)){
      
      new_tab <- left_join(new_tab, tables[[i+1]])
      
    }
    
    contract_table <- new_tab %>%
      select(-matcher) 
    
  } else {
    
    contract_table <- unlist(tables) %>%
      select(-matcher)  
    
  }
  
  ### Add a couple of relevant other variables and urls
  
  ## pagina do adjudicante
  url_adjudicante <- parsed_sub_page %>% 
    html_nodes(xpath = "//td[preceding-sibling::td[contains(text(), 'Entidade adjudicante')]]/a") %>%
    html_attr("href")
  
  # check for emtpy strings...
  url_adjudicante <- ifelse(is_empty(url_adjudicante) || nchar(url_adjudicante) < 2,
                            NA_character_,
                            url_adjudicante)
  
  # if more than one, assign the " <--new element--> " splitter
  contract_table$url_adjudicante <- ifelse(str_count(url_adjudicante, "\\)") > 1,
                                          paste(str_split(url_adjudicante, "(?<=.)(?=\\))")[[1]], collapse = " <--new element--> "),
                                          url_adjudicante)
  
  
  ## pagina do adjudicatario
  url_adjudicatario <- parsed_sub_page %>% 
    html_nodes(xpath = "//td[preceding-sibling::td[contains(text(), 'Entidade adjudicatária')]]/a") %>%
    html_attr("href")
  
  # check for emtpy strings...
  url_adjudicatario <- ifelse(is_empty(url_adjudicatario) || nchar(url_adjudicatario) < 2,
                            NA_character_,
                            url_adjudicatario)
  
  # if more than one, assign the " <--new element--> " splitter
  contract_table$url_adjudicatario <- ifelse(str_count(url_adjudicatario, "\\)") > 1,
                                          paste(str_split(url_adjudicatario, "(?<=.)(?=\\))")[[1]], collapse = " <--new element--> "),
                                          url_adjudicatario)
  
  
  ## pagina do concorrente
  url_concorrentes <- parsed_sub_page %>% 
    html_nodes(xpath = "//td[preceding-sibling::td[contains(text(), 'Concorrentes')]]/a") %>%
    html_attr("href")
  
  # check for emtpy strings...
  url_concorrentes <- ifelse(is_empty(url_concorrentes) || nchar(url_concorrentes) < 2,
                              NA_character_,
                             url_concorrentes)
  
  # if more than one, assign the " <--new element--> " splitter
  contract_table$url_concorrentes <- ifelse(str_count(url_concorrentes, "\\)") > 1,
                                            paste(str_split(url_concorrentes, "(?<=.)(?=\\))")[[1]], collapse = " <--new element--> "),
                                            url_concorrentes)
  
  
  ## pagina do documento
  url_documentos <- parsed_sub_page %>%
    html_nodes(xpath = "//td[preceding-sibling::td[contains(text(), 'Documentos')]]/a") %>%
    html_attr("href")
  
  # check for emtpy strings...
  url_documentos <- ifelse(is_empty(url_documentos) || nchar(url_documentos) < 2,
                             NA_character_,
                           url_documentos)
  
  # if more than one, assign the " <--new element--> " splitter
  contract_table$url_documentos <- ifelse(str_count(url_documentos, "\\)") > 1,
                                          paste(str_split(url_documentos, "(?<=.)(?=\\))")[[1]], collapse = " <--new element--> "),
                                          url_documentos)
  
  
  ## pagina do anuncio
  url_anuncios <- parsed_sub_page %>% 
    html_nodes(xpath = "//td[preceding-sibling::td[contains(text(), 'Anúncio')]]/a") %>%
    html_attr("href")
  
  # check for emtpy strings...
  url_anuncios <- ifelse(is_empty(url_anuncios) || nchar(url_anuncios) < 2,
                           NA_character_,
                         url_anuncios)
  
  # if more than one, assign the " <--new element--> " splitter
  contract_table$url_anuncios <- ifelse(str_count(url_anuncios, "\\)") > 1,
                                        paste(str_split(url_anuncios, "(?<=.)(?=\\))")[[1]], collapse = " <--new element--> "),
                                        url_anuncios)
  
  ## double-check
  print(contract_table[1, sample(1:ncol(contract_table), 4)])
  
  ## write it
  write.csv(contract_table,
            file = file_name,
            fileEncoding = "UTF-8",
            row.names = FALSE)
  
  ## rest time for the server
  Sys.sleep(sample(1:4, 1)) 
  
  
  } else {
    
    print("Already scraped and saved!")
    
    
  }
  
  })

