library(rvest)
library(purrr)
library(dplyr)

# purrr and mutate from https://github.com/jennybc/manipulate-xml-with-purrr-dplyr-tidyr

google_alert <- function(){
  
  url <- 'https://www.google.com/alerts/feeds/08111746092629807497/6708412876604198226'

  rs <- read_xml(url)
  
  ns <- xml2::xml_ns_rename(xml2::xml_ns(rs), d1 = "atom")
  
  rs %>%
    xml_find_all('atom:entry', ns) %>%
    map(~ xml_find_all(.x, xpath = "./*", ns = ns)) %>%
    data_frame(row = seq_len(length(.)),
               nodes = .) %>%
    mutate(col_name_raw = map(nodes, xml_name),
           cell_text = map(nodes, xml_text),
           attr = map(nodes, xml_attrs),
           i = nodes %>% map(~ seq_along(.))) %>%
    select(row, 
           col_name_raw, 
           cell_text,
           attr) %>%
    tidyr::unnest() %>%
    mutate(cell_text = ifelse(col_name_raw == 'link', as.character(attr), cell_text)) %>%
    select(-attr) %>%
    tidyr::spread(col_name_raw, cell_text)

  }

