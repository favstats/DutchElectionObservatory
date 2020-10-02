pacman::p_load(tidyverse, janitor)

dir.create("data")

get_ggl_data <- function() {
  ggl_link <- "https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip"
  
  ggl_file <- "data/ggl.zip"
  
  download.file(ggl_link, ggl_file, mode="wb")
  unzip(ggl_file, exdir = "data")
  
  unlink(ggl_file)
  
  dir("data/google-political-ads-transparency-bundle", full.names =   T) %>% 
    discard(~str_detect(.x,"creative")) %>% 
    walk(file.remove)  
  
  dutch_parties <- c("D66", "VVD", "GroenLinks", "SP (Socialistische Partij)", "Volt Nederland", "Christen Democratisch Appèl", "Partij van de Arbeid")
  
  ggl_ads <- data.table::fread("data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv") %>% 
    # filter(str_detect(Regions, "NL")) %>% 
    janitor::clean_names() %>% 
    filter(advertiser_name %in% dutch_parties) %>% 
    filter(date_range_start >= as.Date("2020-09-01"))
  
  return(ggl_ads)
}

ggl_ads <- get_ggl_data()

tidytemplate::save_it(ggl_ads)
