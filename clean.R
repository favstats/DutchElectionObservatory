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
  
  dutch_parties <- c("D66", "VVD", "GroenLinks", "SP (Socialistische Partij)", "Volt Nederland", "Christen Democratisch Appèl", "Partij van de Arbeid", "FvD")
  
  ggl_ads <- data.table::fread("data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv") %>% 
    # filter(str_detect(Regions, "NL")) %>% 
    janitor::clean_names() %>% 
    # filter(advertiser_name %in% dutch_parties) %>% 
    filter(date_range_start >= as.Date("2020-09-01"))
  
  return(ggl_ads)
}

ggl_ads <- get_ggl_data()

tidytemplate::save_it(ggl_ads)

get_snap_data <- function() {
  
  snap_url <- "https://storage.googleapis.com/ad-manager-political-ads-dump/political/2020/PoliticalAds.zip"
  
  
  
  snap_file <- "data/snap.zip"
  
  download.file(snap_url, snap_file, mode="wb")
  unzip(snap_file, exdir = "data")
  
  unlink(snap_file)
  
  snap_ads <- read_csv("data/PoliticalAds.csv") %>% 
    janitor::clean_names() %>% 
    filter(country_code == "netherlands") %>% 
    mutate(start_date = as.Date(start_date)) %>% 
    filter(start_date >= as.Date("2020-09-01"))
  
  return(snap_ads) 
}

snap_ads <- get_snap_data()

tidytemplate::save_it(snap_ads)
