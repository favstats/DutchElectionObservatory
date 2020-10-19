pacman::p_load(tidyverse, janitor)

dir.create("data")

get_mid <- function(spend_upper_bound, spend_lower_bound) {
  # (spend_upper_bound-spend_lower_bound)/2+spend_lower_bound
  (spend_upper_bound+spend_lower_bound)/2
}


assign_colors <- function(dat, n = 12) {
  
  color_sample <- colorspace::divergingx_hcl(n)
  
  lenght <- dat$color[is.na(dat$color)] %>% length
  
  if(lenght==0) return(invisible())
  
  cols <- sample(color_sample, lenght, replace = T)
  
  dat$color[is.na(dat$color)] <- cols
  
  return(dat)
  
}

get_ggl_data <- function() {
  
  
  ggl_link <- "https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip"
  
  ggl_file <- "data/ggl.zip"
  
  download.file(ggl_link, ggl_file, mode="wb")
  unzip(ggl_file, exdir = "data")
  
  unlink(ggl_file)
  
  dir("data/google-political-ads-transparency-bundle", full.names =   T) %>% 
    discard(~str_detect(.x,"creative")) %>% 
    walk(file.remove)  
  
  # dutch_parties <- c("D66", "VVD", "GroenLinks", "SP (Socialistische Partij)", "Volt Nederland", "Christen Democratisch Appèl", "Partij van de Arbeid", "FvD")
  
  color_dat <- tibble(
    color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#e01003", "#e3101c", "#6f2421"),
    advertiser_name = c("D66", "GroenLinks", "VVD", "Christen Democratisch Appèl", "SP (Socialistische Partij)", "Partij van de Arbeid", "FvD"))
  
  
  ggl_ads <- data.table::fread("data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv") %>% 
    filter(str_detect(Regions, "NL")) %>%
    janitor::clean_names() %>% 
    # filter(advertiser_name %in% dutch_parties) %>%
    filter(date_range_start >= as.Date("2020-09-01"))
  
  
  ggl_total <- ggl_ads %>% 
    group_by(advertiser_name) %>% 
    summarise(spend_range_min = sum(spend_range_min_eur),
              spend_range_max = sum(spend_range_max_eur),
              spend_range_mid = sum(get_mid(spend_range_max_eur, spend_range_min_eur)),
              n = n()) %>% 
    ungroup() %>% 
    left_join(color_dat)  %>% 
    assign_colors()
  
  # tidytemplate::save_it(ggl_total)
  
  ggl_times <- ggl_ads  %>%
    group_by(date_range_start, advertiser_name) %>% 
    summarise(spend_range_min = sum(spend_range_min_eur),
              spend_range_max = sum(spend_range_max_eur),
              spend_range_mid = sum(get_mid(spend_range_max_eur, spend_range_min_eur)),
              n = n()) %>% 
    ungroup() %>% 
    mutate(date_range_start = as.Date(date_range_start)) %>%
    complete(advertiser_name, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="day"), fill = list(n = 0, spend_range_min = 0, spend_range_mid = 0, spend_range_max = 0)) %>% 
    left_join(color_dat) %>% 
    assign_colors()
  
  ggl_gender <- ggl_ads %>%
    mutate(gender_targeting = ifelse(str_detect(gender_targeting, "Male") & str_detect(gender_targeting, "Female"), "Not targeted", gender_targeting)) %>%
    count(advertiser_name, gender_targeting) %>%
    group_by(advertiser_name) %>%
    mutate(perc = round(n/sum(n)*100, 2)) %>% 
    left_join(color_dat) %>% 
    assign_colors()
  
  ggl_age <- ggl_ads %>%
    mutate(age_targeting2 = case_when(
      str_detect(age_targeting, "18-24, 25-34, 35-44, 45-54, 55-64, ≥65") ~ "Not targeted",
      T ~ age_targeting
    )) %>%
    count(advertiser_name, age_targeting2) %>%
    complete(advertiser_name, age_targeting2, fill = list(n = 0)) %>%
    group_by(advertiser_name) %>%
    mutate(perc = round(n/sum(n)*100, 2))%>% 
    left_join(color_dat) %>% 
    assign_colors()
  
  ggl_geo <-  ggl_ads %>%
    mutate(geo_targeting_included = case_when(
      str_count(geo_targeting_included, ",") >= 11 ~ "Not targeted",
      str_detect(geo_targeting_included, "Netherlands") ~ "Not targeted",
      T ~ geo_targeting_included
    )) %>%
    count(advertiser_name, geo_targeting_included) %>%
    complete(advertiser_name, geo_targeting_included, fill = list(n = 0)) %>%
    group_by(advertiser_name) %>%
    mutate(perc = round(n/sum(n)*100, 2))%>% 
    left_join(color_dat) %>% 
    assign_colors()
  
  ggl_aggr <- list(total = ggl_total, times = ggl_times, gender = ggl_gender, age = ggl_age, geo = ggl_geo)
  
  tidytemplate::save_it(ggl_aggr)
  
  
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


library(Radlibrary)
library(httr)

get_fb_ads <- function() {
  
  token <- token_get()$token
  
  
  
  #link to fb api
  my_link<- "https://graph.facebook.com"
  
  #define fields you are interested in
  search_fields=c("ad_creation_time", 
                  "ad_delivery_start_time",
                  "ad_delivery_stop_time",
                  "ad_creative_link_caption",
                  "ad_creative_link_description",
                  "ad_creative_link_title",
                  "currency",
                  "ad_creative_body", 
                  "page_id",
                  "page_name",
                  "spend",
                  "demographic_distribution",
                  "funding_entity",
                  "impressions",
                  "region_distribution") %>% 
    stringr::str_c(., collapse=", ")
  
  #get the data from the first 'page' of data the api provides
  page_one_response <- GET(my_link,
                           path = "/ads_archive",
                           query = list(access_token = token,
                                        limit=100,
                                        ad_active_status="ALL",
                                        search_terms="''",
                                        impression_condition = 'HAS_IMPRESSIONS_LAST_90_DAYS',
                                        fields=search_fields,
                                        # token = token,
                                        ad_reached_countries="NL"))
  page_one_content<- content(page_one_response)
  
  x <- tibble(data=page_one_content$data)
  df_imp <- x %>% 
    unnest_wider(data) 
  
  #get the link refering to the next page
  next_link <- page_one_content$paging$`next`
  
  page <- 1
  
  #iterate over all pages until there is no further page
  while(length(next_link)>0) {
    # while(T) {
    
    print(page)
    
    next_response <- GET(next_link)
    next_content<- content(next_response)
    
    y <- tibble(data=next_content$data)
    df_next <- y %>% 
      unnest_wider(data) 
    
    df_imp <- bind_rows(df_imp, df_next)  
    
    next_link <- next_content$paging$`next`
    
    page <- page + 1
    
  }
  
  dutch_parties <- c("VVD", "D66", "Forum voor Democratie -FVD", "SP", "GroenLinks", "Volt Nederland", "Partij van de Arbeid (PvdA)", "CDA", "Partij voor de Dieren", "ChristenUnie")
  
  fb_dat <- readRDS("fb_dat/fb_dat.rds")
  
  fb_dat <- df_imp %>% 
    bind_rows(fb_dat) %>% 
    distinct(id, .keep_all = T)
  
  saveRDS(fb_dat, "fb_dat/fb_dat.rds")
  
  fb_dat_parties <- fb_dat %>% 
    mutate(ad_delivery_start_time = as.Date(ad_delivery_start_time)) %>% 
    filter(ad_delivery_start_time >= as.Date("2020-09-01")) %>% 
    filter(page_name %in% dutch_parties) 
  
  saveRDS(fb_dat_parties, "fb_dat/fb_dat_parties.rds")
  
  return(fb_dat)
  
}

fb_ads <- get_fb_ads()