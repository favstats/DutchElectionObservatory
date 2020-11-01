pacman::p_load(tidyverse, janitor, highcharter, httr, furrr)

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

unnest_geos <- function(x) {
  # cat(glue::glue("{x$row_number} out of {nrow(age_gender_targeted_raw)} ({round(100*x$row_number/nrow(age_gender_targeted_raw), 2)}%)\n\n"))
  x %>% 
    pull(region_distribution) %>% 
    flatten() %>% 
    map_dfr(flatten) %>% 
    mutate(id = x$id)%>% 
    mutate(start_time = x$start_time)%>% 
    mutate(advertiser_name = x$advertiser_name)
}

unnest_dems <- function(x) {
  # cat(glue::glue("{x$row_number} out of {nrow(age_gender_targeted_raw)} ({round(100*x$row_number/nrow(age_gender_targeted_raw), 2)}%)\n\n"))
  x %>% 
    pull(demographic_distribution) %>% 
    flatten() %>% 
    map_dfr(flatten) %>% 
    mutate(id = x$id)
}

# Set a "plan" for how the code should run.
# plan(multisession, workers = 4)

# get_ggl_data <- function() {
  
cat("\n\nGet Google Data\n\n")  

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
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "SP", "PvdA", "FvD"))


ggl_ads <- data.table::fread("data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv") %>% 
  filter(str_detect(Regions, "NL")) %>%
  janitor::clean_names() %>% 
  # filter(advertiser_name %in% dutch_parties) %>%
  filter(date_range_start >= as.Date("2020-09-01")) %>% 
  mutate(advertiser_name = case_when(
    advertiser_name == 'Christen Democratisch Appèl' ~ "CDA",
    advertiser_name == 'SP (Socialistische Partij)' ~ "SP",
    advertiser_name == 'Partij van de Arbeid' ~ "PvdA",
    advertiser_name == 'Forum voor Democratie' ~ "FvD",
    T ~ advertiser_name
  ))

ggl_ads_old <- read_rds("data/ggl_ads.rds")

ggl_ads <- ggl_ads %>% 
  bind_rows(ggl_ads_old) %>% 
  distinct(ad_id, .keep_all = T)

ggl_total <- ggl_ads %>% 
  mutate(impressions_lower_bound = case_when(
    impressions == "≤ 10k" ~ 1,
    impressions == "10k-100k" ~ 10000,
    impressions == "100k-1M" ~ 100000,
    impressions == "1M-10M" ~ 1000000,
    # impressions == "> 10M" ~ 10000000,
    T ~ 0
  )) %>% 
  mutate(impressions_upper_bound = case_when(
    impressions == "≤ 10k" ~ 10000,
    impressions == "10k-100k" ~ 100000,
    impressions == "100k-1M" ~ 1000000,
    impressions == "1M-10M" ~ 10000000,
    # impressions == "> 10M" ~ 20000000,
    T ~ 0
  )) %>% 
  group_by(advertiser_name, advertiser_id) %>% 
  summarise(spend_range_min = sum(spend_range_min_eur),
            spend_range_max = sum(spend_range_max_eur),
            spend_range_mid = sum(get_mid(spend_range_max_eur, spend_range_min_eur)),
            impressions_range_min = sum(impressions_lower_bound),
            impressions_range_max = sum(impressions_upper_bound),
            impressions_range_mid = sum(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n = n()) %>% 
  ungroup() %>% 
  left_join(color_dat)  %>% 
  assign_colors()

# tidytemplate::save_it(ggl_total)

ggl_times <- ggl_ads  %>%
  mutate(impressions_lower_bound = case_when(
    impressions == "≤ 10k" ~ 1,
    impressions == "10k-100k" ~ 10000,
    impressions == "100k-1M" ~ 100000,
    impressions == "1M-10M" ~ 1000000,
    # impressions == "> 10M" ~ 10000000,
    T ~ 0
  )) %>% 
  mutate(impressions_upper_bound = case_when(
    impressions == "≤ 10k" ~ 10000,
    impressions == "10k-100k" ~ 100000,
    impressions == "100k-1M" ~ 1000000,
    impressions == "1M-10M" ~ 10000000,
    # impressions == "> 10M" ~ 20000000,
    T ~ 0
  )) %>% 
  group_by(date_range_start, advertiser_name, advertiser_id) %>% 
  summarise(spend_range_min = sum(spend_range_min_eur),
            spend_range_max = sum(spend_range_max_eur),
            spend_range_mid = sum(get_mid(spend_range_max_eur, spend_range_min_eur)),
            impressions_range_min = sum(impressions_lower_bound),
            impressions_range_max = sum(impressions_upper_bound),
            impressions_range_mid = sum(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n = n()) %>% 
  ungroup() %>% 
  mutate(date_range_start = as.Date(date_range_start)) %>%
  complete(advertiser_name, advertiser_id, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="day"), fill = list(n = 0, spend_range_min = 0, spend_range_mid = 0, spend_range_max = 0, impressions_range_min = 0, impressions_range_mid = 0, impressions_range_max = 0)) %>% 
  left_join(color_dat) %>% 
  assign_colors()

ggl_gender <- ggl_ads %>%
  mutate(gender_targeting = ifelse(str_detect(gender_targeting, "Male") & str_detect(gender_targeting, "Female"), "Not targeted", gender_targeting)) %>%
  count(advertiser_name, gender_targeting, advertiser_id) %>%
  group_by(advertiser_name, advertiser_id) %>%
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  assign_colors()

ggl_age <- ggl_ads %>%
  mutate(age_targeting2 = case_when(
    str_detect(age_targeting, "18-24, 25-34, 35-44, 45-54, 55-64, ≥65") ~ "Not targeted",
    T ~ age_targeting
  )) %>%
  count(advertiser_name, age_targeting2, advertiser_id) %>%
  complete(advertiser_name, advertiser_id, age_targeting2, fill = list(n = 0)) %>%
  group_by(advertiser_name, advertiser_id) %>%
  mutate(perc = round(n/sum(n)*100, 2))%>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  assign_colors()

ggl_geo <-  ggl_ads %>%
  mutate(geo_targeting_included = case_when(
    str_count(geo_targeting_included, ",") >= 11 ~ "Not targeted",
    str_detect(geo_targeting_included, "Netherlands") ~ "Not targeted",
    T ~ geo_targeting_included
  )) %>%
  count(advertiser_name, geo_targeting_included, advertiser_id) %>%
  complete(advertiser_name, advertiser_id, geo_targeting_included, fill = list(n = 0)) %>%
  group_by(advertiser_name, advertiser_id) %>%
  mutate(perc = round(n/sum(n)*100, 2))%>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  assign_colors()

ggl_aggr <- list(total = ggl_total, times = ggl_times, gender = ggl_gender, age = ggl_age, geo = ggl_geo)


saveRDS(ggl_aggr, file = "app/production/data/ggl_aggr.rds")
saveRDS(ggl_aggr, file = "app/staging/data/ggl_aggr.rds")


  
#   return(ggl_ads)
# }

# ggl_ads <- get_ggl_data()



saveRDS(ggl_ads, file = "data/ggl_ads.rds")


cat("\n\nGoogle Data: Done\n\n")  


cat("\n\nSnapchat Data: Getting it\n\n")  

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

saveRDS(snap_ads, file = "data/snap_ads.rds")
# gc()

cat("\n\nSnapchat Data: Done\n\n")  


# tidytemplate::save_it(snap_ads)

rm(snap_ads)
rm(ggl_ads)
rm(ggl_total)
rm(ggl_times)
rm(ggl_gender)
rm(ggl_age)
rm(ggl_geo)

gc()

cat("\n\nCleanup: Done\n\n")  

cat("\n\nFB Data: Getting it\n\n")  


# get_fb_ads <- function() {
  
readRenviron(".Renviron")

token <- Sys.getenv("fb_token")

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
                                      impression_condition = 'HAS_IMPRESSIONS_LAST_7_DAYS',
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

dutch_parties <- c("VVD", "D66", "FvD", "SP", "GroenLinks", "Volt Nederland", "PvdA", "CDA", "PvdD", "ChristenUnie")

cat("\n\nFB Data: Read in old data\n\n")  

fb_dat <- readRDS("fb_dat/fb_dat.rds")
# saveRDS(df_imp, "data/df_imp.rds")
# df_imp <- readRDS("data/df_imp.rds")

cat("\n\nFB Data: Merge data\n\n")  


fb_dat <- df_imp %>% 
  rename(advertiser_name = page_name) %>% 
  rename(advertiser_id = page_id) %>% 
  bind_rows(fb_dat) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(advertiser_name = case_when(
    advertiser_name == 'Partij voor de Dieren' ~ "PvdD",
    advertiser_name == 'Partij van de Arbeid (PvdA)' ~ "PvdA",
    advertiser_name == 'Forum voor Democratie -FVD' ~ "FvD",
    T ~ advertiser_name
  ))

saveRDS(fb_dat, "fb_dat/fb_dat.rds")

cat("\n\nFB Data: Save data\n\n")  


# fb_dat <- readRDS("fb_dat/fb_dat.rds")

rm(df_imp)
gc()

cat("\n\nGarbage collected\n\n")  

 
# rstudioapi::jobRunScript("get_fb.R")
  
  
  # fb_dat_parties <- fb_dat %>% 
  #   mutate(ad_delivery_start_time = as.Date(ad_delivery_start_time)) %>% 
  #   filter(ad_delivery_start_time >= as.Date("2020-09-01")) %>% 
  #   filter(advertiser_name %in% dutch_parties) 
  # 
  # saveRDS(fb_dat_parties, "fb_dat/fb_dat_parties.rds")
  
#   return(fb_dat)
#   
# }

# fb_ads <- get_fb_ads()

color_dat <- tibble(
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#6f2421", "#e3101c", "#e01003", "#036b2c", "#02a6e9", "#562883"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "FvD", "PvdA", "SP", "PvdD", "ChristenUnie", "Volt Nederland"))

cat("\n\nFB Data: Get totals\n\n")  


total_times <- fb_dat %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
  filter(date_range_start >= as.Date("2020-09-01")) %>% 
  unnest_wider(spend, names_sep = "_") %>%
  unnest_wider(impressions, names_sep = "_") %>%
  mutate_at(vars(spend_lower_bound, spend_upper_bound, impressions_lower_bound, impressions_upper_bound), as.numeric) 

fb_total <- total_times %>% 
  # drop_na(spend_lower_bound, spend_upper_bound, impressions_lower_bound, impressions_upper_bound) %>% 
  mutate(impressions_lower_bound = case_when(
    is.na(impressions_upper_bound) ~ 0, 
    is.na(impressions_lower_bound) ~ 0,
    T ~ impressions_lower_bound)) %>% 
  mutate(impressions_upper_bound = case_when(
    is.na(impressions_upper_bound) ~ 0, 
    is.na(impressions_lower_bound) ~ 0,
    T ~ impressions_upper_bound)) %>% 
  group_by(advertiser_name, advertiser_id) %>% 
  summarise(spend_range_min = sum(spend_lower_bound),
            spend_range_max = sum(spend_upper_bound),
            spend_range_mid = sum(get_mid(spend_lower_bound, spend_upper_bound)),
            impressions_range_min = sum(impressions_lower_bound),
            impressions_range_max = sum(impressions_upper_bound),
            impressions_range_mid = sum(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n = n()) %>% 
  ungroup() %>% 
  left_join(color_dat)  %>% 
  assign_colors()

cat("\n\nFB Data: Get times\n\n")  


# tidytemplate::save_it(fb_total)

fb_times <- total_times %>%
  group_by(date_range_start, advertiser_name) %>% 
  summarise(spend_range_min = sum(spend_lower_bound),
            spend_range_max = sum(spend_upper_bound),
            spend_range_mid = sum(get_mid(spend_lower_bound, spend_upper_bound)),
            impressions_range_min = sum(impressions_lower_bound),
            impressions_range_max = sum(impressions_upper_bound),
            impressions_range_mid = sum(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n = n()) %>% 
  ungroup() %>% 
  complete(advertiser_name, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="day"), fill = list(n = 0, spend_range_min = 0, spend_range_mid = 0, spend_range_max = 0, impressions_range_min = 0, impressions_range_mid = 0, impressions_range_max = 0)) %>% 
  left_join(color_dat) %>% 
  assign_colors() %>% 
  left_join(fb_total %>% select(advertiser_id, advertiser_name))

cat("\n\nFB Data: Get age/gender I\n\n")  


age_gender_targeted_raw <- fb_dat %>% 
  mutate(start_time = lubridate::as_datetime(ad_delivery_start_time) %>% lubridate::floor_date("day")) %>% 
  mutate(start_time = as.Date(start_time)) %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
  filter(date_range_start >= as.Date("2020-09-01")) 

cat("\n\nFB Data: Get age/gender II\n\n")  


age_gender_targeted <- age_gender_targeted_raw %>% 
  mutate(row_number = 1:n()) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_dems) %>% 
  right_join(age_gender_targeted_raw)

cat("\n\nFB Data: Get age/gender III\n\n")  


fb_gender <- age_gender_targeted  %>% 
  # filter(advertiser_name %in% dutch_parties) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  mutate(gender_age = paste(gender, age)) %>% 
  filter(!str_detect(gender_age, "unknown")) %>% 
  filter(!str_detect(gender_age, "13-17")) %>% 
  complete(gender, advertiser_name, age, fill = list(percentage = 0)) %>% 
  group_by(id, gender, advertiser_name) %>% 
  summarize(percentage = sum(percentage)) %>% 
  ungroup()%>% 
  mutate(percentage = percentage * 100) %>% 
  left_join(fb_total %>% select(advertiser_id, advertiser_name))

cat("\n\nFB Data: Get age/gender IV\n\n")  



fb_age <- age_gender_targeted  %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  mutate(gender_age = paste(gender, age)) %>% 
  filter(!str_detect(gender_age, "unknown")) %>% 
  filter(!str_detect(gender_age, "13-17")) %>% 
  complete(gender, age, advertiser_id, fill = list(percentage = 0)) %>% 
  group_by(id, age, advertiser_name, advertiser_id) %>% 
  summarize(percentage = sum(percentage)) %>% 
  ungroup() %>% 
  mutate(percentage = percentage * 100)

cat("\n\nFB Data: Get geo I\n\n")    

geo_targeted <- age_gender_targeted_raw %>% 
  mutate(row_number = 1:n()) %>% 
  # slice(1:10) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_geos) 

cat("\n\nFB Data: Get geo II\n\n")    


dutch_regions <- geo_targeted %>% 
  count(region, sort = T) %>% 
  slice(1:12) %>% pull(region)

geo_targeted <- geo_targeted  %>% 
  filter(start_time >= as.Date("2020-09-01")) %>% 
  # filter(advertiser_name %in% dutch_parties) %>% 
  filter(region %in% dutch_regions) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  complete(region, advertiser_name, fill = list(percentage = 0)) %>% 
  group_by(id, region, advertiser_name) %>% 
  summarize(percentage = sum(percentage)) %>% 
  ungroup()

cat("\n\nFB Data: Get geo III\n\n")    


mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/nl/nl-all.js"))

fb_geo <- mapdata %>% 
  left_join(geo_targeted %>% 
              rename(name = region) %>% 
              # filter(advertiser_name == "D66") %>% 
              group_by(name, advertiser_name) %>% 
              summarize(percentage = median(percentage, na.rm = T)) %>% 
              mutate(name = ifelse(name == "North Brabant", "Noord-Brabant", name))) %>% 
  mutate(percentage = round(100*percentage, 2)) %>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  rename(colorful = color) %>% 
  left_join(fb_total %>% select(advertiser_id, advertiser_name))


fb_aggr <- list(total = fb_total, times = fb_times, geo = fb_geo, gender = fb_gender, age = fb_age)

saveRDS(fb_aggr, "app/production/data/fb_aggr.rds")
saveRDS(fb_aggr, file = "app/staging/data/fb_aggr.rds")

cat("\n\nFB Data: Done\n\n") 