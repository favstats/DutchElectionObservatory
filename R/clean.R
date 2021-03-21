### ---- Load Packages and Functions  ####


pacman::p_load(tidyverse, janitor, highcharter, httr, furrr, lubridate, tidytext)

setwd(here::here())

if(!dir.exists("data")) dir.create("data")

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
    dplyr::pull(region_distribution) %>% 
    flatten() %>% 
    map_dfr(flatten) %>% 
    mutate(id = x$id)%>% 
    mutate(start_time = x$start_time)%>% 
    mutate(advertiser_name = x$advertiser_name)
}

unnest_dems <- function(x) {
  # cat(glue::glue("{x$row_number} out of {nrow(age_gender_targeted_raw)} ({round(100*x$row_number/nrow(age_gender_targeted_raw), 2)}%)\n\n"))
  x %>% 
    dplyr::pull(demographic_distribution) %>% 
    flatten() %>% 
    map_dfr(flatten) %>% 
    mutate(id = x$id)
}


last_updated_time <- as.character(Sys.time())

cat(last_updated_time, file = "app/production/data/last_updated.txt", append = T, sep = "\n")
cat(last_updated_time, file = "app/staging/data/last_updated.txt", append = T, sep = "\n")
cat(last_updated_time, file = "site/data/last_updated.txt", append = T, sep = "\n")


### ---- Get Google data  ####

cat("\n\nGet Google Data\n\n")  

ggl_link <- "https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip"

ggl_file <- "data/ggl.zip"

download.file(ggl_link, ggl_file, mode="wb")
unzip(ggl_file, exdir = "data")

unlink(ggl_file)

# dir("data/google-political-ads-transparency-bundle", full.names =   T) %>% 
#   discard(~str_detect(.x,"creative")) %>% 
#   walk(file.remove)  

# dutch_parties <- c("D66", "VVD", "GroenLinks", "SP (Socialistische Partij)", "Volt Nederland", "Christen Democratisch Appèl", "Partij van de Arbeid", "FvD")

color_dat <- tibble(
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#e01003", "#e3101c", "#6f2421", "#02a6e9"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "SP", "PvdA", "FvD", "ChristenUnie"))


ggl_ads <- read_csv("data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv", locale = locale("nl")) %>% 
  filter(str_detect(Regions, "NL")) %>%
  janitor::clean_names() %>% 
  # filter(advertiser_name %in% dutch_parties) %>%
  filter(date_range_start >= as.Date("2020-09-06")) %>% 
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
  distinct(ad_id, .keep_all = T) %>% 
  filter(date_range_start >= as.Date("2020-09-06")) 

advertiser_emp_ggl <- ggl_ads %>%
  distinct(advertiser_name) %>% 
  dplyr::pull(advertiser_name)  

advertiser_ids_ggl <- ggl_ads %>%
  distinct(advertiser_id, .keep_all = T) %>% 
  select(advertiser_name, advertiser_id)  


weekly_spend <- read_csv("data/google-political-ads-transparency-bundle/google-political-ads-advertiser-weekly-spend.csv", locale = locale("nl"))

weekly_spend_ggl <- weekly_spend  %>% 
  janitor::clean_names() %>% 
  mutate(advertiser_name = case_when(
    advertiser_name == 'Christen Democratisch Appèl' ~ "CDA",
    advertiser_name == 'SP (Socialistische Partij)' ~ "SP",
    advertiser_name == 'Partij van de Arbeid' ~ "PvdA",
    advertiser_name == 'Forum voor Democratie' ~ "FvD",
    T ~ advertiser_name
  )) %>% 
  filter(week_start_date >= as.Date("2020-09-06")) %>% 
  filter(advertiser_name %in% advertiser_emp_ggl) %>% 
  mutate(spend_eur = ifelse(spend_eur == 0, 0.01, spend_eur)) %>% 
  complete(advertiser_name,
           week_start_date = seq.Date(min(week_start_date), max(week_start_date), by="week"),
           fill = list(spend_eur = 0)) %>% 
  select(advertiser_name, date_range_start = week_start_date, spend_eur) 

total_spend_ggl <- weekly_spend_ggl %>% 
  group_by(advertiser_name) %>% 
  summarize(spend_eur = sum(spend_eur))

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
  group_by(advertiser_name) %>% 
  summarise(spend_range_min = sum(spend_range_min_eur),
            spend_range_max = sum(spend_range_max_eur),
            spend_range_mid = sum(get_mid(spend_range_max_eur, spend_range_min_eur)),
            impressions_range_min = sum(impressions_lower_bound),
            impressions_range_max = sum(impressions_upper_bound),
            impressions_range_mid = sum(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n = n()) %>% 
  ungroup() %>% 
  left_join(color_dat)  %>% 
  assign_colors() %>% 
  left_join(total_spend_ggl) %>% 
  left_join(advertiser_ids_ggl)



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
  mutate(date_range_start = floor_date(date_range_start, "week")) %>% 
  group_by(date_range_start, advertiser_name) %>% 
  summarise(spend_range_min = sum(spend_range_min_eur),
            spend_range_max = sum(spend_range_max_eur),
            spend_range_mid = sum(get_mid(spend_range_max_eur, spend_range_min_eur)),
            impressions_range_min = sum(impressions_lower_bound),
            impressions_range_max = sum(impressions_upper_bound),
            impressions_range_mid = sum(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n = n()) %>% 
  ungroup() %>% 
  mutate(date_range_start = as.Date(date_range_start)) %>%
  complete(advertiser_name, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="week"), fill = list(n = 0, spend_range_min = 0, spend_range_mid = 0, spend_range_max = 0, impressions_range_min = 0, impressions_range_mid = 0, impressions_range_max = 0)) %>% 
  left_join(color_dat) %>% 
  assign_colors() %>% 
  left_join(weekly_spend_ggl)  %>% 
  left_join(advertiser_ids_ggl)

  # ggl_times %>%
  # # filter(advertiser_name == "CDA") %>%
  # group_by(advertiser_name) %>%
  # arrange(date_range_start) %>%
  # mutate_if(is.numeric, ~cumsum(.x)) %>%# View
  # ggplot(aes(date_range_start, spend_eur, color = advertiser_name)) +
  # geom_line()

ggl_gender <- ggl_ads %>%
  mutate(gender_targeting = ifelse(str_detect(gender_targeting, "Male") & str_detect(gender_targeting, "Female"), "Not targeted", gender_targeting)) %>%
  count(advertiser_name, gender_targeting) %>%
  group_by(advertiser_name) %>%
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  assign_colors() %>% 
  left_join(advertiser_ids_ggl)

ggl_age <- ggl_ads %>%
  mutate(age_targeting2 = case_when(
    str_detect(age_targeting, "18-24, 25-34, 35-44, 45-54, 55-64, ≥65") ~ "Not targeted",
    T ~ age_targeting
  )) %>%
  count(advertiser_name, age_targeting2) %>%
  complete(advertiser_name, age_targeting2, fill = list(n = 0)) %>%
  group_by(advertiser_name) %>%
  mutate(perc = round(n/sum(n)*100, 2))%>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  assign_colors() %>% 
  left_join(advertiser_ids_ggl)

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
  ungroup() %>% 
  left_join(color_dat) %>% 
  assign_colors() %>% 
  left_join(advertiser_ids_ggl)

ggl_aggr <- list(total = ggl_total, times = ggl_times, gender = ggl_gender, age = ggl_age, geo = ggl_geo)


saveRDS(ggl_aggr, file = "app/production/data/ggl_aggr.rds")
saveRDS(ggl_aggr, file = "app/staging/data/ggl_aggr.rds")
saveRDS(ggl_aggr, file = "site/data/ggl_aggr.rds")



#   return(ggl_ads)
# }

# ggl_ads <- get_ggl_data()



saveRDS(ggl_ads, file = "data/ggl_ads.rds")


cat("\n\nGoogle Data: Done\n\n")  

### ---- Get Snapchat data  ####


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

### ---- Get Facebook data  ####


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
                "ad_snapshot_url",
                "demographic_distribution",
                "funding_entity",
                "potential_reach",
                "publisher_platforms",
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
                                      ad_delivery_date_min = "2020-09-01",
                                      fields=search_fields,
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

# library(feather)
# # saveRDS(df_imp, "fb_dat/fb_dat_experimental.rds")
# write_feather(df_imp, "fb_dat/fb_dat_nl.feather")
# 
# library(jsonlite)
# 
# dat_r = toJSON(df_imp, dataframe = "rows")

# nl <- feather::read_feather("fb_dat/fb_dat_nl.feather")


# missing <- fb_dat %>% 
#   mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#   filter(date_range_start >= as.Date("2020-09-01")) %>% 
#   anti_join(df_imp %>% 
#               mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#               filter(date_range_start >= as.Date("2020-09-01")) %>% select(id))%>% filter(date_range_start <= as.Date("2020-12-01")) %>%  select(id)
# 
# data.table::fwrite(missing, file = "missings.csv")
# 
# fb_dat %>% 
#   mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#   filter(date_range_start >= as.Date("2020-09-01")) %>% 
#   anti_join(df_imp %>% 
#               mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#               filter(date_range_start >= as.Date("2020-09-01")) %>% select(id)) %>% 
#   unnest_wider(spend, names_sep = "_") %>%
#   unnest_wider(impressions, names_sep = "_") %>%
#   mutate_at(vars(spend_lower_bound, spend_upper_bound, impressions_lower_bound, impressions_upper_bound), as.numeric) %>% 
#   arrange(desc(spend_upper_bound)) %>% View
# 
# fb_dat %>% 
#   mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#   filter(date_range_start >= as.Date("2020-09-01")) %>% mutate(collection = "old") %>% 
#   bind_rows(df_imp %>% 
#               mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#               filter(date_range_start >= as.Date("2020-09-01")) %>% mutate(collection = "new")) %>%
#   count(date_range_start, collection) %>% 
#   ggplot(aes(date_range_start, n, color = collection)) +
#   geom_line()
# 
# df_imp %>% 
#   mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
#   filter(date_range_start >= as.Date("2020-09-01")) %>% nrow

dutch_parties <- c("VVD", "D66", "FvD", "SP", "GroenLinks", "Volt Nederland", "PvdA", "CDA", "PvdD", "ChristenUnie", "SGP", "DENK", "50PLUS")

cat("\n\nFB Data: Read in old data\n\n")  

fb_dat <- readRDS("fb_dat/fb_dat.rds")


# fb_dat <- fb_dat %>% 
#   mutate(advertiser_id = ifelse(is.na(advertiser_id), page_id, advertiser_id))


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
    advertiser_name == '50PLUSpartij' ~ "50PLUS",
    T ~ advertiser_name
  ))

saveRDS(fb_dat, "fb_dat/fb_dat.rds")

# fb_dat %>% dplyr::filter(advertiser_name == "BIJ1")

# saveRDS(fb_dat, "fb_dat/fb_dat_old.rds")


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
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#6f2421", "#e3101c", "#e01003", "#036b2c", "#02a6e9", "#562883", "#eeaa00", "#34c1c4", "#92107d", "#202122", "#242b57"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "FvD", "PvdA", "SP", "PvdD", "ChristenUnie", "Volt Nederland", "SGP", "DENK", "50PLUS", "BIJ1", "Ja21"))

cat("\n\nFB Data: Get totals\n\n")  


total_times <- fb_dat %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
  filter(date_range_start >= as.Date("2020-09-01")) %>% 
  unnest_wider(spend, names_sep = "_") %>%
  unnest_wider(impressions, names_sep = "_") %>% 
  unnest_wider(potential_reach , names_sep = "_") %>%
  mutate_at(vars(spend_lower_bound, spend_upper_bound, 
                 impressions_lower_bound, impressions_upper_bound, 
                 potential_reach_lower_bound, potential_reach_upper_bound), as.numeric) %>% 
  # drop_na(spend_lower_bound, spend_upper_bound, impressions_lower_bound, impressions_upper_bound) %>% 
  mutate(impressions_lower_bound = case_when(
    # is.na(impressions_upper_bound) ~ 0, 
    # is.na(impressions_lower_bound) ~ 0,
    impressions_lower_bound == 0 ~ 0.01, 
    T ~ impressions_lower_bound)) %>% 
  mutate(spend_lower_bound = case_when(
    spend_lower_bound == 0 ~ 0.01, 
    T ~ spend_lower_bound)) %>% 
  drop_na(impressions_lower_bound, impressions_upper_bound)

batch_id_dat <- total_times  %>% 
  # filter(is.na(advertiser_id)) %>% View
  mutate(unique_advertiser_id = as.numeric(as.factor(advertiser_name))) %>% 
  group_by(ad_creative_body, ad_creative_link_title, advertiser_name, advertiser_id) %>% 
  mutate(batch_id = paste0(unique_advertiser_id, "_", n(), "_", sample(10000:10000000000, size = 1)))%>% 
  ungroup() %>% 
  select(id, batch_id)

facebook_id_dat <- total_times  %>% 
  distinct(advertiser_name, .keep_all = T) %>% 
  select(advertiser_name, advertiser_id)

fb_total <- total_times  %>% 
  left_join(batch_id_dat) %>% 
  group_by(batch_id, advertiser_name) %>% 
  summarise(potential_reach_min = median(potential_reach_lower_bound) * n(),
            # potential_reach_max = median(potential_reach_upper_bound) ,
            # potential_reach_mid = median(get_mid(potential_reach_lower_bound, potential_reach_upper_bound)),
            spend_range_min = median(spend_lower_bound)  * n(),
            spend_range_max = median(spend_upper_bound)  * n(),
            spend_range_mid = median(get_mid(spend_lower_bound, spend_upper_bound)) * n() ,
            impressions_range_min = median(impressions_lower_bound) * n() ,
            impressions_range_max = median(impressions_upper_bound) * n() ,
            impressions_range_mid = median(get_mid(impressions_lower_bound, impressions_upper_bound)) * n(),
            n_ids = n()) %>% 
  ungroup() %>% 
  group_by(advertiser_name) %>%# View
  summarise(potential_reach_min = sum(potential_reach_min),
            # potential_reach_max = sum(potential_reach_max),
            # potential_reach_mid = sum(potential_reach_mid),
            spend_range_min = sum(spend_range_min),
            spend_range_max = sum(spend_range_max),
            spend_range_mid = sum(spend_range_mid),
            impressions_range_min = sum(impressions_range_min),
            impressions_range_max = sum(impressions_range_max),
            impressions_range_mid = sum(impressions_range_mid),
            n = n()) %>% 
  ungroup() %>% 
  left_join(color_dat)  %>% 
  assign_colors() %>% 
  left_join(facebook_id_dat)# %>% 
  # left_join(total_spend_fb)

cat("\n\nFB Data: Get times\n\n")  


# tidytemplate::save_it(fb_total)

fb_times <- total_times %>%
  left_join(batch_id_dat) %>% 
  group_by(batch_id) %>% 
  mutate(date_range_start = min(date_range_start)) %>% 
  ungroup() %>% 
  group_by(batch_id, advertiser_name, date_range_start) %>% 
  # group_by(date_range_start, ad_creative_body, ad_creative_link_title, advertiser_name) %>% 
  summarise(potential_reach_min = median(potential_reach_lower_bound)  * n(),
            # potential_reach_max = median(potential_reach_upper_bound) ,
            # potential_reach_mid = median(get_mid(potential_reach_lower_bound, potential_reach_upper_bound)),
            spend_range_min = median(spend_lower_bound)  * n(),
            spend_range_max = median(spend_upper_bound) * n() ,
            spend_range_mid = median(get_mid(spend_lower_bound, spend_upper_bound)) * n() ,
            impressions_range_min = median(impressions_lower_bound)  * n(),
            impressions_range_max = median(impressions_upper_bound) * n() ,
            impressions_range_mid = median(get_mid(impressions_lower_bound, impressions_upper_bound))  * n(),
            n_ids = n()) %>% 
  ungroup() %>% 
  group_by(date_range_start, advertiser_name) %>%# View
  summarise(potential_reach_min = sum(potential_reach_min),
            # potential_reach_max = sum(potential_reach_max),
            # potential_reach_mid = sum(potential_reach_mid),
            spend_range_min = sum(spend_range_min),
            spend_range_max = sum(spend_range_max),
            spend_range_mid = sum(spend_range_mid),
            impressions_range_min = sum(impressions_range_min),
            impressions_range_max = sum(impressions_range_max),
            impressions_range_mid = sum(impressions_range_mid),
            n = n()) %>% 
  ungroup() %>% 
  complete(advertiser_name, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="day"), fill = list(n = 0, spend_range_min = 0, spend_range_mid = 0, spend_range_max = 0, impressions_range_min = 0, impressions_range_mid = 0, impressions_range_max = 0)) %>% 
  left_join(color_dat) %>% 
  assign_colors()  %>% 
  left_join(facebook_id_dat)

# fb_times %>% 
#   left_join(daily_spend_fb) %>%
#   # count(date_range_start, advertiser_name) %>% 
#   filter(advertiser_name %in% dutch_parties) %>% 
#   drop_na(amount_spent_eur) %>% 
#     ggplot(aes(date_range_start, amount_spent_eur, color = advertiser_name)) +
#     geom_line()

# fb_times %>% 
#   dplyr::filter(advertiser_name == "VVD") %>% 
#   distinct(date_range_start)
# 
# total_times %>% 
#   filter(advertiser_name == "VVD") %>% View
# 
# fb_total %>%
#   dplyr::filter(advertiser_name == "VVD") %>% 
#   mutate_if(is.numeric, ~cumsum(.x)) %>%
#   dplyr::pull(n)
# 
# fb_times %>%
#   dplyr::filter(advertiser_name == "VVD") %>% 
#   # View
#   mutate_if(is.numeric, ~cumsum(.x)) %>%
#   dplyr::pull(n)
#   max(.$n)
#   ggplot(aes(date_range_start, n)) +
#   geom_line()

cat("\n\nFB Data: Get age/gender I\n\n")  


age_gender_targeted_raw <- fb_dat %>% 
  mutate(start_time = lubridate::as_datetime(ad_delivery_start_time) %>% lubridate::floor_date("day")) %>% 
  mutate(start_time = as.Date(start_time)) %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
  filter(date_range_start >= as.Date("2020-09-01")) 

cat("\n\nFB Data: Get age/gender II\n\n")  

unnest_dems <- possibly(unnest_dems, otherwise = NULL, quiet = F)

age_gender_targeted <- age_gender_targeted_raw %>% 
  # slice(1) %>% 
  mutate(row_number = 1:n()) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_dems) %>% 
  right_join(age_gender_targeted_raw)


cat("\n\nFB Data: Get age/gender III\n\n")  

### Facebook setup
dutch_parties_fb <- c("VVD", "D66", "FvD", "SP", "GroenLinks", "Volt Nederland", "PvdA", "CDA", "PvdD", "ChristenUnie", "SGP", "DENK", "50PLUS", "BIJ1", "Ja21")


fb_gender <- age_gender_targeted  %>% 
  # filter(advertiser_name %in% dutch_parties_fb) %>%
  mutate(percentage = as.numeric(percentage)) %>% 
  mutate(gender_age = paste(gender, age)) %>% 
  filter(!str_detect(gender_age, "unknown")) %>% 
  filter(!str_detect(gender_age, "13-17")) %>% 
  filter(!str_detect(gender_age, "All")) %>% 
  drop_na(gender) %>% 
  group_by(advertiser_name) %>% 
  complete(id, gender, advertiser_name, age, fill = list(percentage = 0)) %>% 
  ungroup() %>% 
  group_by(id, gender, advertiser_name) %>% 
  summarize(percentage = sum(percentage)) %>% 
  ungroup() %>% 
  mutate(percentage = round(percentage * 100, 2)) %>% 
  left_join(batch_id_dat) %>% 
  group_by(advertiser_name, batch_id, gender) %>%
  summarise(percentage = median(percentage)) %>% 
  ungroup() %>% 
  drop_na(batch_id) %>% 
  group_by(advertiser_name) %>% 
  complete(batch_id, gender, fill = list(percentage = 0)) %>%
  ungroup() %>% 
  left_join(facebook_id_dat)  #%>% 
# filter(advertiser_name == "PvdA")

# fb_gender  %>%
#   filter(advertiser_name == "GroenLinks") %>% 
#   ggplot(aes(gender, percentage)) +
#   geom_boxplot()
#   mutate(percentage = as.numeric(percentage)) %>% 
#   mutate(gender_age = paste(gender, age)) %>% 
#   filter(!str_detect(gender_age, "unknown")) %>% 
#   filter(!str_detect(gender_age, "13-17")) %>% 
#   filter(!str_detect(gender_age, "All")) %>% 
#   drop_na(gender) %>% 
#   complete(id,gender, advertiser_name, age, fill = list(percentage = 0)) %>% 
#   group_by(id, gender, advertiser_name) %>% 
#   summarize(percentage = sum(percentage)) %>% 
#   ungroup() %>% 
#   mutate(percentage = round(percentage * 100, 2)) %>% 
#   left_join(batch_id_dat) %>% 
#   filter(batch_id == "1158_1_1112457630") %>% View
#   group_by(advertiser_name, batch_id, gender) %>%
#   summarise(percentage = median(percentage)) %>% 
#   ungroup()

#   fb_gender %>% 
#     group_by(advertiser_name) %>% 
#     complete(batch_id, gender, fill = list(percentage = 0)) %>%
#     ungroup() %>% 
#     left_join(facebook_id_dat) %>% 
#     filter(advertiser_name == "PvdA") %>%  View
#   
# fb_gender %>% 
#   filter(advertiser_name == "PvdA") %>% View
# 
  # fb_gender %>%
  #   count(batch_id, advertiser_name, sort = T) %>% 
  #   count(batch_id) %>% 
  #   arrange(n)
  # filter(batch_id == "1158_1_1112457630") %>% View
# 
# batch_id_dat %>% 
#   filter(batch_id == "2014_1_6732740374")

cat("\n\nFB Data: Get age/gender IV\n\n")  



fb_age <- age_gender_targeted  %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  mutate(gender_age = paste(gender, age)) %>% 
  filter(!str_detect(gender_age, "unknown")) %>% 
  filter(!str_detect(gender_age, "13-17")) %>% 
  filter(!str_detect(gender_age, "All")) %>% 
  group_by(advertiser_name) %>% 
  complete(id, gender, advertiser_name, age, fill = list(percentage = 0)) %>% 
  ungroup() %>% 
  group_by(id, age, advertiser_name) %>% 
  summarize(percentage = sum(percentage)) %>% 
  ungroup() %>% 
  mutate(percentage = round(percentage * 100, 2)) %>% 
  left_join(batch_id_dat) %>% 
  group_by(advertiser_name, batch_id, age) %>%
  summarise(percentage = median(percentage)) %>% 
  ungroup() %>% 
  drop_na(batch_id, age) %>% 
  group_by(advertiser_name) %>% 
  complete(batch_id, age, fill = list(percentage = 0)) %>%
  ungroup() %>% 
  left_join(facebook_id_dat) 

cat("\n\nFB Data: Get geo I\n\n")    

unnest_geos <- possibly(unnest_geos, otherwise = NULL, quiet = F)

geo_targeted <- age_gender_targeted_raw %>% 
  mutate(row_number = 1:n()) %>% 
  # slice(1:10) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_geos) 

cat("\n\nFB Data: Get geo II\n\n")    


dutch_regions <- geo_targeted %>% 
  count(region, sort = T) %>% 
  slice(1:12) %>% dplyr::pull(region)

geo_targeted_dat <- geo_targeted  %>% 
  filter(start_time >= as.Date("2020-09-01")) %>% 
  # filter(advertiser_name %in% dutch_parties) %>% 
  filter(region %in% dutch_regions) %>% 
  mutate(percentage = as.numeric(percentage))%>% 
  complete(id, region, fill = list(percentage = 0)) %>% 
  group_by(id) %>%
  tidyr::fill(start_time, advertiser_name, .direction = "downup") %>% 
  ungroup() %>%
  group_by(id, region, advertiser_name) %>% 
  summarize(percentage = sum(percentage)) %>% 
  ungroup() %>% 
  left_join(batch_id_dat) %>% 
  drop_na(batch_id) %>% 
  group_by(advertiser_name, batch_id, region) %>%
  summarise(percentage = mean(percentage)) %>% 
  ungroup() %>%  
  group_by(advertiser_name) %>% 
  complete(batch_id, region, fill = list(percentage = 0)) %>%
  ungroup() 


cat("\n\nFB Data: Get geo III\n\n")    


mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/nl/nl-all.js"))

fb_geo <- mapdata %>% 
  left_join(geo_targeted_dat %>% 
              rename(name = region) %>% 
              # filter(advertiser_name == "D66") %>% 
              group_by(name, advertiser_name) %>% 
              summarize(percentage = median(percentage, na.rm = T)) %>% 
              mutate(name = ifelse(name == "North Brabant", "Noord-Brabant", name))) %>% 
  ungroup() %>% 
  left_join(color_dat) %>% 
  rename(colorful = color) %>% 
  left_join(facebook_id_dat) 

fb_geo <- fb_geo %>% 
  mutate(percentage = round(100*percentage, 2))


fb_reach <- total_times  %>% 
  left_join(batch_id_dat) %>% 
  group_by(batch_id, advertiser_name) %>% 
  summarise(potential_reach_min = median(potential_reach_lower_bound),
            n_ids = n()) %>% 
  ungroup() %>% 
  left_join(color_dat)  %>% 
  assign_colors() %>% 
  left_join(facebook_id_dat)

## Extract Spending data from report
# dir("../../../data/spending/daily", full.names = T) %>% 
#   # discard(~str_detect(.x, "2020-11")) %>%
#   walk(~unzip(.x, exdir = "data/fb_spending", overwrite = T))

read_in_spend <- function(x) {
  read_csv(x) %>%
    mutate(date_range_start = str_remove_all(x, "data/fb_spending//FacebookAdLibraryReport_|data/fb_spending/FacebookAdLibraryReport_|_NL_last_90_days_advertisers.csv|_NL_yesterday.csv|data/fb_spending/90days/FacebookAdLibraryReport_") %>% lubridate::as_date()
    ) %>% 
    janitor::clean_names()  %>% 
    rename(advertiser_name = page_name) %>% 
    rename(advertiser_id = page_id) %>% 
    mutate(advertiser_name = case_when(
      advertiser_name == 'Partij voor de Dieren' ~ "PvdD",
      advertiser_name == 'Partij van de Arbeid (PvdA)' ~ "PvdA",
      advertiser_name == 'Forum voor Democratie -FVD' ~ "FvD",
      advertiser_name == '50PLUSpartij' ~ "50PLUS",
      T ~ advertiser_name
    ))
}

detect_ge <- function(x) {
  
   
   yes100 <- parse_number(x) == 100
   
   str_count(x) == 4 & str_detect(x, "100") & yes100
}
  
  

# detect_ge("100")


### Data Since September 1st

sepfirst <- read_in_spend("data/fb_spending/90days/FacebookAdLibraryReport_2020-12-01_NL_last_90_days_advertisers.csv") %>% 
  rename(spent_since_sepfirst = amount_spent_eur) %>%
  ## treat spent under 100 as NA
  mutate(spent_since_sepfirst = ifelse(detect_ge(spent_since_sepfirst), NA,
                                       as.numeric(spent_since_sepfirst))) %>% 
  mutate(advertiser_name = case_when(
    str_detect(disclaimer, "CDA") ~ "CDA",
    str_detect(disclaimer, "GroenLinks") ~ "GroenLinks",
    str_detect(disclaimer, "PvdA|Partij van de Arbeid") ~ "PvdA",
    str_detect(disclaimer, "D66") ~ "D66",
    str_detect(disclaimer, "SP") ~ "SP",
    str_detect(disclaimer, "ChristenUnie") ~ "ChristenUnie",
    str_detect(disclaimer, "VVD") ~ "VVD",
    str_detect(disclaimer, "FvD|FVD") ~ "FvD",
    str_detect(disclaimer, "JA21|Ja21") ~ "Ja21",
    str_detect(disclaimer, "Partij voor de Dieren|PvdD") ~ "PvdD",
    str_detect(disclaimer, "Libertaire Partij") ~ "Libertaire Partij (LP)",
    str_detect(disclaimer, "Code Oranje") ~ "Code Oranje",
    str_detect(disclaimer, "DENK") ~ "DENK",
    str_detect(disclaimer, "BIJ1") ~ "BIJ1",
    str_detect(disclaimer, "SGP") ~ "SGP",
    str_detect(disclaimer, "Volt") ~ "Volt Nederland",
    str_detect(disclaimer, "50PLUS") ~ "50PLUS",
    T ~ NA_character_
  )) %>% 
  group_by(advertiser_name, date_range_start) %>%
  summarize(n_below_100 = sum(is.na(spent_since_sepfirst)),
            spent_since_sepfirst = sum(spent_since_sepfirst, na.rm = T)) %>% 
  ungroup()  %>%
  bind_rows(
    tibble(advertiser_name = c("50PLUS", "BIJ1", "Ja21", "Volt Nederland"),
           date_range_start = c(lubridate::as_date("2020-12-01"),
                                lubridate::as_date("2020-12-01"),
                                lubridate::as_date("2020-12-01"),
                                lubridate::as_date("2020-12-01")),
           n_below_100 = c(0, 0, 0, 0),
           spent_since_sepfirst = c(0, 0, 0, 0))    
  )


### Daily Spending data

daily_spend_fb <- dir("data/fb_spending/", full.names = T) %>% 
  keep(~str_detect(.x, "advertisers")) %>% 
  map_dfr(read_in_spend) %>% 
  filter(date_range_start >= as.Date("2020-12-02")) %>%
  # select(-advertiser_id, -disclaimer)%>%
  complete(advertiser_name, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="day"),
           fill = list(amount_spent_eur = 0, number_of_ads_in_library = 0)) %>%
  # filter(detect_ge(amount_spent_eur)) %>% 
  mutate(amount_spent_eur = ifelse(detect_ge(amount_spent_eur), 50,
                                   as.numeric(amount_spent_eur))) %>% 
  # filter(advertiser_name == "DENK")# %>%
  mutate(advertiser_name = case_when(
    str_detect(disclaimer, "CDA") ~ "CDA",
    str_detect(disclaimer, "GroenLinks") ~ "GroenLinks",
    str_detect(disclaimer, "PvdA|Partij van de Arbeid") ~ "PvdA",
    str_detect(disclaimer, "D66") ~ "D66",
    str_detect(disclaimer, "SP") ~ "SP",
    str_detect(disclaimer, "ChristenUnie") ~ "ChristenUnie",
    str_detect(disclaimer, "VVD") ~ "VVD",
    str_detect(disclaimer, "FvD|FVD") ~ "FvD",
    str_detect(disclaimer, "JA21|Ja21") ~ "Ja21",
    str_detect(disclaimer, "Partij voor de Dieren|PvdD") ~ "PvdD",
    str_detect(disclaimer, "Libertaire Partij") ~ "Libertaire Partij (LP)",
    str_detect(disclaimer, "Code Oranje") ~ "Code Oranje",
    str_detect(disclaimer, "DENK") ~ "DENK",
    str_detect(disclaimer, "BIJ1") ~ "BIJ1",
    str_detect(disclaimer, "SGP") ~ "SGP",
    str_detect(disclaimer, "Volt") ~ "Volt Nederland",
    str_detect(disclaimer, "50PLUS") ~ "50PLUS",
    T ~ NA_character_
  )) %>% 
  group_by(advertiser_name, date_range_start) %>%
  summarize( n_below_100 = sum(amount_spent_eur==50),
             amount_spent_eur = sum(amount_spent_eur, na.rm = T),
  ) %>% 
  ungroup() 


### Prep sepfirst data for merging


sepfirst_merger <- sepfirst %>% 
  mutate(cum_spent = spent_since_sepfirst) %>% 
  select(advertiser_name, cum_spent, spent_since_sepfirst, date_range_start, n_below_100) %>% 
  mutate(spent_lower_bound = cum_spent, 
         spent_upper_bound = cum_spent)  %>% 
  # just keep party data for now
  filter(advertiser_name %in% color_dat$advertiser_name) 


### Combine Spending data

# spending %>%
#   filter(advertiser_name == "50PLUS") %>% View


spend_times_data <- sepfirst %>% 
  ## we don't need these two variables
  select(-n_below_100, -date_range_start) %>%
  ## merge the sep first data into the daily spend data
  right_join(daily_spend_fb %>% select(advertiser_name, n_below_100, amount_spent_eur, date_range_start)) %>% 
  # just keep party data for now
  filter(advertiser_name %in% color_dat$advertiser_name) %>%
  ## arrange by advertiser and date start
  arrange(desc(advertiser_name), date_range_start) %>% 
  group_by(advertiser_name)  %>% 
  ## first value is data so far plus 2nd December
  mutate(spent = ifelse(row_number()==1, amount_spent_eur + spent_since_sepfirst, amount_spent_eur)) %>%
  ## cumulative spent
  mutate(cum_spent = cumsum(spent)) %>%
  ## the lower bound
  mutate(spent_lower_bound = case_when(
    n_below_100 != 0 & row_number()==1 ~ cum_spent+(n_below_100*-49.99),
    n_below_100 != 0 ~ (n_below_100*0.01),
    T ~ spent
  )) %>% 
  ## the upper bound
  mutate(spent_upper_bound = case_when(
    n_below_100 != 0 & row_number()==1 ~ cum_spent+(n_below_100*100),
    n_below_100 != 0 ~ (n_below_100*100),
    T ~ spent
  )) %>%
  ## cumulative sum of lower and upper bound
  mutate(spent_lower_bound = cumsum(spent_lower_bound)) %>% 
  mutate(spent_upper_bound = cumsum(spent_upper_bound)) %>% 
  ungroup() %>% 
  select(-amount_spent_eur, -spent) %>% 
  bind_rows(sepfirst_merger)



spending <- spend_times_data %>% 
  distinct(advertiser_name, .keep_all = T) %>% 
  ## estimate daily growth data with mean
  mutate(daily_growth = spent_since_sepfirst/90) %>% 
  select(advertiser_name, spent_since_sepfirst, daily_growth) %>% 
  mutate(date_range_start = as.Date("2020-09-01")) %>% 
  ## complete data
  complete(advertiser_name, date_range_start = seq.Date(as.Date("2020-09-01"), as.Date("2020-12-01"), by="day")) %>%
  group_by(advertiser_name) %>% 
  mutate(spent_since_sepfirst = spent_since_sepfirst[1],
         daily_growth = daily_growth[1]) %>% 
  mutate(cum_spent = cumsum(daily_growth)) %>% 
  ungroup() %>% 
  mutate(type = "before",
         n_below_100 = 0,
         spent_lower_bound = cum_spent,
         spent_upper_bound = cum_spent) %>% 
  ## add spend times data
  bind_rows(spend_times_data %>% mutate(type = "after")) %>% 
  left_join(color_dat)

read_in_spend_loc <- function(x) {
  read_csv(x) %>% 
    janitor::clean_names() %>% 
    mutate(path = x) 
}

fb_spend_locs <- dir("data/fb_spending/regions", full.names = T) %>% map_dfr(read_in_spend_loc)


spending_loc <- fb_spend_locs %>% 
  mutate(date_region = str_remove_all(path, "data/fb_spending/regions/FacebookAdLibraryReport_|_NL_yesterday|.csv")) %>% 
  separate(date_region, into = c("date_range_started", "region"), sep = "_") %>% 
  mutate(date_range_started = lubridate::as_date(date_range_started)) %>% 
  mutate(amount_spent_eur = ifelse(detect_ge(amount_spent_eur), sample(1:100, length(.)),
                                   as.numeric(amount_spent_eur))) %>% 
  arrange(desc(amount_spent_eur))  %>%
  rename(advertiser_name = page_name) %>% 
  rename(advertiser_id = page_id) %>% 
  mutate(advertiser_name = case_when(
    advertiser_name == 'Partij voor de Dieren' ~ "PvdD",
    advertiser_name == 'Partij van de Arbeid (PvdA)' ~ "PvdA",
    advertiser_name == 'Forum voor Democratie -FVD' ~ "FvD",
    advertiser_name == '50PLUSpartij' ~ "50PLUS",
    T ~ advertiser_name
  )) %>% 
  mutate(advertiser_name = case_when(
    str_detect(disclaimer, "CDA") ~ "CDA",
    str_detect(disclaimer, "GroenLinks") ~ "GroenLinks",
    str_detect(disclaimer, "PvdA|Partij van de Arbeid") ~ "PvdA",
    str_detect(disclaimer, "D66") ~ "D66",
    str_detect(disclaimer, "SP") ~ "SP",
    str_detect(disclaimer, "ChristenUnie") ~ "ChristenUnie",
    str_detect(disclaimer, "VVD") ~ "VVD",
    str_detect(disclaimer, "FvD|FVD") ~ "FvD",
    str_detect(disclaimer, "JA21|Ja21") ~ "Ja21",
    str_detect(disclaimer, "Partij voor de Dieren|PvdD") ~ "PvdD",
    str_detect(disclaimer, "Libertaire Partij") ~ "Libertaire Partij (LP)",
    str_detect(disclaimer, "Code Oranje") ~ "Code Oranje",
    str_detect(disclaimer, "DENK") ~ "DENK",
    str_detect(disclaimer, "BIJ1") ~ "BIJ1",
    str_detect(disclaimer, "SGP") ~ "SGP",
    str_detect(disclaimer, "Volt") ~ "Volt Nederland",
    str_detect(disclaimer, "50PLUS") ~ "50PLUS",
    T ~ NA_character_
  ))

# fb_spend_locs %>% 
#   filter(str_detect(disclaimer,"50PLUS")) %>% View

# fb_aggr <- readRDS("site/data/fb_aggr.rds")
# 
# fb_aggr$report_spending <- spending
# fb_aggr$report_spending_loc <- spending_loc

# saveRDS(fb_aggr, "site/data/fb_aggr.rds")

fb_aggr <- list(total = fb_total, times = fb_times,
                geo = fb_geo, gender = fb_gender,
                age = fb_age, reach = fb_reach,
                report_spending = spending,
                report_spending_loc = spending_loc)

### create graph ######

color_dat <- tibble(
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#6f2421", "#e3101c", "#e01003", "#036b2c", "#02a6e9", "#562883", "#eeaa00", "#34c1c4", "#92107d", "#202122", "#242b57"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "FvD", "PvdA", "SP", "PvdD", "ChristenUnie", "Volt Nederland", "SGP", "DENK", "50PLUS", "BIJ1", "Ja21")) %>% 
  mutate(advertiser_name = as.factor(advertiser_name))

extrastop <- c(lsa::stopwords_nl, "https", "een", 
               "http", "te", "deze", "lid.php", "fvd.nl",
               "www.stemvolt.nl", "50pluspartij.nl", "ja", 
               "bit.ly", "youtu.be", "17", "16", "15")



party_colors <- color_dat$color
names(party_colors) <- color_dat$advertiser_name

party_names <- c("CDA", "GroenLinks", "Partij van de Arbeid (PvdA)", "D66", "SP", "VVD", "ChristenUnie", "Forum voor Democratie -FVD", "Partij voor de Dieren", "JA21.nl", "Libertaire Partij - LP", "Code Oranje", "SGP", "DENK", "BIJ1", "Volt Nederland", "Partij van de Arbeid", "PvDA", "FvD", "FVD", "JA21", "Ja21", "PvdD", "50PLUS") %>% 
  paste0(collapse = "|")


party_texts <- fb_dat %>% 
  filter(str_detect(funding_entity, party_names)) %>%
  mutate(party = case_when(
    str_detect(funding_entity, "CDA") ~ "CDA",
    str_detect(funding_entity, "GroenLinks") ~ "GroenLinks",
    str_detect(funding_entity, "PvdA|Partij van de Arbeid") ~ "PvdA",
    str_detect(funding_entity, "D66") ~ "D66",
    str_detect(funding_entity, "SP") ~ "SP",
    str_detect(funding_entity, "ChristenUnie") ~ "ChristenUnie",
    str_detect(funding_entity, "VVD") ~ "VVD",
    str_detect(funding_entity, "FvD|FVD") ~ "FvD",
    str_detect(funding_entity, "JA21|Ja21") ~ "Ja21",
    str_detect(funding_entity, "Partij voor de Dieren|PvdD") ~ "PvdD",
    str_detect(funding_entity, "Libertaire Partij") ~ "Libertaire Partij (LP)",
    str_detect(funding_entity, "Code Oranje") ~ "Code Oranje",
    str_detect(funding_entity, "DENK") ~ "DENK",
    str_detect(funding_entity, "BIJ1") ~ "BIJ1",
    str_detect(funding_entity, "SGP") ~ "SGP",
    str_detect(funding_entity, "Volt") ~ "Volt Nederland",
    str_detect(funding_entity, "50PLUS") ~ "50PLUS",
    T ~ NA_character_
  )) %>% 
  # filter(advertiser_name %in% color_dat$advertiser_name) %>% 
  mutate(text = paste0(ad_creative_body)) 


party_texts %>% 
  # filter(party == "50PLUS") %>%   distinct(text, .keep_all = T) %>% 
  # select(text) %>% View
  filter(!party %in% c("Code Oranje", "Libertaire Partij (LP)")) %>% 
  distinct(text, .keep_all = T) %>% 
  tidytext::unnest_tokens(word, text) %>% 
  count(party, word, sort = T) %>% 
  filter(!word %in% extrastop) %>% 
  group_by(party) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  # mutate(word = fct_reorder2(word, n, advertiser_name)) %>% 
  mutate(word = reorder_within(word, n, party))  %>% 
  mutate(party = as.factor(party)) %>% 
  ggplot(aes(word, n, fill = party)) +
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~party, scales = "free", ncol = 3) +
  coord_flip() +
  scale_fill_manual(values = party_colors) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = here::here("images", "top_words.png"), height = 14, width = 12)

# fb_aggr$report_spending <- spending
# fb_aggr$report_spending_loc <- spending_loc


saveRDS(fb_aggr, "app/production/data/fb_aggr.rds")
saveRDS(fb_aggr, "app/staging/data/fb_aggr.rds")
saveRDS(fb_aggr, file = "site/data/fb_aggr.rds")


party_bigrams <- party_texts %>% 
  filter(str_detect(funding_entity, party_names)) %>%
  mutate(text = paste0(ad_creative_body)) %>% 
  distinct(text, .keep_all = T) %>% 
  # slice(1:10) %>% 
  select(text, party) %>% 
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(party, bigram, sort = T)

bigramstops <- c("https 50pluspartij.nl", "50pluspartij.nl actueel", "meer https", "leer meer", "meer over", "bij1 org", "fvd.nl ja", "https ja21", "ja21 nl", "bigram lid.php", "https www.partijvordedieren.nl", "www.stemvolt.nl na", "weten www.stemvolt.nl")

party_bigrams  %>% 
  filter(!bigram %in% bigramstops) %>% 
  filter(!party %in% c("Code Oranje", "Libertaire Partij (LP)")) %>% 
  group_by(party) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  # mutate(bigram = fct_reorder2(bigram, n, advertiser_name)) %>% 
  mutate(bigram = reorder_within(bigram, n, party))  %>% 
  mutate(party = as.factor(party)) %>% 
  ggplot(aes(bigram, n, fill = party)) +
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~party, scales = "free", ncol = 3) +
  coord_flip() +
  scale_fill_manual(values = party_colors) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(filename = here::here("images", "top_bigrams.png"), height = 14, width = 12)


cat("\n\nFB Data: Done\n\n") 