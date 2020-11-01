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
    mutate(advertiser_name = x$advertiser_name)%>% 
    mutate(advertiser_id = x$advertiser_id)
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

fb_dat <- readRDS("fb_dat/fb_dat.rds")


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

cat("\n\nFB Data: Get age/gender V\n\n")  

geo_targeted <- age_gender_targeted_raw %>% 
  mutate(row_number = 1:n()) %>% 
  # slice(1:10) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_geos) 

cat("\n\nFB Data: Get age/gender VI\n\n")  


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

cat("\n\nFB Data: Get geo\n\n")  


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

saveRDS(fb_aggr, "app/data/fb_aggr.rds")

cat("\n\nFB Data: Done\n\n") 