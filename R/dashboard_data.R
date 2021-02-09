
library(tidyverse)

ggl_aggr <- readRDS("../data/ggl_aggr.rds")
fb_aggr <- readRDS("../data/fb_aggr.rds") 


update_time <- read_lines("../data/last_updated.txt") %>% 
  .[length(.)]


### Facebook setup
dutch_parties_fb <- c("VVD", "D66", "FvD", "SP", "GroenLinks", "Volt Nederland", "PvdA", "CDA", "PvdD", "ChristenUnie", "SGP", "DENK", "50PLUS")

advertiser_names_fb <- fb_aggr$times %>%
  distinct(advertiser_name) %>% 
  # filter(advertiser_name %in% more_than_5_fb) %>% 
  dplyr::pull(advertiser_name) %>% 
  ## only keep national political parties
  keep(~magrittr::is_in(.x, dutch_parties_fb))

defaults_fb <- advertiser_names_fb %>% 
  keep(~magrittr::is_in(.x, dutch_parties_fb))


dutch_parties <- c("D66", "VVD",
                   "GroenLinks", "SP", 
                   "Volt Nederland", "CDA", 
                   "ChristenUnie",
                   "PvdA", "FvD", "50PLUS")

advertiser_names_ggl <- ggl_aggr$total %>%
  pull(advertiser_name) %>% 
  keep(~magrittr::is_in(.x, dutch_parties))  %>% 
  discard(~magrittr::is_in(.x, c( "FvD")))


defaults_ggl <- advertiser_names_ggl %>% 
  keep(~magrittr::is_in(.x, dutch_parties))


color_dat <- tibble(
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#6f2421", "#e3101c", "#e01003", "#036b2c", "#02a6e9", "#562883", "#eeaa00", "#34c1c4", "#92107d"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "FvD", "PvdA", "SP", "PvdD", "ChristenUnie", "Volt Nederland", "SGP", "DENK", "50PLUS"))

