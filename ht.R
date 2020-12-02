
# install.packages("pacman")
library(instaloadeR)
library(reticulate)
library(purrr)
library(tidyverse)
# library(telegram.bot)
# # instaloadeR::install_instaloadeR()
# 
# # reticulate::source_python("scripty.py")
# 
# token <- "1251917882:AAERIxmse4EBS00qubKEldgN_a_qC1CQz-c"
# 
# updater <- Updater(token = token)
# 
# 
# bot <- Bot(token = token)

# reticulate::use_python("D:/ProgramData/Anaconda3/envs/r-reticulate/python.exe")

reticulate::use_python(py_config()$python)
# Sys.setenv(RETICULATE_PYTHON = "D:/ProgramData/Anaconda3/envs/r-reticulate/python.exe")

init_instaloadeR()

# insta_login(save = T)

# insta_login(load = T)

# +447424591732
# faf5100_kolambisk
# XdsfQWsder1234

hashies <- c("plandemic", "plannedemic", 
             "stop5grollout", "stop5g", "stop5gflorida", "stop5gglobal", "freedomkeepers", 
             "stop5gaustralia", "stop5guk", "stop5gitalia", "stop5gcalifornia", 
             "stop5ginternational", "stop5gtowers", "stop5gespaña", "stop5gbarcelona", 
             "stop5gusa", "stop5gcentralcoast", "stop5gpennsylvania", "stop5gtoday", 
             "stop5ghawaii", "stop5gitaly", "stop5gworldwide", "stop5geverywhere", 
             "governmentlies", "fuckbillgates", "informedconsent", "billgatesisevil", 
             "scamdemic", "vaccineinjury", "arrestbillgates", "markofthebeast", 
             "parentalrights", "fearmongering", "firefauci", "readtheinsert", 
             "savethechildren", "saveourchildren", "projectbluebeam", "mindcontrol", 
             "cabal", "event201", "id2020", "healthfreedom", "plandemic2020", 
             "coronalies", "medicalrights", "saynotobillgates", "wakeupsheeple", 
             "reopenusa", "populationcontrol", "idonotconsent", "believemothers", 
             "coronafake", "givegatesnochance", "medicalfreedomofchoice", 
             "gibgateskeinechance", "widerstand2020", "protruth", "fucknwo", 
             "fakevirus", "attilahildmann", "medicalexemption", "coronalüge", 
             "bodoschiffmann", "vaccinationchoice", "stopbillgates", "davidicke", 
             "wedonotconsent", "betweenmeandmydoctor", "truther", "outofshadows", 
             "hollyweirdisevil", "hisnamewassethrich", "andrenochrome", "georgesoros", 
             "wherewegoonewegoall", "wearethenewsnow", "filmyourhospital", 
             "qanonarmy", "qarmy", "thestormisuponus", "darktolight", "weareq", 
             "pizzagate", "nonewnormal", "rfidchip", "newworldorder", "agenda21", "stopthesteal",
             "billgates", "nwo", "fakenews", "illuminati", "covid1984", "releasethekraken",
             "deepstate", "depopulation", "mkultra", "agenda2030", "thegreatawakening", 
             "chemtrails", "qanon", "pedogate", "epsteindidntkillhimself", 
             "newworldorderagenda", "sheeple", "wwg1wga", "nasalies", "greatawakening", 
             "endtimes", "rothschild", "truthseeker", "falseflag", "bilderberggroup", 
             "querdenken", "zionistagenda", "plandemie", "fallofthecabal", 
             "devilworshippers", "theyarekillingus", "thegoyimknow", "falseflagoperations", 
             "satanicgovernments", "querdenken711", "querdenker", "fakepandemie", 
             "depopulationagenda", "adrenochrome", "redpill", "maskenzwang", 
             "obamagate", "plandemia", "covidhoax", "impfzwang", "nomask", 
             "noalnuevoordenmundial", "kenfm", "endthelockdown", "q", "antivax", 
             "medialies", "zwangsimpfung", "5gdangers", "reopenamerica", "scamdemic2020", 
             "nomaskneeded", "saynoto5gworldwide", "saynotomandatoryvaccinations", 
             "saynotomasks", "covidlies", "haarp", "covid19fake", "virusesarenotcontagious", 
             "elonmusk5gsatellites", "fuckmasks", "oneworldgovernment", "psychologicalwarfare", 
             "billgatesisamurderer", "billgatesrfid", "agenda21depopulationplan", 
             "covidgate", "no5g", "billgatescoronavirus", "60ghz", "wwg1wgaworldwide", 
             "thebaqery", "billgatesvaccine", "wuhanvirus", "thevirusscareisfake", 
             "antivaxx", "medicalfreedomactivist", "hugsovermasks", "falldemie", 
             "911insidejob", "endthelockdownnow", "stopwho", "globalhealthsecurityagenda", 
             "pedovore", "prisonplanet", "redpilled", "coronavirusisfake", "greatreset",
             "antivaccine", "plamdemic", "secretsocieties", "losvirusnoexisten", 
             "killilluminati", "fakepandemic", "covid19hoax", "pedowood", "conspiracy",
             "coronavirushoax", "coronafraude", "londonrealarmy", "stopptdenwahnsinn"
) %>% 
  sample(length(.))

# hashies %>% 
#    discard(~magrittr::equals(.x, "q")) %>% 
#    paste0(collapse = " OR ")
#    dput()

get_em <- function(hashtag) {
  
  # r_group <- 1009844052
  # 
  # # Send message
  # bot$sendMessage(chat_id = r_group,
  #                 text = paste0("Now scraping #", hashtag),
  #                 parse_mode = "Markdown"
  # )
  
  Sys.sleep(runif(1, 300))
  
  dat <- instaloadeR::insta_posts(query = hashtag, 
                                  scope = "hashtag",
                                  max_posts = 100000, 
                                  since = "2021-01-01",
                                  until = "2019-12-01",
                                  scrape_comments = F,
                                  save_path = glue::glue("../insta/{hashtag}.csv"))    
  
  # bot$sendMessage(chat_id = r_group,
  #                 text = paste0("Finished scraping #", hashtag, ". Data has ", nrow(dat), " rows."),
  #                 parse_mode = "Markdown"
  # )
  
}

get_em <- possibly(get_em, otherwise = tibble(perc = NULL), quiet = F)

while(T){
  hashies %>%
    purrr::walk(get_em)   
  
  Sys.sleep(runif(1, 3000))
  
}



# read_csv("data/plannedemic.csv")



