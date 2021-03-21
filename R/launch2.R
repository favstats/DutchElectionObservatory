

library(git2r)
library(tidyverse)

# setwd("/home/fabio/main/DutchElectionObservatory")
# Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
# setwd("C:/Users/favoo/Downloads/DutchElectionObservatory/DutchElectionObservatory")

# Git commit.
gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

# Git status.
gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

# Git add.
gitadd <- function(dir = getwd()){
  cmd_list <- list(
    # cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add -A"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

# Git push.
gitpush <- function(dir = getwd()){
  cmd_list <- list(
    # cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

## translation
translation <- read_csv2(here::here("site", "data", "translation.csv"))  

trans_dutch <- translation %>%
  select(text_id, contains("dutch")) %>%
  data.table::transpose() %>%
  janitor::row_to_names(1) %>%
  mutate_all(str_trim)%>%
  mutate_all(~str_replace_all(.x, ", ", ","))

saveRDS(trans_dutch, file = here::here("data", "trans_dutch.rds"))

trans_eng <- translation %>%
  select(text_id, contains("english")) %>%
  data.table::transpose() %>%
  janitor::row_to_names(1) %>%
  mutate_all(str_trim)%>%
  mutate_all(~str_replace_all(.x, ", ", ","))

saveRDS(trans_eng, file = here::here("data", "trans_eng.rds"))



# while(T){
  
  
  currentTime <- Sys.time()
  
  cat("1. Load and Clean Data\n")
  # source("clean.R")
  
  cat("2. Deploy App\n")
  
  
  rmarkdown::render_site(here::here("site", "en"))
  rmarkdown::render_site(here::here("site", "nl"))
  
  
  c('<script type="text/javascript" src="links.js"></script>') %>% 
    cat(file = "docs/include_footer.html", sep = "\n")
  
  
  # Configure git.
  git2r::config(user.name = "favstats", user.email = "fabio.votta@gmail.com")
  
  

  # Add and commit changes. 
  gert::git_add(".")
  
  # NEED TO PUSH ONCE AND DO THIS BEFORE THIS WORKS
  # git config --global credential.helper 'cache --timeout=10000000'
  
  gert::git_commit(message = paste0(currentTime, " Automated Data Pull"))
  
  
  # Push changes to github.
  gert::git_push()
  
  
  # Sys.sleep(60*60*24)
  
  # Sys.sleep(60*60*60)
  
  
# }

