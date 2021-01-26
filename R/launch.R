
setwd("/home/fabio/main/DutchElectionObservatory")

library(git2r)
# sudo systemctl restart shiny-server.service
file.copy(from = "app/staging/helpers.R", to = "app/production", recursive = T, overwrite = T)
file.copy(from = "app/staging/index.html", to = "app/production", recursive = T, overwrite = T)
file.copy(from = "app/staging/index.Rmd", to = "app/production", recursive = T, overwrite = T)
file.copy(from = "app/staging/data", to = "app/production", recursive = T, overwrite = T)

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
    cmd3 = "git add --all"
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
# trans <- read_csv2("data/translation.csv")  %>%
#   select(text_id, contains("dutch")) %>%
#   data.table::transpose() %>% #glimpse
#   janitor::row_to_names(1) %>%
#   mutate_all(str_trim)%>%
#   mutate_all(~str_replace_all(.x, ", ", ","))
# 
# saveRDS(trans_eng, file = "data/trans_eng.rds")
# 
# trans_dutch <- translation %>%
#   select(text_id, contains("dutch")) %>%
#   data.table::transpose() %>%
#   janitor::row_to_names(1)
# 
# saveRDS(trans, file = "data/trans_dutch.rds")
# 
# trans_eng <- translation %>%
#   select(text_id, contains("english")) %>%
#   data.table::transpose() %>%
#   janitor::row_to_names(1)
# 
# saveRDS(trans, file = "data/trans_eng.rds")



while(T){
  
  
  currentTime <- Sys.time()
  
  cat("1. Load and Clean Data\n")
  source(here::here("R", "clean.R"))
  
  cat("2. Deploy App\n")
  
  # rsconnect::deployDoc("dashboard.Rmd", launch.browser = F, forceUpdate = T)
  # 
  # unlink("data/google-political-ads-transparency-bundle/", recursive = T)
  # unlink("data/PoliticalAds.csv")
  # unlink("data/readme.txt")
  
  # Configure git.
  git2r::config(user.name = "favstats", user.email = "fabio.votta@gmail.com")
  
  # Check git status.
  gitstatus()
  
  # Add and commit changes. 
  gitadd()
  
  # NEED TO PUSH ONCE AND DO THIS BEFORE THIS WORKS
  # git config --global credential.helper 'cache --timeout=10000000'
  
  gitcommit(msg = paste0(currentTime, " Automated Data Pull"))
  
  # Push changes to github.
  gitpush()
  
  
  Sys.sleep(60*60*24)
  
  # Sys.sleep(60*60*60)
  
  
}
