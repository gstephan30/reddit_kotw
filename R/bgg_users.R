library(stringr)
library(xml2)
library(dplyr)
library(tidyr)

all_together <- readRDS("data/posts.rds")

get_user_data <- function(username){
  print(paste0("Gathering Data for: ", username))
  
  if (str_detect(username, " ")) {
    username <- str_replace_all(username, " ", "%20")
  }
  url <- paste0("https://boardgamegeek.com/xmlapi2/user?name=", username)
  
  base <- url %>%
    read_xml() %>%
    xml_attrs() %>%
    tibble(key = names(.),
           value = .)
  
  meta <- tibble(
    key = url %>%
      read_xml() %>%
      xml_children() %>%
      xml_name(),
    value = url %>%
      read_xml() %>%
      xml_children() %>%
      xml_attrs("value") %>% unlist() 
  ) 
  data <- bind_rows(base, meta) %>% 
    pivot_wider(names_from = key,
                values_from = value) %>% 
    mutate_all(as.character)
  return(data)
}

users <- unique(all_together$username)[!is.na(unique(all_together$username))]

update_data <- function(user_data) {
  readRDS("data/user_info.rds") %>% 
    bind_rows(user_data) %>% 
    distinct() %>% 
    saveRDS("data/user_info.rds")
}


get_all_users <- function(users) {
  
  user_data <- NULL
  
  know_users <- readRDS("data/user_info.rds") %>% 
    distinct(name) %>% 
    pull()
  
  users <- users[!users %in% know_users]
  
  if (length(users) > 5) {
    sleeper <- seq(1, length(users), 5)[-1]    
  } else {
    sleeper <- 6
  }
  
  for (i in seq_along(users)) {
    
    if (!i %in% sleeper) {
      print(paste0(i, "/", length(users)))
      user_data[[i]] <- get_user_data(users[i])
    }
    if (i %in% sleeper) {
      print("Sleeping 5 ...")
      print("Updating file ...")
      update_data(user_data)
      Sys.sleep(5)
      
      
      
      print(paste0(i, "/", length(users)))
      user_data[[i]] <- get_user_data(users[i])
    }
    
  }
  
  return(bind_rows(user_data))
}

boolFalse <- FALSE
while(boolFalse == FALSE) {
  tryCatch({
    user_data <- get_all_users(users)  
    boolFalse <- TRUE
  }, error = function (e) {
  }, finally = {})
}
