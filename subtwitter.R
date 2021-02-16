library(dplyr)
library(tidyr)
library(rtweet)

source("R/setup_keys.R")
api_key <- setup_keys("api_key")
api_key_secret <- setup_keys("api_key_secret")
access_token <- setup_keys("access_token")
access_token_secret <- setup_keys("access_token_secret")
app_name <- setup_keys("app_name")

token <- create_token(app = app_name,
                      consumer_key = api_key,
                      consumer_secret = api_key_secret,
                      access_token = access_token,
                      access_secret = access_token_secret)

heute <- Sys.Date()
load("data/twitter.RData")

gen_twitter_data <- function(datakey) {
  form <- paste0("twitter_", datakey)
  print(paste0("Collecting", form))
  
  if (datakey == "user") {
    dfpre <- lookup_users("dominionkingd")
  }
  if (datakey == "friends") {
    dfk <- get_friends("dominionkingd") %>% 
      mutate(form = form,
             export = heute) %>% 
      select(form, export, everything())
    
    df_checked <- dfk %>% 
      bind_rows(df[[datakey]]) %>% 
      distinct(user, user_id, .keep_all = TRUE)
    
  }
  if (datakey == "followers") {
    dfk <- get_followers("dominionkingd") %>% 
      mutate(form = form,
             export = heute) %>% 
      select(form, export, everything()) 
    
    df_checked <- dfk %>% 
      bind_rows(df[[datakey]]) %>% 
      distinct(user_id, .keep_all = TRUE)
  }
  if (datakey == "timeline") {
    dfpre <- get_timeline("dominionkingd")
  }
  
  if (exists("dfpre")) {
    dfk <- dfpre %>%   
      mutate_all(as.character) %>% 
      pivot_longer(
        cols = -c("user_id", "status_id"),
        names_to = "key",
        values_to = "values"
      ) %>% 
      mutate(
        form = form,
        export = heute) %>% 
      select(form, export, user_id, status_id, key, values) 
    
    df_checked <- dfk %>% 
      bind_rows(df[[datakey]]) %>% 
      distinct(status_id, key, values, .keep_all = TRUE)
  }
  
  return(df_checked)
}
forms <- c("user", "friends", "followers", "timeline")
df <- lapply(forms, gen_twitter_data)
names(df) <- forms

save(df, file = "data/twitter.RData")