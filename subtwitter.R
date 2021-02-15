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

df_twitter <- get_timeline("dominionkingd")
heute <- Sys.Date()

df <- df_twitter %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -c("user_id", "status_id"),
    names_to = "key",
    values_to = "values"
  ) %>% 
  mutate(
    form = "twitter_timeline",
    export = heute) %>% 
  select(form, export, user_id, status_id, key, values)

df_long <- readRDS("data/twitter_timeline.rds") %>% 
  bind_rows(df) %>% 
  distinct(status_id, key, values, .keep_all = TRUE)

saveRDS(
  df_long,
  file = "data/twitter_timeline.rds"
)
