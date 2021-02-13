library(tidyverse)
library(rtweet)

source("R/setup_keys.R")
source("R/extract_title.R")
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

recent_file <- list.files("data/", full.names = TRUE) %>% sort(decreasing = TRUE) %>% .[1]
load(recent_file)

cards_clean <- df_all %>% 
  unnest(data) %>% 
  filter(grepl("^KotW", title)) %>% 
  select(reddit_id, created, title, score) %>%
  mutate(
    created = as.POSIXct(as.numeric(created), origin = "1970-01-01"),
    score = as.numeric(score)
  ) %>% 
  extract_title() %>%  
  arrange(desc(created))

cards_clean %>% 
  count(Landmark, sort = TRUE)

df_all %>% 
  unnest(data) %>% 
  filter(grepl("^KotW", title)) %>% 
  select(reddit_id, created, title, score) %>% 
  filter(grepl("Tomb", title)) %>% 
  extract_title()
