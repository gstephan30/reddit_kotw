library(rtweet)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

source("R/setup_keys.R")
api_key <- setup_keys("api_key")
api_key_secret <- setup_keys("api_key_secret")
access_token <- setup_keys("access_token")
access_token_secret <- setup_keys("access_token_secret")
app_name <- setup_keys("app_name")

heute <- today()

token <- create_token(app = app_name,
                      consumer_key = api_key,
                      consumer_secret = api_key_secret,
                      access_token = access_token,
                      access_secret = access_token_secret)

recent_file <- list.files("data/", full.names = TRUE, pattern = "kotw") %>% sort(decreasing = TRUE) %>% .[1]
load(recent_file)

picture_list <- list.files("images/") %>% 
  as_tibble() %>% 
  mutate(value = str_remove_all(value, "\\.jpg"))

df_clean <- df_all %>% 
  unnest(data) %>% 
  mutate(score = as.numeric(score)) %>% 
  mutate(
    created = as.POSIXct(as.numeric(created), origin = "1970-01-01"),
    created_utc = as.POSIXct(as.numeric(created_utc), origin = "1970-01-01"),
    ups = as.numeric(ups),
    downs = as.numeric(downs), 
    score = as.numeric(score),
    upvote_ratio = as.numeric(upvote_ratio)
  ) %>% 
  arrange(reddit_id, created, depth) %>% 
  fill(subreddit_subscribers, .direction = "down") %>% 
  fill(title, .direction = "down") 

# user_karma
karma <- df_clean %>% 
  group_by(reddit_id) %>% 
  summarise(comments = n()-1,
            sum_ups = sum(ups),
            sum_downs = sum(downs), 
            created = min(created),
            users = n_distinct(author)) %>% 
  mutate(karma = comments + sum_ups + sum_downs) %>% 
  as_tibble() %>% 
  arrange(desc(karma)) %>% 
  mutate(user_karma = karma/users) %>% 
  arrange(desc(user_karma)) %>% 
  left_join(
    picture_list %>% 
      mutate(pic = 1) %>% 
      rename(reddit_id = value)
  ) %>% 
  filter(!is.na(pic)) %>% 
  top_n(10, karma) 


## check if recently posted
old_data <- readRDS("data/posted_ids.rds")

rnd_id <- sample_n(karma, 1) %>% 
  pull(reddit_id)

while (rnd_id %in% (pull(old_data, id)) == TRUE) {
  print(rnd_id)
  rnd_id <- sample_n(karma, 1) %>% 
    pull(reddit_id)
}

karma_ids <- old_data %>% 
  bind_rows(
    tibble(
      key = "karma_user",
      date = heute,
      id = rnd_id  
    )
  )

saveRDS(karma_ids, file = "data/posted_ids.rds")