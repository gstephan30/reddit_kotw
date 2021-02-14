#library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(glue)
library(jsonlite)
library(lubridate)

source("R/parse_reddit.R")
source("R/parse_reddit_post.R")

base_url <- "https://www.reddit.com/r/dominion/search.json?q=%22kotw%20{month}%2F%22&restrict_sr=1&limit=100"

df_url <- tibble(
  month = 1:12
) %>% 
  mutate(
    url = glue::glue(base_url)
  ) 


df <- df_url %>% 
  mutate(kotw = map(url, parse_reddit_from))

url_pattern <- "https\\:\\/\\/*imgur\\.com\\/a\\/[a-zA-Z0-9_.-]*"
kotw_entries_wide <- df %>% 
  select(kotw) %>% 
  unnest(kotw) %>% 
  filter(grepl("^KotW", title, ignore.case = TRUE)) %>% 
  mutate(release = as.POSIXct(created, tz = "UTC", origin = "1970-01-01"),
         release_date = as_date(release)) %>% 
  select(id, title, upvote_ratio, ups, downs, score, release_date, subreddit_name_prefixed, selftext) %>% 
  separate(selftext, c("text", "image_url"), sep = "\\[Image link to Kingdom\\.\\]") %>% 
  mutate(image_url = str_extract_all(image_url, url_pattern)) %>% 
  unnest(image_url, keep_empty = TRUE) 


download_images(kotw_entries_wide)


all_kotw <- kotw_entries_wide %>% 
  mutate(reddit_url_json = glue("https://www.reddit.com/{subreddit_name_prefixed}/comments/{id}")) %>% 
  select(reddit_url_json)

all_kotw_reddits <- map_df(pull(all_kotw), subreddit)

clean_reddits <- clean_subreddit(all_kotw_reddits)
df_all <- clean_reddits %>% 
  group_by(reddit_id) %>% 
  nest() %>% 
  ungroup()

date_string <- gsub("-", "", Sys.Date())
save(df_all, file = paste0("data/", date_string, "_kotw.RData"))
