library(stringr)
library(dplyr)
library(rtweet)
library(lubridate)
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

library(tidytext)
library(rvest)
library(SnowballC)
typos <- read_html("https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines") %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/pre") %>% 
  html_text(trim = TRUE) %>% 
  as_tibble() %>% 
  separate_rows(value, sep = "[\r\n]") %>% 
  separate(value, c("wrong", "correct"), sep = "\\-\\>")

stemmed_tokens <- df_clean %>% 
  filter(grepl("t1\\_", name),
         author != "DominionCardBot") %>% 
  select(reddit_id, text, score, created) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word) %>% 
  left_join(
    typos,
    by = c("word" = "wrong")
  ) %>% 
  mutate(word = ifelse(!is.na(correct), correct, word)) %>% 
  select(-correct) %>% 
  mutate(word_stem = wordStem(word))

library(widyr)
library(ggplot2)
ind_vertices %>% 
  distinct(words) %>% 
  filter(grepl("market", words)) 

pcor <- stemmed_tokens %>%
  add_count(word_stem) %>% 
  filter(n > 100) %>% 
  filter(word_stem != "hmm",
         !grepl("[0-9]", word_stem)) %>% 
  pairwise_cor(word_stem, reddit_id, upper = FALSE) %>% 
  filter(correlation >= 0.25 |
           correlation <= -0.25)

ind_vertices <- tibble(
  words = c(pcor$item1, pcor$item2)
) %>% 
  distinct()

vertices <- stemmed_tokens %>% 
  count(word_stem) %>% 
  filter(word_stem %in% ind_vertices$words) %>% 
  as.data.frame()


library(ggraph)
library(igraph)
library(ggnewscale)
set.seed(2021)
pcor %>% 
  graph_from_data_frame(vertices = vertices) %>% 
  ggraph("fr") +
  
  geom_edge_link(aes(color = correlation), edge_width = 1.2) +
  scale_color_gradient2(low = "blue", high = "red", midpoint = 0.6) +
  new_scale_color() +
 
  geom_node_point(size = 3.5, color = "black") +
  geom_node_point(aes(color = n), size = 3) +
  scale_color_viridis_c() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() 
  
  
