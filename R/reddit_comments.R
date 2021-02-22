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
library(showtext)
font_add_google("Schoolbell", "bell")
showtext_auto()
#library(ggnewscale)
set.seed(2021)
g_cor <- pcor %>% 
  graph_from_data_frame(vertices = vertices) %>% 
  ggraph("fr") +
  
  geom_edge_link(aes(color = correlation), edge_width = 1.2) +
  scale_edge_color_gradient2(low = "#72bcd4", high = "red", midpoint = 0.6) +
  #new_scale_color() +
 
  geom_node_point(size = 3.5, color = "black") +
  geom_node_point(aes(color = n), size = 3) +
  scale_color_viridis_c() +
  geom_node_text(aes(label = name), repel = TRUE, size = 12, family = "bell") +
  theme_void(base_family = "bell", base_size = 24) +
  labs(title = 'Correlation of words appeared in reddits Kingdom of the week ("KOTW") posts',
       subtitle = paste0("Words are corrected and stemmed, date: ", format(heute, "%b %d, %Y"), "\n",
                         "Words filterd with p (rho) >0.25"),
       color = "Count word stem appeared",
       edge_color = "Word pair correlation"
       ) +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(lineheight = 0.5)) +
  guides(color = guide_colorbar(title.position = "top"),
         edge_color = guide_edge_colorbar(title.position = "top"))

ggsave(g_cor, filename = "gen_figs/comment_cors.jpg")

tm_link <- "https://new.reddit.com/search/?q=author%3Aavocadro%20title%3AKotW&sort=new"
tm_hashs <- " #dominion #boardgame #boardgamegeek #riograndegames #boardgames #deckbuilder #dominionboardgame "
tm_message <- paste0("Text-mining analysis of Dominion cards from reddits Kingdom of the week comments ",
                     tm_link,
                     tm_hashs,
                     "#rstats")
post_tweet(
  status = tm_message,
  media = "gen_figs/comment_cors.jpg"
)  
  
