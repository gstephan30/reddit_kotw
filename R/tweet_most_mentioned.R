library(tidyverse)
library(rtweet)
library(showtext)

font_add_google("Schoolbell", "bell")
showtext_auto()
theme_set(theme_light(base_size = 40, base_family = "bell"))
heute_text <- format(Sys.Date(), "%b %d, %Y")
heute <- gsub("-", "", Sys.Date())
recent_file <- list.files("data/", full.names = TRUE, pattern = "kotw") %>% sort(decreasing = TRUE) %>% .[1]
load(recent_file)

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

clean_cards <- readRDS("data/clean_cards.rds")

top20 <- clean_cards %>% 
  filter(key == "card") %>% 
  count(title, sort = TRUE) %>% 
  head(20)

choosen_card <- top20 %>% 
  sample_n(1)

g1 <- top20 %>% 
  mutate(high = case_when(title == pull(choosen_card, title) ~ "#ff6600",
                          TRUE ~ "#1E2B4A"),
         title = str_to_title(title)) %>% 
  ggplot(aes(n, fct_reorder(title, n), fill = high, label = paste0("N=", n))) +
  geom_col() +
  scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
  labs(title = "Most mentioned cards in Reddits Kingdom of the Week",
       subtitle = paste0("Date: ", heute_text), 
       x = "Count",
       y = "") +
  scale_fill_identity() +
  geom_text(position = position_dodge(width = 0.9), hjust = -1, family = "bell", size = 12)

file_name <- paste0("gen_figs/", heute, "_most_mentioned.png")
ggsave(g1, filename = file_name, scale = 0.4)

example_kingdom <- clean_cards %>% 
  filter(title %in% pull(choosen_card, title)) %>% 
  sample_n(1) %>% 
  pull(reddit_id)

mentioned_df <- df_all %>% 
  filter(reddit_id %in% example_kingdom) %>% 
  unnest(data) %>% 
  mutate(
    created = as.POSIXct(as.numeric(created), origin = "1970-01-01")
  ) %>% 
  filter(grepl("t3", name)) %>% 
  separate(title, c("title", "cards"), sep = "\\:")

# mentioned tweet
mentioned_text <- "One of the Top 20 most mentioned cards in Reddits Kingdom of the week is "
mentioned_card <- str_to_title(pull(choosen_card, title))
mentioned_link <- paste0("https://www.reddit.com/r/dominion/comments/", example_kingdom)
mentioned_title <- mentioned_df$title
mentioned_date <- format(mentioned_df$created, "%b %d, %Y")
mentioned_picture <- file_name
mentioned_hash <- " #dominion #boardgame #boardgamegeek #riograndegames #boardgames #deckbuilder #dominionboardgame "
mentioned_picture2 <- here::here("images", paste0(example_kingdom, ".jpg"))


tweet_message <- paste0(
  mentioned_text, 
  mentioned_card, 
  ". Its example KOTW is ", 
  mentioned_title, 
  mentioned_hash,
  mentioned_link
)

post_tweet(
  tweet_message,
  media = mentioned_picture
)
