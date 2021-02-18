library(tidyverse)
library(lubridate)

clean_cards <- readRDS("data/clean_cards.rds")
recent_file <- list.files("data/", full.names = TRUE, pattern = "kotw") %>% sort(decreasing = TRUE) %>% .[1]
load(recent_file)
df <- df_all %>% unnest(data)



text_raw <- df %>% 
  select(reddit_id, text)

df_all %>% 
  unnest(data) %>% 
  mutate(score = as.numeric(score)) %>% 
  mutate_at(vars(contains("created"), ~function(x) as.POSIXct(as.numeric(x), origin = "1970-01-01")))
  arrange(desc(score)) %>% 
  select(score, permalink)


tokens <- text_raw %>% 
  filter(!grepl("Welcome to ", text)) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords::stopwords("en")) %>% 
  filter(!grepl("[0-9]", word))

tokens %>% count(word, sort = TRUE) %>% print(n=30)
df_all %>% 
  unnest(data) %>% 
  mutate(score = as.numeric(score)) %>% 
  filter(grepl("t1", name)) %>% 
  arrange(desc(score)) %>% 
  select(score, permalink)


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



df_clean %>% 
  group_by(reddit_id) %>% 
  summarise(comments = n()-1,
            sum_ups = sum(ups),
            sum_downs = sum(downs), 
            created = min(created),
            users = n_distinct(author)) %>% 
  mutate(karma = comments + sum_ups + sum_downs) %>% 
  as_tibble() %>% 
  arrange(desc(karma)) %>% 
  ggplot() +
  geom_point(aes(created, karma)) +
  geom_line(aes(created, users)) +
  geom_smooth(aes(created, users)) +
  geom_smooth(aes(created, karma)) +
  scale_y_log10()

df_clean %>% 
  count(author, sort = TRUE)

url_pattern <- "https\\:\\/\\/*imgur\\.com\\/a\\/[a-zA-Z0-9_.-]*"
test <- df_all %>% 
  unnest(data) %>% 
  filter(grepl("t3", name)) %>% 
  filter(grepl("^KotW", title, ignore.case = TRUE)) %>% 
  select(reddit_id, text) %>% 
  separate(text, c("text", "image_url"), sep = "\\[Image link to Kingdom\\.\\]") %>% 
  mutate(image_url = str_extract_all(image_url, url_pattern)) %>% 
  unnest(image_url, keep_empty = TRUE) 


check_pic <- function(reddit_id, image_url) {
  image <- paste0(reddit_id, ".jpg")
  if (!file.exists(paste0("images/", image))) {
    print(paste0("Downloading image: ", image))
    download.file(
      url = paste0(image_url, "/zip"),
      destfile = paste0("images/", image),
      mode = "wb"
    )
  } else {
    print(paste0(image, " already exists."))
  }
}

library(rvest)
test %>% 
  filter(!is.na(image_url)) %>% select(reddit_id , image_url)
  sample_n(1) %>% 
  pull(image_url) 

check_pic("l9vo00", "https://imgur.com/a/7vLXjzD")


%>% html_session() %>% rvest:::request_GET()
  read_html() %>% 
  html_nodes(xpath = "/html/body/div/div/div[1]/div/div[3]/div/div[1]/div[2]/div/div/div[2]/div/div/div/img[1]")
rvest:::request_GET()
 
page <- test %>% 
  filter(!is.na(image_url)) %>% 
  sample_n(1) %>% 
  pull(image_url)


httr::GET(page) %>% httr::content() %>% html_text()

test <- page %>% 
  html_session() 
test$handle$url
class(test)
test %>% unlist() %>% grepl("fC2dBDT", .) %>% as_tibble() %>% count(value)
curl::curl_echo(page)


rtweet::create_token()
rtweet:::create_token_()
