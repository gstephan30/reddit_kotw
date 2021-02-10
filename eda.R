library(tidytext)

text_raw <- df_all %>% 
  unnest(data) %>% 
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
  