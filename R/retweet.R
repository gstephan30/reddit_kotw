library(rtweet)
library(dplyr)

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
                      access_secret = access_token_secret,
		      set_renv = FALSE)

hashtags <- c("#dominionboardgame", "#dominiongame", "#dominioncardgame", "dominion AND boardgame")
dominion_hashtags <- search_tweets2(q = hashtags) 

users <- c("boardgamegeek", "riograndegames")
bbg_tweets <- get_timeline(users, n = 20)
dominion_user_tweets <- bbg_tweets %>% 
  filter(grepl("dominion", text, ignore.case = TRUE)) 

tweets <-
  bind_rows(
    dominion_hashtags,
    dominion_user_tweets
  ) %>% 
  select(-query) %>% 
  distinct()

old_tweets <- readRDS("data/checked_tweets.rds") 

new_tweets <- tweets %>% 
  anti_join(
    old_tweets
  ) %>% filter(screen_name != "dominionkingd")

checked_tweets <- bind_rows(
  old_tweets,
  new_tweets
) %>% 
  distinct()
saveRDS(checked_tweets, "data/checked_tweets.rds")

post_tweets <- pull(new_tweets, status_id)

if (length(post_tweets) != 0) {
  for (i in seq_along(post_tweets)) {
    post_tweet(retweet_id = post_tweets[i])
  }
}
