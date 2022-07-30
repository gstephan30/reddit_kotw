source("R/bgg_crawler.R")
library(rtweet)


print("GETTING KEYS ...")
source("R/setup_keys.R")
api_key <- setup_keys("api_key")
api_key_secret <- setup_keys("api_key_secret")
access_token <- setup_keys("access_token")
access_token_secret <- setup_keys("access_token_secret")
app_name <- setup_keys("app_name")

print("SETTING UP TOKEN ...")
token <- create_token(app = app_name,
                      consumer_key = api_key,
                      consumer_secret = api_key_secret,
                      access_token = access_token,
                      access_secret = access_token_secret,
                      set_renv = FALSE)

print("Loading Data ...")
all_together <- readRDS("data/posts.rds")


new_posts <- all_together %>% 
  arrange(desc(postdate)) %>% 
  # mutate(tweeted = c(rep(NA, 6), rep(TRUE, nrow(all_together))[-c(1:6)])) %>% 
  filter(is.na(tweeted)) %>% 
  mutate(subject = str_remove(subject, "^Re\\: ")) %>% 
  group_by(subject) %>% 
  mutate(n = n()) %>% 
  filter(postdate == max(postdate))

bgg_picture <- "images/bgg_twitter.png"

create_post_tweet <- function(expansion, subject, link, topic, n) {
  if (n == 1) {
    bgg_text <- paste0("New BGG Forum entry in ", expansion, " (", topic, ") Forum about: ", subject)  
  } else {
    bgg_text <- paste0("Multiple new BGG Forum entries in ", expansion, " (", topic, ") Forum about: ", subject)  
  }
  
  # cut down character count if needed
  if (nchar(bgg_text) > 148) {
    bgg_text <- paste0(str_sub(bgg_text, 1, 145), "...")
  }
  
  bgg_link <- link
  
  bgg_hash <- " #dominion #boardgame #boardgamegeek #riograndegames #boardgames"
  
  tweet_message <- paste0(
    bgg_text, 
    " ",
    bgg_link,
    bgg_hash
  )
}

if (nrow(new_posts) > 0) {
  tweets <- NULL
  for (i in 1:nrow(new_posts)) {
    tweets[[i]] <- create_post_tweet(new_posts[i, "expansion"] %>% pull(),
                                     new_posts[i, "subject"] %>% pull(),
                                     new_posts[i, "link"] %>% pull(),
                                     new_posts[i, "topic"] %>% pull(), 
                                     new_posts[i, "n"] %>% pull()
    )
  }
  
  ### prevent of posting more then 10 tweets
  if (length(tweets) >=10) {
    tweets <- tweets[c(1:10)]
  }
}

# save and mark that all tweeted
# prevents from tweeting posts again if none was found
saveRDS(all_together %>% 
          mutate(tweeted = TRUE), 
        file = paste0("data/posts.rds"))

if (length(tweets) > 0) {
  for (i in seq_along(tweets)) {
    print(paste0("posting tweet: ", i))
    post_tweet(
      tweets[[i]],
      media = bgg_picture
    )  
  }
}

print("FINISHED!")
