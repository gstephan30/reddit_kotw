source("R/bgg_util_parse.R")

# get dominion game ids
game_ids <- get_game_info(36218) %>% 
  filter(grepl("Dominion\\:|Dominion \\(Second", value)) %>% 
  bind_rows(
    tibble(
      type = "basegame",
      id = "36218",
      value = "Dominion"
    )
  ) 

# get recent forums
recent_overview <- readRDS("data/overview_forums.rds")

# get new forums
dominion_forums <- game_ids %>% 
  rename(game_id = id) %>% 
  mutate(forums = map(game_id, parse_game_forums)) %>% 
  unnest_wider(forums) %>% 
  unnest(names(.)) %>% 
  arrange(game_id) %>% 
  select(id, value, title, contains("num"), lastpostdate) %>% 
  mutate(
    lastpostdate = dmy_hms(lastpostdate),
    numthreads = as.numeric(numthreads),
    numposts = as.numeric(numposts)) %>% 
  filter(!is.na(lastpostdate)) %>% 
  arrange(desc(lastpostdate))

# receive forums with new entry
new_forum_posts <- dominion_forums %>% 
  anti_join(recent_overview,
            by = c("id", "value", "title", "numthreads", "numposts", "lastpostdate"))


if (length(new_forum_posts$id) > 0) {
  # overview of all forums and its post
  all_forums_clean <- new_forum_posts$id %>% 
    parse_all_forums() %>% 
    arrange(desc(lastpostdate)) %>% 
    filter(lastpostdate > (today() - days(2)))
  
  new_posts <- all_forums_clean$id %>% 
    parse_all_posts()
  
  
  all <- new_posts %>% 
    left_join(all_forums_clean %>% 
                select(forum_id, thread_id = id)) %>% 
    left_join(dominion_forums %>% 
                select(forum_id = id, expansion = value, topic = title) %>% 
                mutate(forum_id = as.numeric(forum_id))) 
  
  all_together <- readRDS("data/posts.rds") %>%
    mutate(tweeted = TRUE) %>% 
    bind_rows(all) %>% 
    distinct(thread_id, post_id, forum_id, .keep_all = TRUE)
  
  
  saveRDS(all_together, file = paste0("data/posts.rds"))
  saveRDS(dominion_forums, file = paste0("data/overview_forums.rds"))
  
} else {
  print("No new posts.")
  saveRDS(dominion_forums, file = paste0("data/overview_forums.rds"))
}


