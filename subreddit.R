library(tidyverse)
library(jsonlite)
library(lubridate)
theme_set(theme_light())

base_url <- "https://www.reddit.com/r/dominion/search.json?q=%22kotw%20{month}%2F%22&restrict_sr=1&limit=100"

df_url <- tibble(
  month = 1:12
) %>% 
  mutate(
    url = glue::glue(base_url)
  ) 

parse_reddit <- function(url) {
  
  print(url)
  
  kotw_base <- url %>% 
    read_json()
  
  kotw_entries <- tibble(json = kotw_base) %>%
    slice(2) %>%
    unnest(json) %>%
    slice(5) %>%
    unnest(json) 
  
  kotw_entries_wide <- NULL
  for (i in 1:nrow(kotw_entries)) {
    kotw_entries_wide[[i]] <- kotw_entries %>%
      slice(i) %>%
      unnest(json) %>%
      slice(2) %>%
      unnest_wider(json)
  }
  kotw_entries_wide <- bind_rows(kotw_entries_wide)
  
  return(kotw_entries_wide)
}

df <- df_url %>% 
  mutate(kotw = map(url, parse_reddit))


kotw_entries_wide <- df %>% 
  select(kotw) %>% 
  unnest(kotw) %>% 
  filter(grepl("^KotW", title, ignore.case = TRUE)) %>% 
  mutate(release = as.POSIXct(created, tz = "UTC", origin = "1970-01-01"),
         release_date = as_date(release)) %>% 
  select(id, title, upvote_ratio, ups, downs, score, release_date, subreddit_name_prefixed)


all_kotw <- kotw_entries_wide %>% 
  mutate(reddit_url = glue::glue("https://www.reddit.com/{subreddit_name_prefixed}/comments/{id}")) %>% 
  select(reddit_url)


get_post <- function(json_tibble){
  json_tibble %>% 
    unnest_wider(json) %>% 
    unnest_wider(data) %>% 
    slice(1) %>% 
    unnest(children) %>% 
    unnest_wider(children, names_sep = "_") %>% 
    unnest_wider(children_data) 
}

get_comments <- function(json_tibble){
  json_tibble %>% 
    unnest_wider(json) %>% 
    unnest_wider(data) %>% 
    slice(2) %>% 
    unnest(children) %>% 
    unnest_wider(children, names_sep = "_") %>% 
    unnest_wider(children_data) 
}

get_replies <- function(comment_tibble){
  comm_raw <- comment_tibble %>% 
    select(replies) %>% 
    unnest_wider(replies, names_sep = "_")
  
  if ("replies_data" %in% colnames(comm_raw)) {
    df <- comm_raw %>% 
      unnest_wider(replies_data) %>% 
      unnest(children) %>% 
      unnest_wider(children, names_sep = "_") %>% 
      unnest_wider(children_data) %>% 
      rename(kind = replies_kind)
  } else {
    message("no further replies")
    df <- NA
  }
  
  return(df)
  
}

subreddit <- function(json_url){
  
  #json_url <- "https://www.reddit.com/r/dominion/comments/1srl2e.json"
  print(json_url)
  
  df_json <- tibble(
    json = jsonlite::read_json(json_url)
  )
  
  # post information
  df_post <- df_json %>% 
    get_post()
  
  # post has comments?
  if (nrow(df_json) > 1) {
    df_post_comm <- df_post %>% 
      bind_rows(
        df_json %>% 
          get_comments()
      )
    
    # check if replies exists
    run <- 1
    df_reply <- NULL
    
    df_reply[[run]] <- df_post_comm %>% 
      get_replies()
    
    if ("replies" %in% names(df_reply[[run]])) {
      df_reply[[run]] <- df_reply[[run]] %>% 
        mutate(replies = as.list(replies))
    }
    
    while (!is.na(df_reply[run])) {
      print(paste0("Reading relies depth: ", run))
      
      if (!"replies" %in% names(df_reply[[run]])) {
        df_reply[[run]] <- df_reply[[run]] %>% 
          mutate(replies = as.list(NA))
      }
      
      run <- run + 1
      #print(run)
      
      
      
      df_reply[[run]] <- df_reply[[run-1]] %>% 
        get_replies() 
      
      if ("replies" %in% names(df_reply[[run]])) {
        df_reply[[run]] <- df_reply[[run]] %>% 
          mutate(replies = as.list(replies))
      }
      
      #print(length(df_reply))
    }
    
    #print("binding")
    df <- df_post_comm %>% 
      bind_rows(df_reply[-run]) %>%
      mutate_all(as.character)
    
  } else {
    df <- df_post %>% 
      mutate_all(as.character)
  }
  
  
  
  return(df)
}

all_kotw_reddits <- map_df(pull(all_kotw), subreddit)

clean_subreddit <- function(subreddit) {
  clean_post <- subreddit %>% 
    filter(children_kind == "t3") %>% 
    select(subreddit, subreddit_id, subreddit_subscribers, id, title, text = selftext, pwls, wls, ups, downs, score, 
           name, upvote_ratio, edited, created, created_utc, permalink, 
           total_awards_received, author_fullname, author, num_comments) %>% 
    mutate(reddit_id = id)
  
  clean_replies <- subreddit %>% 
    filter(children_kind == "t1") %>% 
    select(subreddit, subreddit_id, id, text = body, ups, downs, score, name, depth, controversiality,
           name, parent_id, link_id, edited, created, created_utc, author, author_fullname,
           permalink)
  
  df <- bind_rows(
    clean_post, 
    clean_replies
  ) %>% 
    fill(reddit_id, .direction = "down")
  
  return(df)
}

clean_reddits <- clean_subreddit(all_kotw_reddits)
df_all <- clean_reddits %>% 
  group_by(reddit_id) %>% 
  nest() %>% 
  ungroup()

date_string <- gsub("-", "", Sys.Date())
save(df, file = paste0(date_string, "_kotw.RData"))
