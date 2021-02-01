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
         release_date = as_date(release)) 

%>% 
  select(id, title, upvote_ratio, ups, downs, score, release_date)


kotw_entries_wide %>% 
  mutate(reddit_url = glue::glue("https://www.reddit.com/{subreddit_name_prefixed}/comments/{id}")) %>% 
  select(reddit_url)

reddit_content("https://www.reddit.com/r/dominion/comments/kzqppx")  %>% as_tibble()

test <- read_json("https://www.reddit.com/r/dominion/kzqppx.json")
test <- read_json("https://www.reddit.com/r/dominion/l93191.json")


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
    
    while (!is.na(df_reply[run])) {
      print(paste0("Reading relies depth: ", run))
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
      bind_rows(df_reply[-run])
    
  } else {
    df <- df_post
  }
  
  
  
  return(df)
}

subreddit("https://www.reddit.com/r/dominion/comments/l93191.json")





