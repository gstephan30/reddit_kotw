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
  pre_comm <- json_tibble %>% 
    unnest_wider(json) %>% 
    unnest_wider(data) %>% 
    slice(2) %>% 
    unnest(children) %>% 
    unnest_wider(children, names_sep = "_") 
  
  if (nrow(pre_comm) > 0) {
    comm_data <- pre_comm |> 
      unnest_wider(children_data)   
  } else {
    comm_data <- pre_comm
  }
  
  return(comm_data)
  
}

get_replies <- function(comment_tibble){
  
  if ("replies" %in% colnames(comment_tibble)) {
    comm_raw <- comment_tibble %>% 
      select(replies) %>% 
      unnest_wider(replies, names_sep = "_")
  } else {
    comm_raw <- NA
  }
  
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
  
  #json_url <- "https://www.reddit.com/r/dominion/comments/4703zy.json"
  #json_url <- "https://www.reddit.com/r/dominion/comments/4703zy/kotw_221_conspirator_courtyard_crossroads_great/d0f7luu.json"
  #json_url <- "https://www.reddit.com/r/dominion/comments/4ac0xg"
  #json_url <- "https://www.reddit.com/r/dominion/comments/3wqx3o"
  #json_url <- "https://www.reddit.com/r/dominion/comments/169lrqa"
  
  print(json_url)
  
  if (str_detect(json_url, "\\.json$")) {
    json_url <- json_url
  } else {
    json_url <- paste0(json_url, ".json")
  }
  
  
  
  #df_json <- tibble(
  #  json = jsonlite::read_json(json_url)
  #)
  
  df_json <- tibble(
    json = jsonlite::fromJSON(json_url, simplifyVector = FALSE)
  )
  
  # post information
  df_post <- df_json %>% 
    get_post()
  
  
  post_url <- df_post$permalink
  
  # post has comments?
  if (nrow(df_json) > 1) {
    df_post_comm <- df_post %>% 
      bind_rows(
        df_json %>% 
          get_comments()
      )
    
    # check if replies exists
    run <- 1
    #run <- 9
    ind <- 0
    depth_run <- run + ind
    df_reply <- NULL
    
    df_reply[[depth_run]] <- df_post_comm %>% 
      get_replies()
    
    if ("replies" %in% names(df_reply[[depth_run]])) {
      df_reply[[depth_run]] <- df_reply[[depth_run]] %>% 
        mutate(replies = as.list(replies))
    }
    
    while (!is.na(df_reply[depth_run])) {
      #while (length(df_reply) == depth_run) {
      print(paste0("Reading relies depth: ", depth_run))
      #print(paste0("Reading relies depth: run ", run))
      
      
      if (!"replies" %in% names(df_reply[[depth_run]])) {
        df_reply[[depth_run]] <- df_reply[[depth_run]] %>% 
          mutate(replies = as.list(NA))
      }
      
      if (run == 10) {
        print(df_reply[[depth_run-1]]$id)
        
        depth_url <- paste0("https://www.reddit.com", post_url, df_reply[[depth_run-1]]$id, ".json")
        print(depth_url)
        
        depp_rep <- NULL
        for (i in seq_along(depth_url)) {
          #i <- 1
          #depp_rep[[i]] <- tibble(
          #  json = read_json(depth_url[i])
          #) %>% 
          #  get_comments() %>% 
          #  get_replies()
          
          depp_rep[[i]] <- tibble(
            json = fromJSON(depth_url[i], simplifyVector = FALSE)
          ) %>% 
            get_comments() %>% 
            get_replies()
          
          if ("replies" %in% names(depp_rep[[i]])) {
            depp_rep[[i]] <- depp_rep[[i]] %>% 
              mutate(replies = as.list(replies))
          }
          
          if (is.na(depp_rep[i])) {
            depp_rep[[i]] <- tibble()
          }
          
        }
        df_reply[[depth_run]] <- bind_rows(depp_rep)
        
        
        if (!is.na(df_reply[depth_run])) {
          df_reply[[depth_run]] <- df_reply[[depth_run]] %>% 
            mutate(depth = depth_run)
        }
        
        run <- 1
        
        ind <- ind + 10 - 1
        
        depth_run <- run + ind
        
      } else {
        
        depth_run <- depth_run + 1
        run <- run + 1
        #print(run)
        
        df_reply[[depth_run]] <- df_reply[[depth_run-1]] %>% 
          get_replies() 
        
        if (!is.na(df_reply[depth_run])) {
          df_reply[[depth_run]] <- df_reply[[depth_run]] %>% 
            mutate(depth = depth_run)
        }
        
        if ("replies" %in% names(df_reply[[depth_run]])) {
          df_reply[[depth_run]] <- df_reply[[depth_run]] %>% 
            mutate(replies = as.list(replies))
        }
      }
      
      #print(length(df_reply))
    }
    
    if (length(df_reply[-depth_run]) == 0) {
      df <- df_post_comm %>% 
        mutate_all(as.character)
    } else {
      df <- df_post_comm %>% 
        bind_rows(df_reply[-depth_run]) %>%
        mutate_all(as.character)
    }
    
  } else {
    df <- df_post %>% 
      mutate_all(as.character)
  }
  
  df <- df %>% 
    mutate(reddit_id = str_sub(json_url, -11, -6))
  
  return(df)
}



clean_subreddit <- function(subreddit) {
  clean_post <- subreddit %>% 
    filter(children_kind == "t3") %>% 
    select(reddit_id, subreddit, subreddit_id, subreddit_subscribers, id, title, text = selftext, pwls, wls, ups, downs, score, 
           name, upvote_ratio, edited, created, created_utc, permalink, 
           total_awards_received, author_fullname, author, num_comments)
  
  clean_replies <- subreddit %>% 
    filter(children_kind == "t1") %>% 
    select(reddit_id, subreddit, subreddit_id, id, text = body, ups, downs, score, name, depth, controversiality,
           name, parent_id, link_id, edited, created, created_utc, author, author_fullname,
           permalink)
  
  df <- bind_rows(
    clean_post, 
    clean_replies
  ) 
  
  return(df)
}