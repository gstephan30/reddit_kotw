library(dplyr)
library(tibble)
library(xml2)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)

test_is_number <- function(number){
  if (!is.numeric(number)) {
    stop("Please provide Game ID as number.", call. = FALSE)
  }
}

#' @import tibble
test_is_tibble <- function(tibble){
  if (!is_tibble(tibble)) {
    stop("Something went wrong in data frame creation. Data probably empty.", call. = FALSE)
  }
}

#' Helper function to create a data frame from aattributes
#'
#' @param entry the base xml file
#'
#' @import dplyr
#'
#' @return a tibble with xml attributes
get_xml_data <- function(entry){
  attributes(entry) %>%
    as_tibble()
}

#' Parse Game info from Game ID
#'
#' @param game_id numeric of game_id
#'
#' @import dplyr xml2 purrr
#'
#' @return tibble with all attributes
#'
#' @examples
#'
#' \dontrun{
#' get_game_info(36218)
#' }
#' @export
get_game_info <- function(game_id){
  
  test_is_number(game_id)
  
  game_url <- paste0("https://boardgamegeek.com/xmlapi2/thing?id=", game_id, "&type=boardgame&page=1")
  
  base_meta <- read_xml(game_url) %>%
    xml_find_all("//link") %>%
    as_list()
  
  expansion_ids <- map_df(base_meta, get_xml_data)
  
  test_is_tibble(expansion_ids)
  
  return(expansion_ids)
  
}

parse_bgg <- function(url_xml) {
  
  print(paste0("Parsing: ", url_xml))
  
  xml_list <- read_xml(url_xml) %>%  
    xml_find_all("//boardgame") %>% 
    as_list()
  
  data <- tibble(
    xml_data = xml_list
  ) %>% 
    unnest(xml_data) %>% 
    mutate(key = names(xml_data),
           ind_len = lengths(key)) %>% 
    unnest(xml_data) %>% 
    mutate(value = as.character(xml_data))
  
  return(data)
}

parse_game_forums <- function(game_id) {
  print(paste0("Parsing forum for game id: ", game_id))
  
  forums <- read_xml(
    paste0(
      "https://boardgamegeek.com/xmlapi2/forumlist?type=thing&id=",
      game_id
    )
  ) %>% 
    xml_find_all("//forum") %>% 
    xml_attrs()
  
  df <- tibble(
    key = forums
  ) %>% 
    unnest_wider(key)
  
  return(df)
}

parse_thread <- function(thread_id) {
  
  #thread_id <- 2146527
  
  print(paste0("Reading Thread: ", thread_id))
  
  thread <- read_xml(
    paste0(
      "https://www.boardgamegeek.com/xmlapi2/thread?id=",
      thread_id
    )
  )
  
  a <- thread %>% 
    xml_find_all("//article") %>% 
    as_list()
  
  b <- thread %>% 
    xml_find_all("//article") %>% 
    xml_attrs()
  
  data_pre <- tibble(
    thread_id = thread_id,
    key = b,
    xml_data = a
  ) %>% unnest_wider(key) %>% 
    unnest(xml_data) %>% 
    mutate(key = names(xml_data),
           #ind_len = lengths(key)sor
    ) %>% 
    unnest(xml_data) %>% 
    mutate(value = as.character(xml_data),
           value = str_replace_all(value, "[\r\n]" , ""), 
           value = str_replace_all(value, "\\<br\\/\\>" , " "),
           value = str_replace_all(value, "\\<i\\>" , ""),
           value = str_replace_all(value, "\\<\\/i\\>" , "")) %>% 
    select(-xml_data)
  
  if (nrow(data_pre) > 0) {
    data <- data_pre %>% 
      rename(post_id = id) %>% 
      group_by(post_id, key) %>% 
      mutate(value2 = paste(value, collapse = " ")) %>% 
      select(-value) %>% 
      distinct() %>% 
      pivot_wider(
        names_from = "key",
        values_from = "value2"
      ) %>% 
      mutate_at(vars(contains("date")), ymd_hms) %>% 
      ungroup()
   
  } else {
    data <- tibble(
      thread_id = NA,
      post_id = NA,
      username = NA,
      link = NA,
      postdate = NA,
      editdate = NA,
      numedits = NA,
      subject = NA,
      body = NA
    )
  }
  
  
  #Sys.sleep(0.5)
  
  return(data)
  
}
parse_forum <- function(forum_id){
  
  page <- 1
  
  print(paste0("Parsing Forum: ", forum_id, " page ", page))
  
  forum <- read_xml(
    paste0(
      "https://www.boardgamegeek.com/xmlapi2/forum?page=",
      page,
      "&id=",
      forum_id
    )
  )
  
  posts <- forum %>% 
    xml_find_all("//thread") %>% 
    xml_attrs()
  
  data <- NULL
  data[[page]] <- tibble(
    forum_id = forum_id,
    posts = posts
  ) %>% 
    unnest_wider(posts)
  
  while (nrow(data[[page]]) != 0) {
    page <- page + 1
    
    print(paste0("Parsing Forum: ", forum_id, " page ", page))
    
    forum <- read_xml(
      paste0(
        "https://www.boardgamegeek.com/xmlapi2/forum?page=",
        page,
        "&id=",
        forum_id
      )
    )
    
    posts <- forum %>% 
      xml_find_all("//thread") %>% 
      xml_attrs()
    
    data[[page]] <- tibble(
      forum_id = forum_id,
      posts = posts
    ) %>% 
      unnest_wider(posts)
  }
  
  Sys.sleep(0.1)
  return(bind_rows(data))
  
}

parse_all_forums <- function(forum_ids) {
  
  #forum_ids <- 1659
  all_forums <- NULL
  
  sleeper <- seq(1, length(forum_ids), 50)[-1]  

  for (i in seq_along(forum_ids)) {
    
    if (!i %in% sleeper) {
      all_forums[[i]] <- parse_forum(forum_ids[i])
    }
    if (i %in% sleeper) {
      print("Sleeping 30s ...")
      Sys.sleep(30)
      all_forums[[i]] <- parse_forum(forum_ids[i])
    }
    
  }
  
  data <- all_forums %>% 
    bind_rows() %>% 
    mutate_at(vars(contains("date")), dmy_hms) %>% 
    readr::type_convert()
  
  return(data)
}

parse_all_posts <- function(thread_ids) {
  start <- Sys.time()
  all_thread <- NULL
  sleeper_thread <- seq(1, length(thread_ids), 25)[-1]
  for (i in seq_along(thread_ids)) {
    
    if (!i %in% sleeper_thread) {
      all_thread[[i]] <- parse_thread(thread_ids[i]) 
    }
    if (i %in% sleeper_thread) {
      print("Sleeping 60s ...")
      Sys.sleep(60)
      all_thread[[i]] <- parse_thread(thread_ids[i]) 
    }
  }
  end <- Sys.time()
  process_time <- end - start
  
  data <- all_thread %>% 
    map(mutate_all, as.character) %>% 
    bind_rows() %>% 
    #mutate_at(vars(contains("date")), dmy_hms) %>% 
    readr::type_convert() %>% 
    filter(!is.na(thread_id))
  
  print(paste0("Process took: ", round(process_time, 1)/60, " min. For ", 
               length(thread_ids), " Forum Threads."))
  return(data)
}