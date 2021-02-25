library(dplyr)
library(tidyr)
library(purrr)
library(rvest)
library(stringr)
library(lubridate)

con <- file("data/runner.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

print("start script")
dominion_ids <- tribble(
  ~name, ~game_id,
  "dominion-intrigue", 40834,
  "dominion-update-pack", 209357,
  "dominion-intrigue-second-edition", 209419,
  "dominion-die-intrige-erweiterung", 160673, 
  "dominion-envoy-promo-card", 39707, 
  "dominion-seaside", 51811, 
  "dominion-black-market-promo-card", 41105, 
  "dominion-stash-promo-card", 66682, 
  "dominion-prosperity", 66690, 
  "dominion-alchemy", 66098,
  "dominion-walled-village-promo-card", 101665, 
  "dominion-hinterlands", 104557, 
  "dominion-governor-promo-card", 110831, 
  "dominion-cornucopia", 90850,
  "dominion-dark-ages", 125403,
  "dominion-alchemisten-reiche-ernte-mixbox", 172592, 
  "dominion-guilds", 137166, 
  "dominion-prince-promo-card", 160943,
  "dominion-summon-promo-card", 182822, 
  "dominion-guilds-cornucopia", 177903, 
  "dominion-adventures", 171915,
  "dominion-sauna-avanto-promo-card", 209431, 
  "dominion-intrigue-update-pack", 209358,
  "dominion-hoflinge-promo-card", 234892,
  "dominion-empires", 192951,
  "dominion-nocturne", 232452,
  "dominion-erweiterung-basisspiel-die-intrige", 228386,
  "dominion-dismantle-promo-card", 238623,
  "dominion-renaissance", 257673,
  "dominion-church-promo-card", 285888,
  "dominion-captain-promo-card", 285887,
  "dominion-menagerie", 298055,
  "dominion", 36218
)

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

domionion_bgg <- dominion_ids %>% 
  mutate(url_xml = glue::glue("https://www.boardgamegeek.com/xmlapi/boardgame/{game_id}?stats=1&comments=1"),
         data = map(url_xml, parse_bgg))

read_xml("https://boardgamegeek.com/xmlapi2/thing?type=boardgame&id=36218&stats=1&comments=1&ratingcomments=1&pagesize=100")

### Get forums ids per game
parse_game_forums <- function(game_id) {
  print(paste0("Parseing forum for game id: ", game_id))
  
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

dominion_forums <- dominion_ids %>% 
  mutate(forums = map(game_id, parse_game_forums)) %>% 
  unnest_wider(forums) %>% 
  unnest(names(.)) %>% 
  arrange(game_id)

# forum parsing

parse_thread <- function(thread_id) {
  
  #thread_id <- 582679
  
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
  
  data <- tibble(
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
    select(-xml_data) %>% 
    rename(post_id = id) %>% 
    group_by(post_id, key) %>% 
    mutate(value2 = paste(value, collapse = " ")) %>% 
    select(-value) %>% 
    distinct() %>% 
    pivot_wider(
      names_from = "key",
      values_from = "value2"
    ) %>% 
    mutate_at(vars(contains("date")), ymd_hms)
  
  #Sys.sleep(0.5)
  
  return(data)
  
}
#parse_thread(582679)

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
  
  return(bind_rows(data))
  
}

iterative_parse <- function(forum_id) {
  
  i <- 1
  data_full <- NULL
  data_full[[i]] <- parse_forum(forum_id) %>% 
    select(forum_id, id) %>% 
    mutate(id = as.numeric(id),
           messages = map(id, possibly(parse_thread, NA_real_)))
  
  data_clean <- NULL
  data_retour <- NULL
  data_clean[[i]] <- data_full[[i]] %>% 
    mutate(ind = as.character(messages),
           test = str_detect(ind, "^NA")) %>% 
    filter(test == FALSE) %>% 
    select(-ind, -test)
  
  data_retour[[i]] <- data_full[[i]] %>% 
    mutate(ind = as.character(messages),
           test = str_detect(ind, "^NA")) %>% 
    filter(test == TRUE)
  
  laenge_retour <- NULL
  
  while (nrow(data_retour[[i]]) != 0) {
    
    print(paste0("Retour: ", i, ", starting in 5 sec"))
    
    Sys.sleep(5)
    
    # check if empty message and break after 5 repeats
    laenge_retour[i] <- nrow(data_retour[[i]])
  
    
    i <- i + 1
    data_full[[i]] <- data_retour[[i-1]] %>% 
      mutate(messages = map(id, possibly(parse_thread, NA_real_)))
    
    data_clean[[i]] <- data_full[[i]] %>% 
      mutate(ind = as.character(messages),
             test = str_detect(ind, "^NA")) %>% 
      filter(test == FALSE) %>% 
      select(-ind, -test)
    
    data_retour[[i]] <- data_full[[i]] %>% 
      mutate(ind = as.character(messages),
             test = str_detect(ind, "^NA")) %>% 
      filter(test == TRUE) %>% 
      select(-ind, -test)
    
    if (length(laenge_retour) >= 10) {
      
      
      rev_laenge <- laenge_retour %>% rev()
      a <- sum(rev_laenge[-1] %in% rev_laenge[1])
      
      #print(paste0("Laenge: ", length(laenge_retour)))
      #print(paste0("a=", a))
      
      if (a > 5) {
        data_clean[[i+1]] <- data_retour[[i]]
        data_retour[[i]] <- tibble()
        
      }
    }
    
  }
  
  return(bind_rows(data_clean))
}


## parse everything

start_parse <- Sys.time()
dominion_bgg_all <- dominion_forums %>% 
  select(name, game_id, forum_id = id) %>% 
  mutate(forum_threads = map(forum_id, iterative_parse))
finish_parse <- Sys.time()
(parse_time <- finish_parse - start_parse)

heute <- gsub("-", "", Sys.Date())
file_name <- paste0("data/", heute, "_bgg_forums.RData")
save(dominion_bgg_all, file = filename)





parse_forum(2430039)





sessions_dom <- iterative_parse(450)

sessions_dom %>% 
  rename(thread_id = id) %>% 
  unnest_wider(messages, names_sep = "_") %>% 
  unnest(names(.)) %>% 
  select(-messages_1) 





  
#### test sample
test <- parse_forum(450) %>% 
  sample_n(10) %>% 
  select(forum_id, id) %>% 
  bind_rows(
    tibble(
      id = c("582679","582653","582215","573618","390648","568157"),
      forum_id = 450  
    )
    
  )
