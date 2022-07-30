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


#' for rpi needed to swap from read_json to fromJSON
#' due to lexica errors
parse_reddit_from <- function(url) {
  # url <- "https://www.reddit.com/r/dominion/search.json?q=kotw%201%2F&restrict_sr=1&limit=100"
  print(url)
  kotw_base <- fromJSON(url)
  kotw_entries <- tibble(
    key = names(kotw_base),
    json = kotw_base) %>%
    slice(2) %>%
    unnest(json) %>%
    mutate(key = names(json)) %>% 
    filter(key == "children") %>% 
    unnest_wider(json) %>%
    unnest(cols = c(kind, data)) %>% 
    select(-1)
  
  return(kotw_entries)
}


download_images <- function(kotw_df) {
  
  url <- NULL
  id <- NULL
  for (i in 1:nrow(kotw_df)) {
    
    url <- kotw_df$image_url[i]
    id <- kotw_df$id[i]
    
    if (!is.na(url)) {
      check_pic(id, url)
    } else {
      print(paste0(id, " has no image"))
    }
    
  }
  
}




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
