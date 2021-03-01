library(httr)
library(dplyr)
library(stringr)
library(magick)
library(tibble)
req <- GET("https://api.github.com/repos/blakevanlan/KingdomCreator/git/trees/master?recursive=1")
stop_for_status(req)
card_files <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F) %>% 
  grep("jpg", ., value = TRUE)


files <- paste0("https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/", card_files)


clean_cards <- readRDS("data/clean_cards.rds")
picture_list <- list.files("images/") %>% 
  as_tibble() %>% 
  mutate(value = str_remove_all(value, "\\.jpg")) %>% 
  pull(value)

need_pci_df <- clean_cards %>% 
  filter(!reddit_id %in% picture_list,
         key == "card") %>% 
  group_by(reddit_id) %>% 
  arrange(card_type) %>% 
  group_split()

common_cards <- grep("common", card_files, value = TRUE) %>% 
  basename() %>% 
  str_remove_all(., ".jpg")
common_cards <- c(grep("copper", common_cards, value = TRUE),
                  grep("silver", common_cards, value = TRUE),
                  grep("gold", common_cards, value = TRUE),
                  grep("curse", common_cards, value = TRUE),
                  grep("estate", common_cards, value = TRUE),
                  grep("duchy", common_cards, value = TRUE),
                  grep("province", common_cards, value = TRUE)
                  )

shelter_cards <- c(grep("hovel", card_files, value = TRUE),
                   grep("necropolis", card_files, value = TRUE),
                   grep("overgrownestate", card_files, value = TRUE)
                   ) %>% 
  grep("fr\\/", ., invert = TRUE, value = TRUE) %>% 
  sort(., decreasing = TRUE) %>% 
  basename() %>% 
  str_remove_all(., ".jpg") 

colony_platin <- c(grep("colony", card_files, value = TRUE),
                   grep("platinum", card_files, value = TRUE)
                   ) %>% 
  grep("fr\\/", ., invert = TRUE, value = TRUE) %>% 
  sort(., decreasing = TRUE) %>% 
  basename() %>% 
  str_remove_all(., ".jpg") 


create_picture <- function(data) {
  need_cards <- data %>% 
    pull(title) %>% 
    str_to_lower(.) %>% 
    str_trim(.) %>% 
    str_remove_all(., "[[:punct:]]") %>% 
    str_remove_all(., " ")
  need_cards <- c(need_cards, common_cards)
  need_bane <- unique(data$bane)
  if (!is.na(need_bane)) {
    bane_cards <- need_bane %>%
      str_to_lower(.) %>% 
      str_trim(.) %>% 
      str_remove_all(., "[[:punct:]]") %>% 
      str_remove_all(., " ")
    
    if (bane_cards == "sage") {
      bane_cards <- "_sage"
    }
    
    need_cards <- c(need_cards, bane_cards)
  }
  need_shelters <- unique(data$shelters)
  need_shelters <- ifelse(is.na(need_shelters), FALSE, TRUE)
  if (need_shelters == TRUE) {
    need_cards <- c(need_cards, shelter_cards)
  }
  
  need_platin <- unique(data$colony_platinum)
  need_platin <- ifelse(is.na(need_platin), FALSE, TRUE)
  if (need_platin == TRUE) {
    need_cards <- c(need_cards, colony_platin)
  }
  
  deck_files <- NULL
  images <- NULL
  for (i in seq_along(need_cards)) {
    deck_files[i] <- grep(need_cards[i], card_files, value = TRUE) %>% 
      grep("fr\\/", ., invert = TRUE, value = TRUE) %>% 
      sort(., decreasing = TRUE) %>% 
      .[1]
    
    dest_file <- paste0("card_images/", need_cards[i], ".jpg")
    
    if (!file.exists(dest_file)){
      print(paste0("Downloading: ", basename(dest_file)))
      
      download.file(
        url =  paste0("https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/", deck_files[i]),
        destfile = dest_file,
        mode = "wb"
      )
    }
    
    images[[i]] <- image_read(paste0("card_images/", need_cards[i], ".jpg"))
  }
  
  length_images <- length(images)
  rows <- ceiling(length_images/5) 
  cuts_at <- 5
  max_items <- 100
  needed_rows <- tibble(
    cut_low = seq(1, max_items, cuts_at),
    max = 100
  ) %>% 
    rownames_to_column("id") %>% 
    rowwise() %>% 
    mutate(row_needed = between(length_images, cut_low, max)) %>% 
    filter(row_needed == TRUE)
  
  image_rows <- NULL
  for (i in 1:nrow(needed_rows)) {
    #i <- 1
    print(i)
    needed <- needed_rows$row_needed[i]
    print(needed)
    
    range_low <- needed_rows$cut_low[i]
    range_up <- range_low + cuts_at-1
    
    image_rows[[i]] <- images[range_low:range_up] %>% 
      image_join() %>% 
      image_append()
    
    needed <- FALSE
    
  }
  pic <- image_rows %>% 
    image_join() %>% 
    image_append(stack = TRUE)
  
  return(pic)
}


create_picture(need_pci_df[[189]])
pic

