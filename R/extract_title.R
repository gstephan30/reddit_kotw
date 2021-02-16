library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(jsonlite)


recent_file <- list.files("data/", full.names = TRUE, pattern = "kotw") %>% sort(decreasing = TRUE) %>% .[1]
load(recent_file)

## json cards and sets
json_files <- list.files(path = "cards/", recursive = TRUE, pattern = "\\.json", full.names = TRUE)

json_cards <- grep("cards_db.json", json_files, value = TRUE) %>% 
  fromJSON(simplifyVector = FALSE)
json_sets <- grep("sets_db.json", json_files, value = TRUE) %>% 
  fromJSON(simplifyVector = FALSE)

df_cards <- tibble(
  json = json_cards
) %>% 
  unnest_wider(json)

df_sets <- tibble(
  json = json_sets
) %>% 
  unnest_wider(json) %>% 
  select(set_name, edition) %>% 
  mutate(expansion = str_remove_all(set_name, "\\*"),
         expansion = str_to_lower(expansion)) %>% 
  distinct(expansion) %>% 
  left_join(
    read_csv("cards/expansions.csv") %>% 
      mutate(expansion = str_to_lower(expansion))
    ) %>% 
  distinct(expansion, release) %>% 
  mutate(expansion_ed = expansion,
         expansion = str_remove_all(expansion, "1stedition"),
         expansion = str_remove_all(expansion, "2ndedition"))
      
## title cards
kotw_cards <- df_all %>% 
  unnest(data) %>% 
  select(reddit_id, title) %>% 
  distinct() %>% 
  filter(!is.na(title))



test <- kotw_cards %>% 
  filter(grepl("Bane", title)) 


test %>% 
  mutate(test = gsub("Bane\\: *(.*?)", "\\1",title)) %>% 
  select(test)

pull(test[1, 2]) %>% str_


kotw_cards %>%
  # corrections
  mutate(title = str_remove_all(title, ' \\"Renaissance Fair\\"'),
         title = str_replace_all(title, "Landmark\\: Obelisk \\(naming Throne Room\\)", "Landmark\\: Obelisk naming Throne Room"),
         title = gsub(".*[0-9]\\:", "", title),
         title = str_replace_all(title, "\\, Way\\: ", "\\. Way\\: "),
         title = str_replace_all(title, "No Colony\\/Platinum\\/Shelters", "No Colony\\/Platinum\\. No Shelters"),
         title = str_replace_all(title, "Salt the Earth\\, Wall\\, Sinister Plot\\.", "Event\\: Salt the Earth\\, Wall\\, Sinister Plot\\."),
         title = str_replace_all(title, "Dominion, Seaside, Prosperity, Cornucopia, Hinterland, Empires, Nocturne", "Dominion, Seaside, Prosperity, Cornucopia, Hinterlands, Empires, Nocturne"),
         title = str_replace_all(title, "Sage as Bane", "Bane\\: Sage"),
         title = str_replace_all(title, "Watch Tower", "Watchtower"),
         title = str_replace_all(title, "Stewart", "Steward"),
         title = str_replace_all(title, "Sauna\\/Avanto", "Sauna\\, Avanto"),
         title = str_replace_all(title, "Settlers\\/Bustling", "Settlers\\, Bustling"),
         title = str_remove_all(title, "BM Deck: Prosperity and Cornucopia, excluding Tournament and Young Witch.")) %>% 
  separate_rows(title, sep = "\\,") %>% 
  separate_rows(title, sep = "\\:") %>% 
  separate_rows(title, sep = "\\;") %>% 
  separate_rows(title, sep = "\\.") %>% 
  mutate(title = str_trim(title),
         title = str_to_lower(title),
         title = str_remove_all(title, "[[:punct:]]")) %>% 
  mutate(title = str_replace_all(title, "kings court", "king's court"),
         title = str_replace_all(title, "workers village", "worker's village"),
         title = str_replace_all(title, "fools gold", "fool's gold"),
         title = str_replace_all(title, "farmers market", "farmers' market"),
         title = str_replace_all(title, "devils workshop", "devil's workshop"),
         title = str_replace_all(title, "illgotten gains", "ill-gotten gains"),
         title = str_replace_all(title, "philosophers stone", "philosopher's stone"),
         title = ifelse(title  == "candlestick", "candlestick maker", title)) %>% 
  # filter trash
  filter(!grepl("kotw", title, fixed = TRUE),
         !grepl("events", title, fixed = TRUE),
         !grepl("event", title, fixed = TRUE),
         !grepl("landmarks", title, fixed = TRUE),
         !grepl("landmark", title, fixed = TRUE),
         !grepl("way", title, fixed = TRUE),
         !grepl("projects", title, fixed = TRUE),
         !grepl("project", title, fixed = TRUE)) %>%
  left_join(
    df_cards %>% 
      select(card_tag, group_tag) %>% 
      mutate(card_tag = str_to_lower(card_tag)) %>% 
      mutate(key = "card"),
    by = c("title" = "card_tag")
  ) %>% 
  left_join(
    df_sets %>% distinct(expansion) %>% 
      mutate(key = "expansion")
    , 
    by = c("title" = "expansion")
  ) %>% 
  mutate(key = ifelse(is.na(key.x), key.y, key.x)) %>% 
  select(-key.x, -key.y) %>% 
  filter(is.na(key)) %>% count(title, sort = TRUE) %>% print(n=100)





%>% 
    mutate(title = str_remove_all(title, ' \\"Renaissance Fair\\"'),
           title = str_replace_all(title, "Landmark\\: Obelisk \\(naming Throne Room\\)", "Landmark\\: Obelisk naming Throne Room"),
           title = gsub(".*[0-9]\\:", "", title),
           title = str_replace_all(title, "\\, Way\\: ", "\\. Way\\: "),
           title = str_replace_all(title, "No Colony\\/Platinum\\/Shelters", "No Colony\\/Platinum\\. No Shelters"),
           title = str_replace_all(title, "Salt the Earth\\, Wall\\, Sinister Plot\\.", "Event\\: Salt the Earth\\, Wall\\, Sinister Plot\\."),
           title = str_replace_all(title, "Dominion, Seaside, Prosperity, Cornucopia, Hinterland, Empires, Nocturne", "Dominion, Seaside, Prosperity, Cornucopia, Hinterlands, Empires, Nocturne"),
           title = str_replace_all(title, "Sage as Bane", "Bane\\: Sage")) %>% 
    #filter(grepl("Landmark", title)) %>%
    separate_rows(title, sep = "\\.") %>%
    separate_rows(title, sep = ";") %>%
    separate_rows(title, sep = " \\(") %>% 
    mutate(indicator = case_when(grepl("Landmarks\\:", title) ~ "Landmark",
                                 grepl("Landmark\\:", title) ~ "Landmark",
                                 grepl("Way\\:", title) ~ "Way",
                                 grepl("Event\\:", title) ~ "Event",
                                 grepl("Events\\:", title) ~ "Event",
                                 grepl("Project\\:", title) ~ "Project",
                                 grepl("Projects\\:", title) ~ "Project",
                                 grepl("Shelters", title) ~ "Shelters",
                                 grepl("Colony", title) ~ "Colony_Platinum",
                                 grepl("\\[", title) ~ "Expansion",
                                 grepl("Bane\\:", title) ~ "Bane",
                                 grepl("Boons\\:", title) ~ "Boon",
                                 TRUE ~ "cards"), 
           title = str_remove_all(title, "\\["),
           title = str_remove_all(title, "\\]"),
           title = str_remove_all(title, "\\)"),
           title = str_trim(title),
           title = as.character(title),
           title = tolower(title),
           title = str_to_title(title)) %>% 
    pivot_wider(names_from = indicator,
                values_from = title)
    
    
    df_wide %>% 
      mutate_at(if("cards" %in% names(df_wide)) "cards" else integer(0), as.character) %>% 
      mutate_at(if("Event" %in% names(df_wide)) "Event" else integer(0), as.character) %>% 
      mutate_at(if("Expansion" %in% names(df_wide)) "Expansion" else integer(0), as.character) %>% 
      mutate_at(if("Project" %in% names(df_wide)) "Project" else integer(0), as.character) %>% 
      mutate_at(if("Colony_Platinum" %in% names(df_wide)) "Colony_Platinum" else integer(0), as.character) %>% 
      mutate_at(if("Landmark" %in% names(df_wide)) "Landmark" else integer(0), as.character) %>% 
      mutate_at(if("Way" %in% names(df_wide)) "Way" else integer(0), as.character) %>% 
      mutate_at(if("Shelters" %in% names(df_wide)) "Shelters" else integer(0), as.character) %>% 
      mutate_at(if("Bane" %in% names(df_wide)) "Bane" else integer(0), as.character) %>% 
      mutate_at(if("Boon" %in% names(df_wide)) "Boon" else integer(0), as.character) %>% 
      mutate(cards = str_remove_all(cards, '\\"'),
             cards = str_remove_all(cards, "c\\("),
             cards = str_remove_all(cards, "\\, No Events\\/Landmarks"),
             cards = str_remove_all(cards, "\\, No Events"),
             cards = case_when(!grepl("Bane", cards) ~ str_remove_all(cards, "\\)"),
                               TRUE ~ cards),
             cards = str_remove_all(cards, "It\\'s Terminal "),
             cards = str_remove_all(cards, "\\*"),
             cards = str_replace_all(cards, "Smugglers", "Smuggler"),
             cards = str_replace_all(cards, "Smuggler", "Smugglers"),
             cards = str_replace_all(cards, "Watch Tower", "Watchtower"),
             cards = str_replace_all(cards, "Stewart", "Steward"),
             cards = str_replace_all(cards, "Settlers\\/Bustling Village", "Settlers"),
             cards = str_replace_all(cards, "Settlers", "Settlers\\/Bustling Village"),
             cards = str_replace_all(cards, "Gladiator\\/Fortune", "Gladiator"),
             cards = str_replace_all(cards, "Gladiator", "Gladiator\\/Fortune"),
             cards = str_replace_all(cards, "Encampment\\/Plunder", "Encampment"),
             cards = str_replace_all(cards, "Encampment", "Encampment\\/Plunder"),
             cards = str_replace_all(cards, "Candlestick Maker", "Candlestick"),
             cards = str_replace_all(cards, "Candlestick", "Candlestick Maker"))
    df
    
    
    ,
             Bane = str_remove_all(Bane, "Bane\\:"),
             Bane = str_trim(Bane),
             Event = str_remove_all(Event, "Event\\: "),
             Event = str_remove_all(Event, "Events\\: "),
             Event = str_replace_all(Event, "Scouting Party", "Scouting Part"),
             Landmark = str_remove_all(Landmark, "Landmark\\: "),
             Landmark = str_remove_all(Landmark, "Landmarks\\: "),
             Project = str_remove_all(Project, "Project\\: "),
             Project = str_remove_all(Project, "Projects\\: "),
             Way = str_remove_all(Way, "Way\\: "),
             Shelters = str_replace_all(Shelters, "With Shelters", "Shelters"),
             Boon = str_remove_all(Boon, "Boons\\: "),
             Expansion = str_replace_all(Expansion, "Promos", "Promo"))
