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

bane <- kotw_cards %>% 
  filter(grepl("bane", title, ignore.case = TRUE)) %>% 
  mutate(title = ifelse(grepl("Sage as Bane", title), str_replace_all(title, "Sage as Bane", "Bane: Sage"), title)) %>% 
  mutate(bane = str_extract(title, "Bane:.*")) %>% 
  separate(bane, c("bane", "trash"), sep = "\\.") %>% 
  select(-trash) %>% 
  mutate(bane = str_remove_all(bane, "Bane\\: |\\)")) %>% 
  select(reddit_id, bane)

cards_clean_pre <- kotw_cards %>% 
  # corrections
  mutate(title = str_remove_all(title, ' \\"Renaissance Fair\\"'),
         title = ifelse(reddit_id == "4c8hpb", "Beggar, Bishop, Counterfeit, Courtyard, Doctor, Ghost Ship, Ill-Gotten Gains, Stonemason, Throne Room, Trader. No Colony/Platinum; no Shelters. [Dominion, Intrigue, Seaside, Prosperity, Hinterlands, Dark Ages, Guilds]", title),
         title = str_replace_all(title, "Landmark\\: Obelisk \\(naming Throne Room\\)", "Landmark\\: Obelisk"),
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
         title = str_remove_all(title, "[[:punct:]]"),
         title = str_remove_all(title, "promos"),
         title = str_remove_all(title, "all sets")) %>% 
  mutate(title = str_replace_all(title, "kings court", "king's court"),
         title = str_replace_all(title, "workers village", "worker's village"),
         title = str_replace_all(title, "fools gold", "fool's gold"),
         title = str_replace_all(title, "farmers market", "farmers' market"),
         title = str_replace_all(title, "devils workshop", "devil's workshop"),
         title = str_replace_all(title, "illgotten gains", "ill-gotten gains"),
         title = str_replace_all(title, "philosophers stone", "philosopher's stone"),
         title = str_replace_all(title, "night guardian", "guardian"),
         title = ifelse(title  == "scouting part", "scouting party", title),
         title = ifelse(title  == "candlestick", "candlestick maker", title),
         title = ifelse(title  == "bath", "baths", title)) %>% 
  mutate(title = ifelse(title == "smuggler", "smugglers", title)) %>% 
  # filter trash
  filter(!grepl("kotw", title, fixed = TRUE),
         !grepl("events", title, fixed = TRUE),
         !grepl("event", title, fixed = TRUE),
         !grepl("landmarks", title, fixed = TRUE),
         !grepl("landmark", title, fixed = TRUE),
         !grepl("way", title, fixed = TRUE),
         !grepl("projects", title, fixed = TRUE),
         !grepl("project", title, fixed = TRUE),
         !grepl("bane", title, fixed = TRUE)) 




split_pile <- tibble(
  cards_together = c("gladiatorfortune", "gladiatorfortune",
                     "encampmentplunder", "encampmentplunder",
                     "patricianemporium", "patricianemporium",
                     "catapultrocks", "catapultrocks"),
  cards_single = c("gladiator", "fortune", 
                   "encampment", "plunder", 
                   "patrician", "emporium",
                   "catapult", "rocks")
)

split_cards <- function(df, id_column, cards_column) {
  df %>% 
    filter({{ cards_column }} %in% pull(split_pile, cards_together)) %>% 
    rename(cards_together = {{ cards_column }}) %>% 
    left_join(split_pile) %>% 
    select({{ id_column }}, {{ cards_column }} := cards_single) 
}

add_split_cards <- cards_clean_pre %>% 
  bind_rows(split_cards(cards_clean_pre, reddit_id, title)) %>% 
  filter(!title %in% pull(split_pile, cards_together)) %>% 
  arrange(reddit_id) 

correct_boon <- add_split_cards %>% 
  mutate(title = ifelse(title == "river", "the river's gift.", title),
         title = ifelse(title == "sun", "the sun's gift", title),
         title = ifelse(title == "field", "the field's gift", title)) %>% 
  filter(title != "boons")

card_clean <- correct_boon %>%
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
  select(-key.x, -key.y) 


# test
card_clean %>% 
  filter(grepl("farmer", title))

# further cleaning required
card_clean %>% 
  filter(is.na(key)) %>% count(title, sort = TRUE) %>% print(n=100)


  

