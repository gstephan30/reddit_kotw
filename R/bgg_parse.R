library(dplyr)
library(tidyr)
library(purrr)
library(rvest)

dominion_ids <- tribble(
  ~name, ~id,
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
  mutate(url_xml = glue::glue("https://www.boardgamegeek.com/xmlapi/boardgame/{id}?stats=1&comments=1"),
         forum_xml = glue::glue("https://www.boardgamegeek.com/xmlapi/forum?parameters")
         data = map(url_xml, parse_bgg))

forums <- read_xml("https://boardgamegeek.com/xmlapi2/forumlist?id=36218&type=thing")

forum <- read_xml("https://www.boardgamegeek.com/xmlapi2/forum?id=450&page=1")

thread <- read_xml("https://www.boardgamegeek.com/xmlapi2/thread?id=2438171")
