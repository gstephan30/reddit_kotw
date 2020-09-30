# Analysing Dominion Kingdom of the Week Cards from reddit


Importing KotW set from [reddit](https://www.reddit.com/r/dominion/search/?q=%22KotW%20%22&restrict_sr=1).


### Importing the data via reddits json API interface with R with this code:

```
library(tidyverse)
library(jsonlite)

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

```

### First Exploration

![](https://github.com/gstephan30/reddit_kotw/blob/master/descrip_git.png?raw=true)

