    library(httr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    req <- GET("https://api.github.com/repos/blakevanlan/KingdomCreator/git/trees/master?recursive=1")
    stop_for_status(req)
    card_files <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F) %>% 
      grep("jpg", ., value = TRUE)


    files <- paste0("https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/", card_files)

    pictures <- sample(files, 15)
    parts <- split(pictures, ceiling(seq_along(pictures)/5))
    lapply(parts, knitr::include_graphics)

    ## $`1`
    ## [1] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/seaside_bazaar.jpg"            
    ## [2] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/empires_landmark_palace.jpg"
    ## [3] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/nocturne_vampire.jpg"       
    ## [4] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/empires_royalblacksmith.jpg"   
    ## [5] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/empires_capital.jpg"        
    ## attr(,"class")
    ## [1] "knit_image_paths" "knit_asis"       
    ## 
    ## $`2`
    ## [1] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/cornucopia_jester.jpg"    
    ## [2] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/darkages_armory.jpg"   
    ## [3] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/darkages_sirmartin.jpg"
    ## [4] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/common_estate.jpg"        
    ## [5] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/empires_sacrifice.jpg"    
    ## attr(,"class")
    ## [1] "knit_image_paths" "knit_asis"       
    ## 
    ## $`3`
    ## [1] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/nocturne_boon_themoonsgift.jpg"
    ## [2] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/seaside_embargo.jpg"           
    ## [3] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/intrigue_steward.jpg"       
    ## [4] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/alchemy_herbalist.jpg"      
    ## [5] "https://raw.githubusercontent.com/blakevanlan/KingdomCreator/master/docs/img/cards/fr/baseset2_vassal.jpg"        
    ## attr(,"class")
    ## [1] "knit_image_paths" "knit_asis"
