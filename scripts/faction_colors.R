library(tidyverse)
library(partycoloR) # devtools::install_github("lwarode/partycoloR")

# get data ----------------------------------------------------------------
factions <- readRDS("data/factions.RDS")

# partycoloR --------------------------------------------------------------

faction_colors_fun <- function() {
  # URL list of Wikipedia party articles
  wikipedia_raw <- read_csv("https://raw.githubusercontent.com/hdigital/partyfactsdata/master/import/wikipedia/wikipedia.csv")
  
  # DEU subset
  wikipedia_de <- wikipedia_raw %>%
    filter(country == "DEU")
  
  # URL list
  wikipedia_de_url_list <- as.list(wikipedia_de$url)
  
  # applying partycoloR::wikipedia_party_color()
  party_de_color <- wikipedia_party_color(wikipedia_de_url_list)
  
  # wrangling data
  party_de_color_link <- wikipedia_de %>%
    left_join(party_de_color, by = "url") %>%
    filter(!is.na(color_1)) %>%
    # switching first color
    mutate(color_1 = case_when(
      name_short %in% c("CDU", "DIE/LINKE") ~ color_2,
      name_short == "NPD" ~ color_4,
      TRUE ~ color_1)
    ) %>% 
    right_join(factions, by = c("name_short" = "abbreviation")) 
}

faction_colors <- faction_colors_fun()

# named character with color codes
faction_colors_vec <- faction_colors %>%
  na.omit(color_1) %>%
  pull(color_1)

names(faction_colors_vec) <- faction_colors_vec %>%
  na.omit(color_1) %>%
  pull(party_name_short)
