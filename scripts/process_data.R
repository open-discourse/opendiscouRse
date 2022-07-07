library(tidyverse)

# add political affiliation to politicians --------------------------------

# get data ----------------------------------------------------------------
speeches <- readRDS("data/speeches.RDS")
factions <- readRDS("data/factions.RDS")

speeches %>%
  glimpse

speeches %>% 
  distinct(politician_id, faction_id) -> distinct_pol_fac
  
distinct_pol_fac %>% 
  group_by(politician_id) %>% 
  count %>% 
  arrange(desc(n))
  
distinct_pol_fac %>% 
  group_by(politician_id) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  ungroup %>% 
  filter(n != 1)

# min max date of politician by faction (chancellor adenauer has duplicate observation, one with CDU/CSU (4) affiliation and one with -1)
speeches %>% 
  filter(politician_id != -1) %>% 
  group_by(politician_id, faction_id) %>% 
  summarise(min_date = min(date, na.rm = T), max_date = max(date, na.rm = T)) %>% 
  ungroup %>% 
  left_join(factions, by = c("faction_id" = "id")) -> pol_affiliation_dates

pol_affiliation_dates %>% 
  group_by(politician_id)
  


