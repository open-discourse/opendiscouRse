library(tidyverse)

# get data ----------------------------------------------------------------
speeches <- readRDS("data/speeches.RDS")

speeches %>% 
  glimpse()

# n politicians
speeches %>% 
  select(politician_id) %>% 
  distinct %>% 
  count

# take a look at speech content
speeches %>% 
  slice(1) %>% 
  pull(speech_content)

# length of speech
speeches %>% 
  mutate(length_speech = str_length(speech_content))

# check pattern new electoral term
speeches %>% 
  filter(electoral_term == 2)

# get all speeches by MP per session
speeches %>% 
  filter(session == 1, electoral_term == 1)

# n speeches by politician and electoral term
speeches %>% 
  group_by(politician_id, electoral_term) %>% 
  count() %>% 
  arrange(desc(n)) -> speech_n_politician

speech_n_politician

speeches %>% distinct(id, last_name)

speech_n_politician %>% 
  group_by(politician_id) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  left_join(speeches %>% distinct(id, last_name), by = c("politician_id" = "id"))

# speech n by party/faction
speeches %>% 
  # filter(faction_id != -1) %>% 
  group_by(faction_id) %>% 
  count %>% 
  arrange(desc(n))

# speech n by  electoral term
speeches %>% 
  # filter(faction_id != -1) %>% 
  group_by(electoral_term) %>% 
  count %>% 
  arrange(desc(n)) 

# speech n by electoral term and faction
speeches %>% 
  filter(faction_id != -1) %>% 
  group_by(faction_id, electoral_term) %>% 
  count %>% 
  arrange(desc(n)) 


# distinct positions
speeches %>% 
  group_by(position_short) %>% 
  count %>% 
  arrange(desc(n))

speeches %>% 
  group_by(position_long) %>% 
  count %>% 
  arrange(desc(n))


# most recent electoral term ----------------------------------------------
speeches_recent_electoral_term <- speeches %>% 
  filter(electoral_term == max(electoral_term, na.rm = T))

speeches_recent_electoral_term %>% 
  distinct(id) %>% 
  pull -> politicians_id_lp_20




