library(tidyverse)

# get data ----------------------------------------------------------------
contributions_extended <- readRDS("data/contributions_extended.RDS")
contributions_simplified <- readRDS("data/contributions_simplified.RDS")

# n zuruf type
contributions_extended %>% 
  group_by(type) %>% 
  count %>% 
  arrange(desc(n))

# contrib affiliation
contributions_extended %>% 
  group_by(faction_id) %>% 
  count %>% 
  arrange(desc(n))

# n zuruf type by faction
contributions_extended %>% 
  filter(faction_id != -1) %>% 
  group_by(faction_id, type) %>% 
  count %>% 
  group_by(faction_id) %>% 
  mutate(freq = n / sum(n)) %>% 
  # ungroup %>% 
  # not working????
  arrange(desc(freq), .by_group = T) %>% 
  print(n = nrow(.))

# differentiate between individual contribution by politician and general (non-traceable) contribution
contributions_extended %>% 
  mutate(
    # might be not 100% accurate
    contrib_origin = if_else(
      politician_id == -1,
      "general",
      "individual"
    )
  ) %>% 
  group_by(contrib_origin, type) %>% 
  count %>% 
  group_by(type) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(type)

# link with speeches data for electoral term dependency -------------------
speeches <- readRDS("data/speeches.RDS")
factions <- readRDS("data/factions.RDS")

speeches %>% 
  select(id, session, electoral_term, politician_id, faction_id, position_short, date) %>% 
  rename_all(~ paste0("speech_", .)) %>% 
  right_join(contributions_extended) -> contrib_speeches

# n contributions pro LP und faction -- eventutell noch gewichten nach seat share von factions
contrib_speeches %>% 
  group_by(speech_electoral_term, faction_id) %>% 
  filter(faction_id != -1) %>% 
  count %>% 
  group_by(speech_electoral_term) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(speech_electoral_term)) %>% 
  left_join(factions %>% select(faction_id = id, abbreviation)) %>% 
  ungroup %>% 
  arrange(desc(speech_electoral_term %>% as.double()), desc(n)) %>% 
  print(n = nrow(.))

# n contributions pro type, LP und faction
contrib_speeches %>% 
  group_by(speech_electoral_term, type, faction_id) %>% 
  filter(faction_id != -1) %>% 
  count %>% 
  group_by(speech_electoral_term, type) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(speech_electoral_term)) %>% 
  left_join(factions %>% select(faction_id = id, abbreviation)) %>% 
  ungroup %>% 
  arrange(desc(speech_electoral_term %>% as.double()), type, desc(n)) %>% 
  print(n = nrow(.))

  