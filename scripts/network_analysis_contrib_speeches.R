library(tidyverse)
# library(igraph)
# library(tidygraph)
# library(ggraph)
# library(cranlogs)

# get data ----------------------------------------------------------------
contributions_extended <- readRDS("data/contributions_extended.RDS")
speeches <- readRDS("data/speeches.RDS")
factions <- readRDS("data/factions.RDS")

# compare package relevance -----------------------------------------------
cran_downloads(
  packages = c("igraph", "tidygraph", "ggraph", "network"),
  from = Sys.Date() - 365,
  to = Sys.Date()
) %>%
  group_by(package) %>%
  summarise(total_count = sum(count))

# analysis ----------------------------------------------------------------
speeches %>%
  select(id, session, electoral_term, politician_id, faction_id, position_short, date) %>%
  rename_all(~ paste0("speech_", .)) %>%
  right_join(contributions_extended) %>%
  left_join(factions %>% select(faction_id = id, abbreviation)) -> contrib_speeches

edge_list <- contrib_speeches %>%
  select(
    to = speech_politician_id,
    from = politician_id,
    party_to = speech_faction_id,
    party_from = faction_id
  )

graph_from_data_frame(
  edge_list %>% sample_n(100)
  # vertices = contrib_speeches %>% distinct(politician_id)
) %>%
  # plot()
  as_tbl_graph() %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(aes(fill = party_from)) +
  theme_graph()
  class

tidy_graph_contrib_speeches <- graph_from_data_frame(
  d = edge_list %>% slice(10000:11000),
  directed = T
) %>%
  as_tbl_graph()

tidy_graph_contrib_speeches %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(aes(fill = tidy_graph_contrib_speeches$party_from)) +
  theme_graph()

V(tidy_graph_contrib_speeches)$party_from

ggraph(
  graph_from_data_frame(
    edge_list %>% sample_n(100),
    # vertices = contrib_speeches %>% distinct(politician_id)
  ) %>%
    as_tbl_graph()
) +
  geom_edge_link() +
  geom_node_point(aes(fill = party_to)) +
  theme_graph()


ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

