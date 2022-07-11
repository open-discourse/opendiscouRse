if(!exists("speeches")) {
  speeches <- readRDS("data/speeches.RDS")
}

valid_grouping_vars <- speeches %>%
  dplyr::select(-c(id, first_name, last_name, speech_content, document_url, search_speech_content)) %>%
  names()

count_tokens <- function(grouping_vars) {
  if (!all(grouping_vars %in% valid_grouping_vars)) {
  # if (!grouping_vars %in% valid_grouping_vars) {
    stop ("Select a set of valid grouping variables.")
  }
  speeches %>%
    dplyr::group_by(
      across(
        {{ grouping_vars }}
      )
    ) %>%
    dplyr::count()
}

count_tokens(c("faction_id", "electoral_term"))
count_tokens("first_name")

speeches %>%
  group_by(session)


count_tokens <- function(grouping_vars) {
  if (!all(grouping_vars %in% valid_grouping_vars)) {
    stop ("Select a set of valid grouping variables.")
  }
  print("Selection valid.")
}

count_tokens("first_name")
count_tokens(first_name)

count_tokens(faction_id)
count_tokens("faction_id")

count_tokens(c("faction_id", "electoral_term"))
count_tokens(c(faction_id, electoral_term))

count_tokens(c("first_name", "electoral_term"))
count_tokens(c("electoral_term", "first_name"))




(!all(c("electoral_term", "first_name") %in% valid_grouping_vars))
(all(!c("electoral_term", "first_name") %in% valid_grouping_vars))


