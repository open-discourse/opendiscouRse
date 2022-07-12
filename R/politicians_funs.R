# if(!exists("politicians")) {
#   politicians <- readRDS("data/politicians.RDS")
# }

valid_grouping_vars <- c(
  "birth_place",
  "birth_country",
  "gender",
  "aristocracy"
  # "academic_title"
  # "age",
  # "profession" # depends on data format
)

show_valid_grouping_vars <- function() {
  valid_grouping_vars
}

# not sensitive for gender specific profession values
count_professions <- function(data, grouping_vars, sort_n_desc = TRUE, exclude_na = FALSE) {
  max_n_jobs <- politicians %>%
    dplyr::mutate(n_comma = stringr::str_count(profession, ",")) %>%
    dplyr::summarise(max(n_comma, na.rm = T)) %>%
    dplyr::pull() + 1

  professions_long <- politicians %>%
    tidyr::separate(profession,
                    into = purrr::map_chr(1:max_n_jobs, ~ paste0("profession_", .x)),
                    sep = ",") %>%
    tidyr::pivot_longer(
      cols = starts_with("profession"),
      names_to = "profession_nr",
      values_to = "profession_value"
    ) #%>%
    # keep? -- manipulation of original version of data set
    # dplyr::mutate(profession_value = stringr::str_trim(profession_value)) %>%
    # suppressWarnings()

  if (missing(grouping_vars)) {
    df <- professions_long %>%
      dplyr::group_by(profession_value) %>%
      dplyr::count()
  }
  else if (!all(grouping_vars %in% valid_grouping_vars)) {
    stop ("Select a set of valid grouping variables.")
  }
  else {
    df <- professions_long %>%
      dplyr::group_by(
        across(
          c("profession_value", {{ grouping_vars }})
        )
      ) %>%
      dplyr::count()
  }
  if (sort_n_desc == TRUE) {
    df %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
        dplyr::desc(
          n
        )
      )
  }
  else {
    df
  }
}


