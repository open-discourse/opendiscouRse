if(!exists("speeches")) {
  speeches <- readRDS("data/speeches.RDS")
}

# valid_grouping_vars <- speeches %>%
#     dplyr::select(-c(id, first_name, last_name, speech_content, document_url, search_speech_content)) %>%
#     names()

valid_grouping_vars <- c(
  "session",
  "electoral_term",
  "politician_id",
  "faction_id",
  "position_short",
  "position_long",
  "date"
)

#' Counting tokens by grouping variables.
#'
#' @importFrom magrittr %>%
#'
#' @param grouping_vars A character vector containing grouping variables of speeches table.
#' @param sort_n_desc A boolean value indicating whether the whole data frame should be sorted in descending order by n. Default is `FALSE`.
#'
#' @return A (grouped) data frame.
#' @export
#'
count_tokens <- function(grouping_vars, sort_n_desc = FALSE) {
  if (!all(grouping_vars %in% valid_grouping_vars)) {
    stop ("Select a set of valid grouping variables.")
  }
  df <- speeches %>%
    dplyr::group_by(
      across(
        {{ grouping_vars }}
      )
    ) %>%
    dplyr::count()
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

#' Relative frequencies of tokens by grouping variables.
#'
#' @importFrom magrittr %>%
#'
#' @param grouping_vars A character vector containing grouping variables of speeches table.
#' @param rel_freq_vars A character vector containing grouping variables of speeches table that have to be part of argument `grouping_vars`.
#' @param sort_n_desc A boolean value indicating whether the whole data frame should be sorted in descending order by n. Default is `FALSE`.
#'
#' @return A (grouped) data frame.
#' @export
#'
rel_freq_tokens <- function(grouping_vars, rel_freq_vars, sort_n_desc = FALSE) {
  if (!all(grouping_vars %in% valid_grouping_vars)) {
    stop ("Select a set of valid grouping variables.")
  }
  if (!all(rel_freq_vars %in% grouping_vars)) {
    stop ("Select a set of valid grouping variables for computing relative frequencies.")
  }
  df <- speeches %>%
    dplyr::group_by(
      across(
        {{ grouping_vars }}
      )
    ) %>%
    dplyr::count() %>%
    dplyr::group_by(
      across(
        {{ rel_freq_vars }}
      )
    ) %>%
    dplyr::mutate(
      rel_freq = n / sum(n)
    )
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

