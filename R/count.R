#' Counting observations by grouping variables.
#'
#' @importFrom magrittr %>%
#'
#' @param grouping_vars A character vector containing grouping variables of a data table.
#' @param sort_n_desc A boolean value indicating whether the whole data frame should be sorted in descending order by n. Default is `FALSE`.
#'
#' @return A (grouped) data frame.
#' @export
#'
count_data <- function(data, grouping_vars, sort_n_desc = FALSE) {
  if (missing(grouping_vars)) {
    stop("Indicate at least one variable for grouping data.")
  }
  if (!all(grouping_vars %in% colnames(data))) {
    stop ("Select a set of grouping variables that exist in data.")
  }
  if (!all(grouping_vars %in% VALID_GROUP_VARS)) {
    stop ("Select a set of valid grouping variables.")
  }
  df <- data %>%
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

#' Relative frequencies in data when grouping by more than one variable.
#'
#' @importFrom magrittr %>%
#'
#' @param grouping_vars A character vector containing grouping variables of a data table.
#' @param rel_freq_vars A character vector containing grouping variables of a data table that have to be part of argument `grouping_vars`.
#' @param sort_n_desc A boolean value indicating whether the whole data frame should be sorted in descending order by n. Default is `FALSE`.
#'
#' @return A (grouped) data frame.
#' @export
#'
rel_freq_data <- function(data, grouping_vars, rel_freq_vars, sort_n_desc = FALSE) {
  if (missing(grouping_vars)) {
    stop("Indicate at least one variable for grouping data.")
  }
  else if (missing(rel_freq_vars)) {
    stop("Indicate at least one variable for computing relative frequencies.")
  }
  if (!all(grouping_vars %in% colnames(data))) {
    stop ("Select a set of grouping variables that exist in data.")
  }
  if (!all(grouping_vars %in% VALID_GROUP_VARS)) {
    stop ("Select a set of valid grouping variables.")
  }
  if (!all(rel_freq_vars %in% grouping_vars)) {
    stop ("Select a set of valid grouping variables for computing relative frequencies.")
  }
  if (grouping_vars == rel_freq_vars) {
    warning("Same set of grouping variables is selected. Relative frequency is 1 respectively.")
  }
  df <- data %>%
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
