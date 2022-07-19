#' Count observations by grouping variables.
#'
#' @param data Input data frame.
#' @param grouping_vars A character vector containing grouping variables of a data table.
#' @param sort_n_desc A boolean value indicating whether the whole data frame should be sorted in descending order by n. Default is `FALSE`.
#'
#' @return A (grouped) data frame.
#' @import checkmate
#' @importFrom magrittr %>%
#' @export
#'
count_data <- function(data, grouping_vars, sort_n_desc = FALSE) {
  assert_false(
    is.null(grouping_vars)
  )

  purrr::map(
    grouping_vars,
    ~ assert(
      check_character(data[[.x]]),
      # check_date(data[[.x]]),
      check_factor(data[[.x]]),
      check_integerish(data[[.x]])
    )
  )

  assert(
    check_subset(
      grouping_vars,
      VALID_GROUP_VARS
    )
  )

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
#' @param data Input data frame.
#' @param grouping_vars A character vector containing grouping variables of a data table.
#' @param rel_freq_vars A character vector containing grouping variables of a data table that have to be part of argument `grouping_vars`.
#' @param sort_n_desc A boolean value indicating whether the whole data frame should be sorted in descending order by n. Default is `FALSE`.
#'
#' @return A (grouped) data frame.
#' @import checkmate
#' @importFrom magrittr %>%
#' @export
#'
rel_freq_data <- function(data, grouping_vars, rel_freq_vars, sort_n_desc = FALSE) {
  assert_false(
    is.null(grouping_vars)
  )

  assert_false(
    is.null(rel_freq_vars)
  )

  purrr::map(
    grouping_vars,
    ~ assert(
      check_character(data[[.x]]),
      # check_date(data[[.x]]),
      check_factor(data[[.x]]),
      check_integerish(data[[.x]])
    )
  )

  assert(
    check_subset(
      grouping_vars,
      colnames(data)
    ),
    check_subset(
      grouping_vars,
      VALID_GROUP_VARS
    ),
    check_subset(
      rel_freq_vars,
      grouping_vars
    )
  )

  assert_false(
    grouping_vars == rel_freq_vars
  )

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
