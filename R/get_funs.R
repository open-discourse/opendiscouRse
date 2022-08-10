#' Get age of politician.
#'
#' @param date_birth Birth date of politician.
#' @param round_val Numeric value indicating how much the age should be rounded, default is 2.
#'
#' @return Current age of a politician (numeric).
#' @import checkmate
#' @export
#'
get_age <- function(date_birth, round_val = 2) {
  assert_date(date_birth)
  round(as.numeric(Sys.Date() - date_birth) / 365, round_val)
}

#' Get age of politician based on a historic date.
#'
#' @param date_hist Historic date on which age computation is based on.
#' @param date_birth Birth date of politician.
#' @param round_val Numeric value indicating how much the age should be rounded, default is 2.
#'
#' @return Historic age of a politician based on a certain date, such as the starting date of a legislative period (numeric).
#' @import checkmate
#' @export
#'
get_age_hist <- function(date_hist, date_birth, round_val = 2) {
  assert(
    check_date(date_hist),
    check_date(date_birth)
  )
  round(as.numeric(date_hist - date_birth) / 365, round_val)
}

#' Generate list of electoral term affilions per politician.
#'
#' @param data Input `data.frame`.
#' @param id_value Politician ID values.
#'
#' @return A `list`.
#'
.ets_pol_id <- function(data, id_value) {
  data %>%
    dplyr::filter(politician_id == id_value) %>%
    dplyr::distinct(electoral_term) %>%
    dplyr::arrange(electoral_term) %>%
    dplyr::pull() %>%
    list()
}

#' Get electoral term affiliations of politicians.
#'
#' @param data Input `data.frame`.
#' @param var Name of variable (`character`) that contains the electoral term values.
#' @param dummy
#' @param merge
#'
#' @return A `data.frame`.
#' @importFrom magrittr %>%
#' @export
#'
get_ets <- function(data, var, dummy = TRUE, merge = TRUE) {
  checkmate::check_data_frame(data)

  var <- rlang::sym(var)

  if (dummy == TRUE) {
    ets_df <- data %>%
      dplyr::arrange(as.numeric(!!var)) %>%
      tidyr::pivot_wider(
        names_from = !!var,
        names_prefix = "et_",
        values_from = !!var
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("et_"), ~ ifelse(!is.na(.x), 1, 0))
      ) %>%
      dplyr::group_by(politician_id) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("et_"), ~ max(.x))) %>%
      dplyr::select(dplyr::starts_with("et_"))
    if (merge == TRUE) {
      data %>%
        dplyr::left_join(lps_df)
    } else {
      ets_df
    }
  } else {
    data$electoral_terms <- purrr::map(
      data %>% dplyr::pull(politician_id),
      ~ .ets_pol_id(data = df, id_value = .x)
    ) %>%
      purrr::map(
        ~ purrr::pluck(.x, 1)
      )
    if (merge == TRUE) {
      data
    } else {
      data %>%
        dplyr::select(electoral_terms)
    }
  }
}

#' Generate dummy variables for profession variable.
#'
#' @param var Name of variable that contains the profession values.
#' @param group_name Name of the profession group.
#'
#' @return
#'
.group_dummy_fun <- function(var, group_name) {
  purrr::map_dbl(
    stringr::str_to_lower(var),
    ~ any(stringr::str_detect(., group_name))
  )
}

#' Get professional group affiliation of politicians' jobs.
#'
#' @param data Input data frame.
#' @param var Name of variable that contains the profession values.
#' @param merge A boolean value indicating whether to return just the profession groups or the whole data frame. Default is `TRUE`.
#'
#' @return A data frame.
#' @importFrom magrittr %>%
#' @export
#'
get_profession_groups <- function(data, var, merge = TRUE) {
  check_data_frame(data)

  jobs <- rjson::fromJSON(file = "data/jobs.json")

  jobs_names <- jobs %>%
    names() %>%
    .[!(. == "sonstiges")]

  df <- map(
    jobs_names,
    ~ .group_dummy_fun(data %>% dplyr::pull(var) %>% stringr::str_to_lower(), .x)
  ) %>%
    dplyr::bind_cols() %>%
    suppressMessages() %>%
    dplyr::rename_with(~ paste0("group_", jobs_names))

  if (merge == TRUE) {
    data %>%
      dplyr::bind_cols(df)
  } else {
    df
  }
}



