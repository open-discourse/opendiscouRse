#' Get age of politician.
#'
#' @param date_birth Birth date of politician.
#' @param round_val Numeric value indicating how much the age should be rounded, default is 2.
#'
#' @return Current age of a politician (numeric).
#' @export
#'
get_age <- function(date_birth, round_val = 2) {
  if (!class(date_birth) == "Date") {
    stop("Select a valid variable of class 'Date'.")
  }
  round(as.numeric(Sys.Date() - date_birth) / 365, round_val)
}

#' Get age of politician based on a historic date.
#'
#' @param date_hist Historic date on which age computation is based on.
#' @param date_birth Birth date of politician.
#' @param round_val Numeric value indicating how much the age should be rounded, default is 2.
#'
#' @return Historic age of a politician based on a certain date, such as the starting date of a legislative period (numeric).
#' @export
#'
get_age_hist <- function(date_hist, date_birth, round_val = 2) {
  if (all(!class(date_hist) == "Date" | !class(date_birth) == "Date")) {
    stop("Select a valid variable of class 'Date'.")
  }
  round(as.numeric(date_hist - date_birth) / 365, round_val)
}

#' Get legislative periods where politicians make speeches.
#'
#' @importFrom magrittr %>%
#'
#' @param data Input data frame.
#' @param input_id Input politician id.
#' @param output_format Format of output, either "data.frame" or "vector". Default is "data.frame".
#'
#' @return A data frame or (integer) vector.
#' @export
#'
get_lps <- function(data, input_id, output_format = "data.frame") {
  if (output_format == "data.frame") {
    # speeches
    data %>%
      dplyr::mutate(politician_id = politician_id %>% as.character()) %>%
      dplyr::filter(politician_id %in% input_id) %>%
      dplyr::distinct(politician_id, electoral_term)
  } else if (output_format == "vector") {
    # speeches
    data %>%
      dplyr::mutate(politician_id = politician_id %>% as.character()) %>%
      dplyr::filter(politician_id %in% input_id) %>%
      dplyr::distinct(electoral_term) %>%
      dplyr::pull()
  }
}

#' Get professional group affiliation of politicians' jobs.
#'
#' @importFrom magrittr %>%
#'
#' @param data Input data frame.
#' @param var Name of variable that contains the profession values.
#' @param merge A boolean value indicating whether to return just the profession groups or the whole data frame. Default is `TRUE`.
#'
#' @return A data frame.
#' @export
#'
get_profession_groups <- function(data, var, merge = TRUE) {
  jobs <- rjson::fromJSON(file = "data/jobs.json")

  jobs_names <- jobs %>%
    names() %>%
    .[!(. == "sonstiges")]

  # helper function
  group_dummy_fun <- function(var_fun, group_name) {
    purrr::map_dbl(
      stringr::str_to_lower(var_fun),
      ~ any(stringr::str_detect(., group_name))
    )
  }

  df <- map(
    jobs_names,
    ~ group_dummy_fun(data %>% dplyr::pull(var) %>% stringr::str_to_lower(), .x)
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



