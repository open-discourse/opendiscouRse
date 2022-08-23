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

#' Get legislative periods where politicians make speeches.
#'
#'
#' @param data Input data frame.
#' @param input_id Input politician id.
#' @param output_format Format of output, either "data.frame" or "vector". Default is "data.frame".
#'
#' @return A data frame or (integer) vector.
#' @importFrom magrittr %>%
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

#' Get main table ("Table 1") with descriptive summaries of the database.
#'
#' @param table_speeches A `data.frame` object, indicating the `speeches` table.
#' @param table_contributions  A `data.frame` object, indicating the `contributions_simplified` table.
#' @param output_format A `character`, either `"data.frame"` or `"latex"`, indicating the output format. Default is `"data.frame"`.
#'
#' @return Either a `data.frame` (default), or `LaTeX` table code.
#' @importFrom magrittr %>%
#' @export
#'
get_table_1 <- function(table_speeches, table_contributions, output_format = "data.frame") {
  checkmate::assert_data_frame(table_speeches)
  checkmate::assert_data_frame(table_contributions)

  min_date <- table_speeches %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::filter(date == min(date)) %>%
    dplyr::distinct(electoral_term, date) %>%
    dplyr::rename(`Earliest Date` = date) %>%
    dplyr::ungroup()

  max_date <- table_speeches %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::distinct(electoral_term, date) %>%
    dplyr::rename(`Latest Date` = date) %>%
    dplyr::ungroup()

  sessions_count <- table_speeches %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::distinct(session) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Sessions Count` = n,
      `Cumulated Sessions Count` = cum_sum_n
    )

  speeches_count <- table_speeches %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Speeches Count` = n,
      `Cumulated Speeches Count` = cum_sum_n
    )

  tokens_count <- table_speeches %>%
    dplyr::mutate(n_tokens = stringr::str_count(speech_content, "\\w+")) %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::summarise(n = sum(n_tokens)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Tokens Count` = n,
      `Cumulated Tokens Count` = cum_sum_n
    )

  contrib_et <- table_speeches %>%
    dplyr::select(id, electoral_term, date) %>%
    dplyr::right_join(table_contributions, by = c("id" = "speech_id"))

  contributions_count <- contrib_et %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Contributions Count` = n,
      `Cumulated Contributions Count` = cum_sum_n
    )

  tokens_contributions_count <- contrib_et %>%
    dplyr::mutate(n_tokens = stringr::str_count(content, "\\w+")) %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::summarise(n = sum(n_tokens)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Tokens Count` = n,
      `Cumulated Tokens Count` = cum_sum_n
    )

  df <- purrr::reduce(
    list(
      min_date,
      max_date,
      sessions_count,
      speeches_count,
      tokens_count,
      contributions_count,
      tokens_contributions_count
    ),
    dplyr::left_join
  ) %>%
    suppressMessages() %>%
    dplyr::rename(`Electoral Term` = electoral_term)

  if (output_format == "data.frame") {
    df
  } else if (output_format == "latex") {
    latex_table <- df %>%
      dplyr::mutate(
        dplyr::across(
        4:dplyr::last_col(),
        ~ scales::number(.x, big.mark = ".", decimal.mark = " ")
        )
      ) %>%
      knitr::kable(
        format = "latex",
        booktabs = T,
        escape = F,
        col.names = kableExtra::linebreak(
          colnames(table1) %>% stringr::str_replace(" ", "\n"),
          align = "l"
          ),
        align = paste0(
          "r",
          paste0(rep("l", length(colnames(df))), collapse = "")
        )
      ) %>%
      kableExtra::kable_styling(full_width = F, position = "left", latex_options = "scale_down") %>%
      kableExtra::row_spec(0, align = "c", bold = T) %>%
      kableExtra::column_spec(1, bold = T)

    message("When using this table in LaTeX, you have to include the package 'makecell'.")
    return(latex_table)
  }
}



