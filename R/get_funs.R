get_age <- function(date, round_val = 2) {
  if (!class(date) == "Date") {
    stop("Select a valid variable of class 'Date'.")
  }
  round(as.numeric(Sys.Date() - date) / 365, round_val)
}

# same but for start of lp
get_age_lp <- function(lp_start_date, date_birth, round_val = 2) {
  if (all(!class(lp_start_date) == "Date" | !class(date_birth) == "Date")) {
    stop("Select a valid variable of class 'Date'.")
  }
  round(as.numeric(lp_start_date - date_birth) / 365, round_val)
}

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
      dplyr::mutate(politician_id = politician_id %>% as.character) %>%
      dplyr::filter(politician_id %in% input_id) %>%
      dplyr::distinct(electoral_term) %>%
      dplyr::pull()
  }
}

get_profession_group <- function() {data, var} {
  jobs <- rjson::fromJSON(file = "data/jobs.json")
}


