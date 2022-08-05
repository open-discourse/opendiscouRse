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

get_state <- function(politician_id, electoral_term) {
  url <- "https://www.bundestag.de/resource/blob/472878/4b9303987cc0520ed0d56b7a0311930a/MdB-Stammdaten-data.zip"
  temp <- tempfile()
  download.file(url, temp, quiet = TRUE)
  stammdaten <- xml2::read_xml(unz(temp, "MDB_STAMMDATEN.XML"))
  unlink(temp)

  blnd_mapping <- c("BAD" = "Baden-Württemberg",
                    "BAY" = "Bayern",
                    "BB" = "Brandenburg",
                    "BE" = "Berlin",
                    "BLN" = "Berlin",
                    "BRA" = "Brandenburg",
                    "BRE" = "Bremen",
                    "BW" = "Baden-Württemberg",
                    "BWG" = "Baden-Württemberg",
                    "BY" = "Bayern",
                    "HB" = "Bremen", # changed
                    "HBG" = "Hamburg",
                    "HE" = "Hessen",
                    "HES" = "Hessen",
                    "HH" = "Hamburg",
                    "MBV" = "Mecklenburg-Vorpommern",
                    "MV" = "Mecklenburg-Vorpommern",
                    "NDS" = "Niedersachsen",
                    "NI" = "Niedersachsen",
                    "NRW" = "Nordrhein-Westfalen",
                    "NW" = "Nordrhein-Westfalen",
                    "RP" = "Rheinland-Pfalz",
                    "RPF" = "Rheinland-Pfalz",
                    "SAA" = "Sachsen-Anhalt",
                    "SAC" = "Sachsen",
                    "SH" = "Schleswig-Holstein",
                    "SL" = "Saarland",
                    "SLD" = "Saarland",
                    "SN" = "Sachsen",
                    "ST" = "Sachsen-Anhalt",
                    "SWH" = "Schleswig-Holstein",
                    "TH" = "Thüringen",
                    "THÜ" =  "Thüringen",
                    "WBB" = "Baden-Württemberg",
                    "WBH" = "Baden-Württemberg") %>%
    tibble::tibble(abbr = names(.), name = .)

  id_col <- rep(
    stammdaten %>%
      xml_find_all("//MDB/ID") %>%
      xml_text(),
    stammdaten %>%
      xml_find_all("//MDB/WAHLPERIODEN") %>%
      xml_length()
  ) %>%
    as_tibble_col("id")

  wkr_land_col <- stammdaten %>%
    xml_find_all(".//WKR_LAND") %>%
    xml_text() %>%
    as_tibble_col("WKR_LAND")

  liste_col <- stammdaten %>%
    xml_find_all(".//LISTE") %>%
    xml_text() %>%
    as_tibble_col("LISTE")

  wp_col <- stammdaten %>%
    xml_find_all(".//WP") %>%
    xml_text() %>%
    as_tibble_col("WP")

  df <- tibble(id_col, wp_col, wkr_land_col, liste_col) %>%
    mutate(
      across(
        c(WKR_LAND, LISTE),
        ~ case_when(
          . == "" ~ NA_character_,
          str_detect(., "\\*") ~ NA_character_,
          TRUE ~ .
        )
      ),
      state = case_when(
        !is.na(WKR_LAND) ~ WKR_LAND,
        !is.na(LISTE) ~ LISTE,
        TRUE ~ NA_character_
      )
    )

  # return_vector <- function() {

  if (all(is.na(politician_id))) {
    return(NA_character_)
  } else if (all(is.na(electoral_term))) {
    return(NA_character_)
  } else {
    df %>%
      filter(id == politician_id, WP == electoral_term) %>%
      pull(state)
  }

  # }

  # return_vector()

  ## !!! ## ADD: LISTE Besonderheiten: Ausnahmen: * Eingliederung Saarland, ** Berlin West Änderungsgesetz, *** von der Volkskammer gewählt

}

