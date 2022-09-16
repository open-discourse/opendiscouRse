#' Get age of politician.
#'
#' @param date_birth Birth `Date` of politician.
#' @param round_val `Numeric` value indicating how much the age should be rounded, default is 2.
#'
#' @return Current age of a politician (`numeric`).
#' @import checkmate
#' @export
#'
get_age <- function(date_birth, round_val = 2) {
  assert_date(date_birth)
  round(as.numeric(Sys.Date() - date_birth) / 365, round_val)
}

#' Get age of politician based on a historic date.
#'
#' @param date_hist Historic `Date` on which age computation is based on.
#' @param date_birth Birth `Date` of politician.
#' @param round_val `Numeric` value indicating how much the age should be rounded, default is 2.
#'
#' @return Historic age of a politician based on a certain date, such as the starting date of a legislative period (`numeric`).
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

#' Generate list of electoral term affiliations per politician.
#'
#' @param data Input `data.frame`.
#' @param id_value Politician ID values.
#'
#' @return A `list`.
#'
.ets_pol_id <- function(data, id_value) {
  data %>%
    dplyr::filter(politician_id == id_value) %>%
    dplyr::mutate(electoral_term = as.numeric(electoral_term)) %>%
    dplyr::distinct(electoral_term) %>%
    dplyr::arrange(electoral_term) %>%
    dplyr::pull() %>%
    list()
}

#' Get electoral term affiliations of politicians.
#'
#' @param data Input `data.frame`.
#' @param var Name of variable (`character`) that contains the electoral term values.
#' @param dummy A `logical` value indicating whether to generate dummy variables per electoral term (`TRUE`) or a single `list` variable. Default is `TRUE`.
#' @param merge A `logical` value indicating whether to return just the generated columns (`FALSE`) or the whole `data.frame` (`TRUE`). Default is `TRUE`.
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
      dplyr::mutate(dplyr::across(dplyr::starts_with("et_"), ~ max(.x))) %>%
      dplyr::select(politician_id, dplyr::starts_with("et_")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(politician_id, .keep_all = TRUE)
    if (merge == TRUE) {
      data %>%
        dplyr::left_join(ets_df, by = "politician_id") %>%
        suppressMessages()
    } else {
      ets_df
    }
  } else {
    data$electoral_terms <- purrr::map(
      data %>% dplyr::pull(politician_id),
      ~ .ets_pol_id(data = data, id_value = .x)
    ) %>%
      purrr::map(
        ~ purrr::pluck(.x, 1)
      ) %>%
      tibble::as_tibble_col()
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
#' @param var `character` name of variable that contains the profession values.
#' @param group_name `character` name of the profession group.
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
#' @param data Input `data.frame`.
#' @param var Name of variable (`character`) that contains the profession values.
#' @param merge A `logical` value indicating whether to return just the profession groups or the whole data frame. Default is `TRUE`.
#'
#' @return A data frame.
#' @importFrom magrittr %>%
#' @export
#'
get_profession_groups <- function(data, var, merge = TRUE) {
  checkmate::assert_data_frame(data)

  jobs <- rjson::fromJSON(file = "data/jobs.json")

  jobs_names <- jobs %>%
    names() %>%
    .[!(. == "sonstiges")]

  df <- purrr::map(
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

#' Get color code of factions.
#'
#' @param input_id A `character` indicating the ID of the faction.
#' @param id_type A `character` indicating the type of the `input_id`, either `"number"` (ID of the faction) or `"faction_name"` (abbreviated name of the faction). Default is `"number"`.
#'
#' @return A (named) `character` yielding a hexadecimal color code.
#' @importFrom magrittr %>%
#' @export
#'
get_faction_color <- function(input_id, id_type = "number") {
  faction_colors <- readr::read_csv("data/faction_colors.csv") %>%
    suppressMessages()
  # faction_colors <- read.csv(
  #   system.file("data", "faction_colors.csv", package = "opendiscouRse")
  # )
  # faction_colors <- readr::read_csv(
  #   file.path(
  #     system.file("data", package = "opendiscouRse"),
  #     "faction_colors.csv"
  #   )
  # )

  colors <- faction_colors %>% dplyr::pull(hex_color_code)
  if (id_type == "number") {
    names(colors) <- faction_colors %>% dplyr::pull(faction_id)
  } else if (id_type == "faction_name") {
    names(colors) <- faction_colors %>% dplyr::pull(abbreviation)
  }

  colors[as.character(input_id)]
}

#' Get state affiliation of elected politician (either list or direct mandate).
#'
#' @param politician_id A `character` vector containing the politician ID values.
#' @param electoral_term A `character` vector containing the electoral term values (number of the legislative period).
#'
#' @return An object of type `character`.
#' @importFrom magrittr %>%
#' @export
#'
get_state <- function(politician_id, electoral_term) {
  checkmate::assert_multi_class(
    politician_id,
    c("character", "numeric")
  )
  checkmate::assert_multi_class(
    electoral_term,
    c("character", "numeric")
  )

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
      xml2::xml_find_all("//MDB/ID") %>%
      xml2::xml_text(),
    stammdaten %>%
      xml2::xml_find_all("//MDB/WAHLPERIODEN") %>%
      xml2::xml_length()
  ) %>%
    tibble::as_tibble_col("id")

  wkr_land_col <- stammdaten %>%
    xml2::xml_find_all(".//WKR_LAND") %>%
    xml2::xml_text() %>%
    tibble::as_tibble_col("WKR_LAND")

  liste_col <- stammdaten %>%
    xml2::xml_find_all(".//LISTE") %>%
    xml2::xml_text() %>%
    tibble::as_tibble_col("LISTE")

  wp_col <- stammdaten %>%
    xml2::xml_find_all(".//WP") %>%
    xml2::xml_text() %>%
    tibble::as_tibble_col("WP")

  df <- tibble::tibble(id_col, wp_col, wkr_land_col, liste_col) %>%
    dplyr::mutate(
      volkskammer_dummy = ifelse(stringr::str_detect(LISTE, "\\*\\*\\*"), 1, 0),
      dplyr::across(
        c(WKR_LAND, LISTE),
        ~ dplyr::case_when(
          . == "" ~ NA_character_,
          stringr::str_detect(., "\\*\\*\\*") ~ NA_character_,
          stringr::str_detect(., "\\*\\*") ~ "BLN",
          stringr::str_detect(., "\\*") ~ "SLD",
          TRUE ~ .
        )
      ),
      state = dplyr::case_when(
        !is.na(WKR_LAND) ~ WKR_LAND,
        !is.na(LISTE) ~ LISTE,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::left_join(blnd_mapping, by = c("state" = "abbr"))

  check_volkskammer <- df$volkskammer_dummy[match(
    paste0(politician_id, "_", electoral_term),
    df %>% dplyr::transmute(paste0(id, "_", WP)) %>% dplyr::pull()
  )]

  check_volkskammer[is.na(check_volkskammer)] <- 0

  if (!is.null(check_volkskammer)) {
    if (any(check_volkskammer == 1)) {
      warning('NA values are generated for observations that are labelled with "von der Volkskammer gewählt".')
    }
  }

  df$name[match(
    paste0(politician_id, "_", electoral_term),
    df %>% dplyr::transmute(paste0(id, "_", WP)) %>% dplyr::pull()
  )]
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
          colnames(df) %>% stringr::str_replace(" ", "\n"),
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

#' Get data that is implausible based on specific data table based information.
#'
#' @param data Input `data.frame`.
#'
#' @return A `data.frame`.
#' @importFrom magrittr %>%
#' @export
#'
get_implausible_data <- function(data) {
  checkmate::assert_data_frame(data)

  if (deparse(substitute(data)) == "speeches") {
    data %>%
      dplyr::filter(position_short == "Member of Parliament") %>%
      dplyr::filter(faction_id == -1)
  } else if (deparse(substitute(data)) == "contributions_extended") {
    data %>%
      dplyr::filter(politician_id != -1) %>%
      dplyr::filter(faction_id == -1)
  }
}

