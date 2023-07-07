library(magrittr)

# remove after working solution is found
VALID_GROUP_VARS <<- c(
  "session",
  "electoral_term",
  "politician_id",
  "faction_id",
  "position_short",
  "position_long",
  "date",
  "birth_place",
  "birth_country",
  "gender",
  "aristocracy",
  "speech_id",
  "type"
)

# count_data() ------------------------------------------------------------
test_that("count_data() returns grouped data.frame", {
  expect_class(
    count_data(
      readr::read_csv(
        test_path(
          "data",
          "politicians.csv"
        )
      ),
      grouping_vars = "gender"
    ),
    c("grouped_df", "data.frame")
  )
})

test_that("count_data() with `sort_n_desc = TRUE` returns sorted grouped data.frame", {
  n_1 <- count_data(
    readr::read_csv(
      test_path(
        "data",
        "politicians.csv"
      )
    ),
    grouping_vars = "gender",
    sort_n_desc = TRUE
  ) %>%
    dplyr::slice(1) %>%
    dplyr::pull(n)

  n_2 <- count_data(
    readr::read_csv(
      test_path(
        "data",
        "politicians.csv"
      )
    ),
    grouping_vars = "gender",
    sort_n_desc = TRUE
  ) %>%
    dplyr::slice(2) %>%
    dplyr::pull(n)

  expect_true(n_1 > n_2)
})

# rel_freq_data() ---------------------------------------------------------
test_that("rel_freq_data() returns grouped data.frame", {
  expect_class(
    rel_freq_data(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      grouping_vars = c("electoral_term", "faction_id"),
      rel_freq_vars = "faction_id"
    ),
    c("grouped_df", "data.frame")
  )
})

test_that("rel_freq_data() with `sort_n_desc = TRUE` returns sorted grouped data.frame", {
  n_1 <- rel_freq_data(
    readr::read_csv(
      test_path(
        "data",
        "test_speeches.csv"
      )
    ),
    grouping_vars = c("electoral_term", "faction_id"),
    rel_freq_vars = "faction_id",
    sort_n_desc = TRUE
  ) %>%
    dplyr::slice(1) %>%
    dplyr::pull(n)

  n_2 <- rel_freq_data(
    readr::read_csv(
      test_path(
        "data",
        "test_speeches.csv"
      )
    ),
    grouping_vars = c("electoral_term", "faction_id"),
    rel_freq_vars = "faction_id",
    sort_n_desc = TRUE
  ) %>%
    dplyr::slice(2) %>%
    dplyr::pull(n)

  expect_true(n_1 > n_2)
})


