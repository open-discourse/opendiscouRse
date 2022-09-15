library(magrittr)

# get_age() ---------------------------------------------------------------
test_that("get_age() returns numeric value", {
  expect_numeric(get_age(Sys.Date()))
})

# get_age_hist() ----------------------------------------------------------
test_that("get_age_hist() returns numeric value", {
  expect_numeric(get_age_hist(Sys.Date(), Sys.Date()))
})

# get_faction_color() -----------------------------------------------------
test_that("get_faction_color() returns consistent color codes", {
  expect_true(
    get_faction_color(
      readr::read_csv(
        test_path(
          "data",
          "faction_colors.csv"
        )
      ) %>%
        dplyr::pull(faction_id)
    ) %>%
      na.omit() %>%
      purrr::map(
        ~ stringr::str_length(.x)
      ) %>%
      unique() %>%
      length() == 1
  )
})

# get_ets() ---------------------------------------------------------------
test_that("get_ets() returns data.frame config 1", {
  expect_data_frame(
    get_ets(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      "electoral_term",
      dummy = TRUE,
      merge = TRUE
    )
  )
})

test_that("get_ets() returns data.frame config 2", {
  expect_data_frame(
    get_ets(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      "electoral_term",
      dummy = TRUE,
      merge = FALSE
    )
  )
})

test_that("get_ets() returns data.frame config 3", {
  expect_data_frame(
    get_ets(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      "electoral_term",
      dummy = FALSE,
      merge = TRUE
    )
  )
})

test_that("get_ets() returns data.frame config 4", {
  expect_data_frame(
    get_ets(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      "electoral_term",
      dummy = FALSE,
      merge = FALSE
    )
  )
})

# get_profession_groups() -------------------------------------------------
test_that("get_profession_groups() returns data.frame config 1", {
  expect_data_frame(
    get_profession_groups(
      readr::read_csv(
        test_path(
          "data",
          "politicians.csv"
        )
      ),
      "profession",
      merge = TRUE
    )
  )
})

test_that("get_profession_groups() returns data.frame config 2", {
  expect_data_frame(
    get_profession_groups(
      readr::read_csv(
        test_path(
          "data",
          "politicians.csv"
        )
      ),
      "profession",
      merge = FALSE
    )
  )
})

