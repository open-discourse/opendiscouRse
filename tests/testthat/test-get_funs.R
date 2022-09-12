library(magrittr)

test_that("get_age() returns numeric value", {
  expect_numeric(get_age(Sys.Date()))
})

test_that("get_age_hist() returns numeric value", {
  expect_numeric(get_age_hist(Sys.Date(), Sys.Date()))
})

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
