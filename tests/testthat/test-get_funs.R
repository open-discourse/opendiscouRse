library(magrittr)

# get_age() ---------------------------------------------------------------
test_that("get_age() returns numeric value", {
  expect_numeric(get_age(Sys.Date()))
})

# get_age_hist() ----------------------------------------------------------
test_that("get_age_hist() returns numeric value", {
  expect_numeric(get_age_hist(Sys.Date(), Sys.Date()))
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

# get_state() -------------------------------------------------------------
test_that("get_state() returns a `character` vector", {
  pol_vec <- readr::read_csv(
      test_path(
        "data",
        "test_speeches.csv"
      )
    ) %>%
    dplyr::pull(politician_id) %>%
    as.character()

  elec_vec <- readr::read_csv(
    test_path(
      "data",
      "test_speeches.csv"
    )
  ) %>%
    dplyr::pull(electoral_term) %>%
    as.character()

  expect_vector(
    get_state(pol_vec, elec_vec)
  )
})

test_that("get_state() can be used within dplyr::mutate()", {
  expect_data_frame(
    readr::read_csv(
      test_path(
        "data",
        "test_speeches.csv"
      )
    ) %>%
      dplyr::mutate(
        state = get_state(politician_id, electoral_term)
      )
  )
})

# get_table_1() -----------------------------------------------------------
test_that("get_table_1() returns `data.frame`", {
  expect_data_frame(
    get_table_1(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      readr::read_csv(
        test_path(
          "data",
          "test_contributions_simplified.csv"
        )
      ),
      output_format = "data.frame"
    )
  )
})

test_that("get_table_1() returns `knitr_kable`", {
  expect_class(
    get_table_1(
      readr::read_csv(
        test_path(
          "data",
          "test_speeches.csv"
        )
      ),
      readr::read_csv(
        test_path(
          "data",
          "test_contributions_simplified.csv"
        )
      ),
      output_format = "latex"
    ),
    "knitr_kable"
  )
})

# get_implausible_data() --------------------------------------------------
test_that("get_implausible_data() returns `data.frame` config 1", {
  speeches <- readr::read_csv(
      test_path(
        "data",
        "test_speeches.csv"
      )
    )

  expect_data_frame(
    get_implausible_data(speeches)
  )
})

test_that("get_implausible_data() returns `data.frame` config 2", {
  contributions_extended <- readr::read_csv(
    test_path(
      "data",
      "test_contributions_extended.csv"
    )
  )

  expect_data_frame(
    get_implausible_data(contributions_extended)
  )
})




