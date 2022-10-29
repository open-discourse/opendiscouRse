library(magrittr)

# remove after working solution is found
VALID_GROUP_VARS <- c(
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

# plot_dist() -------------------------------------------------------------
test_that("plot_dist() returns `ggplot` object", {
  expect_class(
    plot_dist(
      readr::read_csv(
        test_path(
          "data",
          "politicians.csv"
        )
      ),
      metric_var = "birth_date",
      group_var = "gender"
    ),
    "ggplot"
  )
})

# plot_cov() --------------------------------------------------------------
test_that("plot_cov() returns `ggplot` object", {
  expect_class(
    plot_cov(
      readr::read_csv(
        test_path(
          "data",
          "politicians.csv"
        )
      )
    ),
    "ggplot"
  )
})

# plot_count() ------------------------------------------------------------
test_that("plot_count() returns `ggplot` object", {
  expect_class(
    count_data(
      readr::read_csv(
        test_path(
          "data",
          "politicians.csv"
        )
      ),
      grouping_vars = "gender"
    ) %>%
      plot_count("gender"),
    "ggplot"
  )
})

# plot_rel_freq() ---------------------------------------------------------
test_that("plot_rel_freq() returns `ggplot` object", {
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
    ) %>%
      plot_rel_freq(
        x_var = "electoral_term",
        fill_var = "faction_id"
      ),
    "ggplot"
  )
})








