# library(magrittr)

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





