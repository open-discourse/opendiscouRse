library(ggplot2)

test_that("theme_od() works with `ggplot`", {
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
    ) + theme_od(),
    "ggplot"
  )
})
