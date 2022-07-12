test_that("count_tokens() returns data.frame", {
  expect_true(
    count_tokens(
      "electoral_term"
    ) %>%
      is.data.frame()
  )
})

test_that("rel_freq_tokens() returns data.frame", {
  expect_true(
    rel_freq_tokens(
      c("electoral_term", "faction_id"),
      "faction_id"
    ) %>%
      is.data.frame()
  )
})
