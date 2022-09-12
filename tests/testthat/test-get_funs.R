test_that("get_age() returns numeric value", {
  expect_numeric(get_age(Sys.Date()))
})
