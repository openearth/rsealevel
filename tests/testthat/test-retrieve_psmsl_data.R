test_that("connection with psmsl monthly works", {
  testthat::skip_if_offline()
  expect_s3_class(read_monthly_psmsl_csv(20), "data.frame")
})


test_that("connection with psmsl yearly works", {
  testthat::skip_if_offline()
  expect_s3_class(read_yearly_psmsl_csv(20), "data.frame")
})

test_that("adding addtional station information works", {
  testthat::skip_if_offline()
  df <- read_yearly_psmsl_csv(20)
  expect_s3_class(add_station_info(df = df), "data.frame")
})


test_that("psmsl station info scraping works", {
  testthat::skip_if_offline()
  expect_s3_class(get_psmsl_station_table(), "data.frame")
})