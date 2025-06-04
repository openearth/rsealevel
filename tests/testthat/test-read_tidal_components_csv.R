test_that("multiplication works", {
  testthat::skip_if_offline()
  expect_s3_class(read_tidal_components_csv(), "data.frame")
})
