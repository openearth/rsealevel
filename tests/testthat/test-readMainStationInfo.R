
test_that("main station info reading works", {
  expect_s3_class(readMainStationInfo(), "data.frame")
})
