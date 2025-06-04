test_that("DDL metadata service works", {
  requier
    expect_s3_class(
      get_selected_metadata(
        compartiment = "OW", 
        grootheid = waterhoogtegrootheden(), 
        locatie = "Den Helder"
      ), 
      "data.frame"
    )
})
