test_that("broken squared model works", {
  
  expect_equal(
    {
      data("dutch_sea_level")
      epoch = 1970

      byStation <- dutch_sea_level %>%
        addBreakPoints() %>%
        dplyr::group_by(station) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

      selectedmodel <- "broken_squared"

      byStation %>%
        tidyr::expand_grid(modeltype = selectedmodel) %>%
        dplyr::mutate(modelfunctionname = paste(modeltype, "model", sep = "_")) %>%
        # add functions for model calculation
        dplyr::mutate(modelfunctions = purrr::map(modelfunctionname, get)) %>%
        mutate(epoch = epoch) %>%
        # add models based on data and functions
        dplyr::mutate(model = purrr::pmap(
          list(
            data,
            modelfunctions,
            epoch
          ),
          \(.d, .f, .e) .f(.d, .e)
        )) %>%
        mutate(
          glance = map(model, broom::glance),
          rsq    = glance %>% purrr::map_dbl("r.squared"),
          adj.rsq = glance %>% purrr::map_dbl("adj.r.squared"),
          AIC    = glance %>% purrr::map_dbl("AIC"),
          tidy   = map(model, broom::tidy),
          augment = map(model, broom::augment)
        ) %>%
        summarize(sum = sum(rsq)) %>%
        unlist() %>%
        unname()
    },
      # 7.12831,
    7.12831027
  )
})
