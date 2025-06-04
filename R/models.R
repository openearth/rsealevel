

use_gtsm <- function(forcing = "GTSM"){
  
  if(forcing == "GTSM") TRUE

}

check_workflow_wind <- function(wind_or_surge_type){
  
  if(!wind_or_surge_type %in% config$runparameters$wind_or_surge_types) {
    cat("incorrect wind or surge specification")
  } else {
    if(use_gtsm()) {
      cat("Surge correction by GTSM is used")
    } else {
      cat("Wind correction ", wind_or_surge_type, " is used")
    }
  }
}



#' linear_model
#'
#' @param df dataframe containing sea level data.
#'
#' @returns Model (lm) object 
#' @export
#'
#' @examples
linear_model <- function(df){
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  } else {
    lm(
      height ~ wind_anomaly + 
        I(year - epoch) + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}

#' broken_linear_model
#'
#' @param df dataframe containing sea level data.
#'
#' @returns Model (lm) object 
#' @export
#'
#' @examples
#' 
#'  data("dutch_sea_level")
#'  
#'  byStation <- dutch_sea_level %>%
#'  addBreakPoints() %>%
#'    dplyr::group_by(station) %>%
#'    tidyr::nest() %>%
#'    dplyr::ungroup()
#'  
#'  selectedmodel <- "broken_linear"
#'  
#'  models <- byStation %>%
#'    tidyr::expand_grid(modeltype = selectedmodel) %>%
#'    dplyr::mutate(modelfunctionname = paste(modeltype, "model", sep = "_")) %>%
#'    # add functions for model calculation
#'    dplyr::mutate(modelfunctions = purrr::map(modelfunctionname, get)) %>%
#'    # add models based on data and functions
#'    dplyr::mutate(model = purrr::pmap(
#'      list(
#'        data,
#'        modelfunctions
#'      ),
#'      \(.d, .f) .f(.d)
#'    )) %>%
#'    mutate(
#'      glance = map(model, broom::glance),
#'      rsq    = glance %>% purrr::map_dbl("r.squared"),
#'      adj.rsq = glance %>% purrr::map_dbl("adj.r.squared"),
#'      AIC    = glance %>% purrr::map_dbl("AIC"),
#'      tidy   = map(model, broom::tidy),
#'      augment = map(model, broom::augment),
#'      equation = map(model, function(x) equatiomatic::extract_eq(x, ital_vars = TRUE))
#'    )
#'  
broken_linear_model <- function(df){
  
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        from1993 + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
    
  } else {
    lm(
      height ~ wind_anomaly +
        I(year - epoch) +
        from1993 +
        I(cos(2 * pi * (year - epoch)/(18.613))) +
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}

#' broken_squared_model
#'
#' @param df dataframe containing sea level data.
#'
#' @returns Model (lm) object 
#' @export
#'
#' @examples
#' 
#'  data("dutch_sea_level")
#'  
#'  byStation <- dutch_sea_level %>%
#'  addBreakPoints() %>%
#'    dplyr::group_by(station) %>%
#'    tidyr::nest() %>%
#'    dplyr::ungroup()
#'  
#'  selectedmodel <- "broken_squared"
#'  
#'  models <- byStation %>%
#'    tidyr::expand_grid(modeltype = selectedmodel) %>%
#'    dplyr::mutate(modelfunctionname = paste(modeltype, "model", sep = "_")) %>%
#'    # add functions for model calculation
#'    dplyr::mutate(modelfunctions = purrr::map(modelfunctionname, get)) %>%
#'    # add models based on data and functions
#'    dplyr::mutate(model = purrr::pmap(
#'      list(
#'        data,
#'        modelfunctions
#'      ),
#'      \(.d, .f) .f(.d)
#'    )) %>%
#'    mutate(
#'      glance = map(model, broom::glance),
#'      rsq    = glance %>% purrr::map_dbl("r.squared"),
#'      adj.rsq = glance %>% purrr::map_dbl("adj.r.squared"),
#'      AIC    = glance %>% purrr::map_dbl("AIC"),
#'      tidy   = map(model, broom::tidy),
#'      augment = map(model, broom::augment),
#'      equation = map(model, function(x) equatiomatic::extract_eq(x, ital_vars = TRUE))
#'    )
#'  
broken_squared_model <- function(df, epoch){
  
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        from1960_square + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
    
  } else {
    lm(
      height ~ wind_anomaly +
        I(year - epoch) +
        from1960_square + 
        I(cos(2 * pi * (year - epoch)/(18.613))) +
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}


#' broken_jerk_model
#'
#' @param df dataframe containing sea level data.
#'
#' @returns Model (lm) object 
#' @export
#'
#' @examples
broken_jerk_model <- function(df){
  # experimental 
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        from1960_square + 
        from1960_third +
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
    
  } else {
    lm(
      height ~ wind_anomaly +
        I(year - epoch) +
        from1960_square + 
        from1960_third +
        I(cos(2 * pi * (year - epoch)/(18.613))) +
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}

