
waterhoogtegrootheden <- function() {
  c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    
}



#' get_selected_metadata from Rijkswaterstaat data distribution layer database
#'
#' This is a function to retrieve metadata from the Rijkswaterstaaat database. 
#'
#' @param compartiment Compartment name or code (AQUO standard)
#' @param grootheid Quantity name  or code(AQUO standard)
#' @param parameter Parameter name or code(AQUO standard)
#' @param locatie Location name  or code (RWS standard)
#'
#' @returns Dataframe containing metadata selection for chosen parameters. 
#' @export
#'
#' @examples
#' 
get_selected_metadata <- function(
    compartiment = NULL, 
    grootheid = NULL, 
    parameter = NULL, 
    locatie = NULL
) {
  
  require(rwsapi)
  require(tidyverse)
  
  md <- rwsapi::rws_metadata()
  
  md$content$AquoMetadataLijst %>% 
    unnest(
      names_sep = ".", 
      c(Compartiment, Eenheid, Grootheid, Hoedanigheid, Parameter)) %>% 
    filter(
      if(is.null(grootheid)) TRUE else Grootheid.Omschrijving %in% grootheid | Grootheid.Code %in% grootheid,
      if(is.null(parameter)) TRUE else Parameter.Omschrijving %in% parameter | Parameter.Code %in% parameter,
      if(is.null(compartiment)) TRUE else Compartiment.Code %in% compartiment | Compartiment.Code %in% compartiment
    ) %>%
    left_join(
      md$content$AquoMetadataLocatieLijst, 
      by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")
    ) %>%
    left_join(md$content$LocatieLijst) %>%
    filter(if(is.null(locatie)) TRUE else Naam %in% locatie |  Code %in% locatie) %>% 
    rename_with(tolower) %>%
    rename(
      locatie.naam = naam,
      locatie.code = code
    )
}

#' Title
#'
#' @param models Dataframe with models and data
#' @param lookup lookup table for shorter names
#'
#' @returns Table with prediction values per station and model
#' @export
#'
#' @examples
makePredictionTable <- function(models, lookup = lookup) {
  
  all_predictions <- models %>%
    mutate(
      preds = map2(data, model, add_predictions)
    ) %>%
    dplyr::select(
      station,
      modeltype, 
      data, 
      tidy, 
      preds) %>%
    tidyr::unnest(c(data, preds), names_sep = "_") %>% 
    tidyr::unnest(tidy) %>%
    # str(max.level = 2)
    
    dplyr::select(-std.error, -statistic, -p.value) %>% # clean up
    tidyr::pivot_wider(
      names_from = term, 
      values_from = estimate
    ) %>%
    mutate(`data_height-surge_anomaly` = data_height - `preds_surge_anomaly`) %>%
    mutate(`preds_height-surge_anomaly` = preds_pred - `preds_surge_anomaly`) %>%
    rename(any_of(lookup)) %>%
    # str(max.level = 2)
    mutate(
      nodal_tide = 
        u_nodal * cos(2*pi*(data_year-epoch)/18.613) + 
        v_nodal * sin(2*pi*(data_year-epoch)/18.613),
      prediction_recalc = case_when(
        if("linear" %in% params$modeltype){
          modeltype == "linear" ~ 
            Constant + 
            Trend * (data_year - epoch)
        },
        if("broken_linear" %in% params$modeltype){
          modeltype == "broken_linear" ~ 
            Constant + 
            Trend * (data_year - epoch) +
            (data_year >= 1993) * `+ trend 1993` * (data_year - 1993)
        },
        if("broken_squared" %in% params$modeltype){
          modeltype == "broken_squared" ~ Constant + 
            Trend * (data_year - epoch) +
            (data_year >= 1960) * `+ square_trend 1960` * (data_year - 1960) * (data_year - 1960)
        }
      )
    ) %>%
    select(
      station,
      modeltype,
      data_year,
      data_height,
      preds_year,
      prediction_recalc,
      `data_height-surge_anomaly`,
      `preds_height-surge_anomaly`,
      nodal_tide
    )
  
  return(all_predictions)
}






#' Read water level data from Rijkswaterstaat ddl
#'
#' @param station 
#' @param startyear 
#' @param endyear 
#' @param grootheid 
#' @param outDir 
#'
#' @returns
#' @export
#'
#' @examples
readDDLwaterhoogte <- function(station, startyear, endyear, grootheid = "Waterhoogte", outDir = "data/rijkswaterstaat/ddl/raw"){
  
  require(rwsapi)
  require(tidyverse)
  
  waterhoogteparameters <- c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    
  
  # make warning if grootheid is not in list of waterhoogteparameters.  
  
  md <- rwsapi::rws_metadata()
  thisCatalogue <- md$content$AquoMetadataLijst %>% 
    unnest(
      names_sep = ".", 
      c(Compartiment, Eenheid, Grootheid, Hoedanigheid, Parameter)) %>% 
    filter(Grootheid.Omschrijving == grootheid) %>%
    left_join(
      md$content$AquoMetadataLocatieLijst, 
      by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")
    ) %>%
    left_join(md$content$LocatieLijst) %>% 
    filter(Code %in% station) %>%
    rename_with(tolower) %>%
    rename(
      locatie.naam = naam,
      locatie.code = code
    )
  
  for(iyear in startyear:endyear){
    rwsapi::getDDLdata( 
      startyear = iyear,
      endyear = iyear,
      myCatalogue = thisCatalogue,
      outDir = outDir
    )
  }
}

#' Read water level data from the Rijkswaterstaat database
#'
#' @param ddlmetadata Metadata as generated by [get_selected_metadata()] 
#' @param startyear 
#' @param endyear 
#' @param outDir 
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' 
#' 
readDDLwaterhoogte2 <- function(ddlmetadata, startyear, endyear, outDir = "data/rijkswaterstaat/ddl/raw"){
  
  require(rwsapi)
  require(tidyverse)
  
  waterhoogteparameters <- c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    
  
  # make warning if grootheid is not in list of waterhoogteparameters.  
  
  for(iyear in startyear:endyear){
    rwsapi::getDDLdata( 
      startyear = iyear,
      endyear = iyear,
      myCatalogue = ddlmetadata,
      outDir = outDir
    )
  }
}
