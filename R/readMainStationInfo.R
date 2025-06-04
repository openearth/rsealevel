

#' readMainStationInfo
#'
#' @param filepath path to main_stations.json file
#'
#' @returns dataframe with station info of 6 main stations as used in the Dutch Sea Level Monitor
#' @import readr
#' @import dplyr
#' @import jsonlite
#' @import purrr
#' @export
#'
#' @examples
#' readMainStationInfo()
readMainStationInfo <- function(filepath = "") {
  jsonlite::read_json(
    path = ifelse(
      filepath == "", 
      "https://raw.githubusercontent.com/Deltares-research/sealevelmonitor/refs/heads/main/data/deltares/main_stations.json",
      file.path(filepath, "data/deltares/main_stations.json")
    )
    
  ) %>%
    purrr::map_df(~ unlist(.[1:16]))
}



#' Title
#'
#' @param path path to file NLstations.csv
#'
#' @returns dataframe with locations for Dutch main tide gauges only. The information has been retrieved from PSMSL. 
#' @export
#' @import readr
#' @import dplyr
#'
#' @examples
readMainStationLocations <- function(path = ""){
  read_delim(
    file = ifelse(
      path == "",
      "data/psmsl/NLstations.csv",
      file.path(path, "data/psmsl/NLstations.csv")
    ),
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
}

