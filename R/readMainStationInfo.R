

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
#' readMainStationInfo() %>% View()
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