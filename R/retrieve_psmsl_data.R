



#' get_psmsl_station_table
#' @param url url of psmsl station table page. Default is https://psmsl.org/data/obtaining/index.php
#' @returns dataframe with station information including lat and lon. The information is scraped from the web page. 
#' @export
#' @import rvest
#' @import rlist
#'
#' @examples
#' stationinfo <- get_psmsl_station_table()

get_psmsl_station_table <- function(url = "https://psmsl.org/data/obtaining/index.php") {
  
  df <- url %>% 
    rvest::read_html() %>% 
    rvest::html_nodes("table") %>% 
    rvest::html_table(fill = T) %>%
    rlist::list.rbind()
  
}


#' read_yearly_psmsl_csv
#'
#' @param station_nr psmsl station number
#' @returns dataframe containing sea level data for the requested station
#' @import readr
#' @import dplyr
#' @export
#'
#' @examples
#' read_yearly_psmsl_csv(c(20, 22, 23, 24, 25, 32))
read_yearly_psmsl_csv  <- function(station_nr){
  
  base_rlr_url = "https://psmsl.org/data/obtaining/rlr.annual.data/"
  base_rlr_ext = ".rlrdata"
  
  rlr_df <- lapply(station_nr, 
                   function(x) {
                     readr::read_delim(
                       file = paste0(base_rlr_url, x, base_rlr_ext), 
                       col_names = c("year", "rlr_height_mm", "interpolated", "flag"),
                       col_types = c("nncc"),
                       na = "-99999",
                       delim = ";"
                     ) |>
                       dplyr::mutate(psmsl_id = as.character(x))
                   }
  ) |>
    dplyr::bind_rows()
  
  return(rlr_df)
  
}


#' read_monthly_psmsl_csv
#'
#' @param station_nr psmsl station number
#' @returns dataframe containing sea level data for the requested station
#' @import readr
#' @import dplyr
#' @export
#'
#' @examples
#' read_monthly_psmsl_csv(c(20, 22, 23, 24, 25, 32))
read_monthly_psmsl_csv  <- function(station_nr){
  
  base_rlr_url = "https://psmsl.org/data/obtaining/rlr.monthly.data/"
  base_rlr_ext = ".rlrdata"
  
  rlr_df <- lapply(station_nr,
                   function(x) {
                     rlr_df <- readr::read_delim(
                       file = paste0(base_rlr_url, x, base_rlr_ext), 
                       col_names = c("decimal_year", "rlr_height_mm", "interpolated", "flag"),
                       col_types = "niic",
                       delim = ";",
                       trim_ws = T, 
                       locale = locale(decimal_mark = "."
                       )
                     ) |>
                       dplyr::mutate(psmsl_id = as.character(x))
                   }
  ) |>
    bind_rows()
  
  return(rlr_df)

}

#' add_station_info
#'
#' @param df dataframe containing sea level data obtained from psmsl.org using read_monthly_psmsl_csv or read_yearly_psmsl_csv. As a minimum it should contain a column named "psmsl_id" referring to the station id as used in the psmsl database. 
#' @param path path to 
#'
#' @returns input dataframe with extra columns "name", "nap-rlr", and "gtsm_id".
#' @import readr
#' @import dplyr
#' @export
#'
#' @examples
#'  read_monthly_psmsl_csv(c(20, 22, 23, 24, 25, 32)) %>% 
#'  add_station_info()
add_station_info <- function(df, path = ""){

mainStationInfo <- readMainStationInfo(path) |>
  dplyr::select(psmsl_id, name, `nap-rlr`, gtsm_id)

df <- df %>% dplyr::left_join(mainStationInfo, by = c(psmsl_id = "psmsl_id"))

return(df)

}
