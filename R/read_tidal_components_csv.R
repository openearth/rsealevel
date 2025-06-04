
#' read_tidal_components_csv
#'
#' @param files Specify files to read. Alternatively specify a directory to read all files from.
#' @param filesdir Specify directory to read all files from. Either files or filesdir needs to be specified.
#'
#' @returns dataframe with amplitudes and phases for all stations and tidal components in the files read.
#' @export
#' @import readr
#' @import dplyr
#' @import tibble
#' @import stringr
#' 
#' @examples
read_tidal_components_csv <- function(files = NA, filesdir = "p:/11202493--systeemrap-grevelingen/1_data/Wadden/ddl/calculated/TA_filtersurge") {
  
  if(any(is.na(files))){
    filelist <- list.files(filesdir, pattern = "component", full.names = T)
    # get names of stations and year from filenames in filelistShort
    filelistShort <- list.files(filesdir, pattern = "component", full.names = F)
  } else{
    filelist <- file.path(files)
    filelistShort <- basename(files)
  }   
  
  mainstations_df <- readMainStationInfo()
  
  df <- lapply(filelist, 
               function(x) {
                 readr::read_csv(x, col_types = readr::cols(), progress = FALSE) %>%
                   dplyr::mutate(verticalreference = dplyr::case_when(
                    grepl("MSL", x) ~ "MSL",
                     !grepl("MSL", x) ~ "NAP"
                   ))
               }
  )
  dfs <- dplyr::bind_rows(df)
  
  names <- tibble::tibble(name = stringr::str_replace(filelistShort, pattern = "_UTC\\+1.csv", replacement = "")) %>%
    tidyr::separate(name, c("station", "jaar", "component"), sep = "_") %>%
    dplyr::select(-component) %>%
    dplyr::left_join(mainstations_df[,c("ddl_id", "name")], by = c(station = "ddl_id"))
  
  names %>% 
    dplyr::mutate(jaar = as.integer(jaar)) %>%
    dplyr::mutate(data = df)
}
