

#' Title
#'
#' @param df dataframe with sea level information, including a numeric column "year".
#' @param broken_line_breakpoint breakpoint year for broken line model
#' @param broken_squared_breakpoint breakpoint year for broken squared model
#' @returns input dataframe with extra columns
#' @export
#' @import dplyr
#'
#' @examples
#' 
#' data(dutch_sea_level)
#' df <- dutch_sea_level %>%
#' addBreakPoints()
#' str(df)
#' 
addBreakPoints = function(df, broken_line_breakpoint = 1993, broken_squared_breakpoint = 1960){
  
  stopifnot(
    any(names(df)=="year"),
    is.data.frame(df), 
    broken_line_breakpoint    >  1900, 
    broken_squared_breakpoint >  1900,
    broken_line_breakpoint     < 2000, 
    broken_squared_breakpoint  < 2000
  )
  
  blb_name = paste0("from", broken_line_breakpoint)
  bsb_name = paste0("from", broken_squared_breakpoint, "_square")
  
  df %>%
    dplyr::mutate(!!sym(blb_name) := (year >= broken_line_breakpoint) * (year - broken_line_breakpoint)) %>%
    dplyr::mutate(!!sym(bsb_name) := (year >= broken_squared_breakpoint) * (year - broken_squared_breakpoint) * (year - broken_squared_breakpoint))

}
