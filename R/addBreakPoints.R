

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
    is.data.frame(df), 
    broken_line_breakpoint > 1900, 
    broken_squared_breakpoint > 1900,
    broken_line_breakpoint < 2025, 
    broken_squared_breakpoint < 2025
  )
  
  df %>%
    dplyr::mutate(from1993 = (year >= broken_line_breakpoint) * (year - broken_line_breakpoint)) %>%
    dplyr::mutate(from1960_square = (year >= broken_squared_breakpoint) * (year - broken_squared_breakpoint) * (year - broken_squared_breakpoint))

}
