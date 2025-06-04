
#' Add HAC standard terms
#'
#' This function adds a column of HAC terms using the sandwich package to a dataframe containging models.
#'
#' @param models Dataframe with data and models containing one column named 'model' containg elements of class 'model'.
#'
#' @returns
#' @export
#' @import sandwich
#' @import dplyr
#' @import broom
#' @import purrr
#'
#' @examples
addHACterms <- function(models) {
  
  modelstemp <- models %>%
    dplyr::mutate(
      tidy.HAC = purrr::map(
        model, 
        function(x) broom::tidy(
          sqrt(
            diag(
              sandwich::NeweyWest(
                x, 
                lag = 1, 
                prewhite = F, 
                adjust = T
              )
            )
          )
        )
      )
    )
  
  modelstemp$tidy.HAC <- lapply(modelstemp$tidy.HAC,
                                function(x) {
                                  x %>%
                                    dplyr::rename(
                                      term.HAC = names,
                                      st.err.HAC = x
                                    )
                                }
  )
  
  return(modelstemp)
}
