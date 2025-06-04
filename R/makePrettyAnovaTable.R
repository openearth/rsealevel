

#' Make pretty ANOVA table for presentation
#'
#' @param output ANOVA output table
#' @param digits Number of digits desired in pretty table
#'
#' @returns output table with pretty numbers
#' @export
#'
#' @examples
makePrettyAnovaTable <- function(output, digits) {
  rm.cols <- NULL
  for (i in 1:(dim(output)[2])) {
    if (all(is.na(output[,i]))) { rm.cols <- c(rm.cols,i) }
  }
  if (!is.null(rm.cols)) { output <- output[,-rm.cols] }
  # Reformat column names
  names(output) <- sub("Rsq","R^2^",names(output))
  names(output) <- sub("Pr\\(>F\\)","p",names(output))
  names(output) <- sub("^P$","p",names(output))
  # Rounding
  output <- apply(output, 2, signif, digits = digits)
  # Generate the kable
  options(knitr.kable.NA = '')
  knitr::kable(output)
}