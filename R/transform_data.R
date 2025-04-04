#' Created centered and scaled variables for modeling.
#' @author Ryan N. Kinzer
#' @param .data dataset containing a variable 'nosaij'
#' @param ... grouping variables
#'
#' @return
#' @export
#'
#' @examples
transform_data <- function(.data, ...) {
  df <- .data %>%
    group_by(...) %>%
    mutate(
      #z = scale(nosaij),
      c = (nosaij - mean(nosaij, na.rm = TRUE)),
      z = (nosaij - mean(nosaij, na.rm = TRUE)) / sd(nosaij, na.rm = TRUE),
      mu = mean(nosaij, na.rm = TRUE),
      #center = nosaij - mu,
      logSA = log(nosaij + 1),
      R = lead(nosaij, n = 4),
      logRS = log(R + 1) - log(tsaij + 1),
      RS = exp(logRS),
      pos = logRS >= 0
    ) %>%
    ungroup()
  
  return(df)
}
