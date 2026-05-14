#' Summarize MARSS model fits.
#' @author Ryan N. Kinzer
#'
#' @param mod_fit list of MARSS model objects 
#'
#' @return
#' @export
#'
#' @examples
summarize_ModelFits <- function(mod_fit, model_grid = NULL){
  
  require(MARSS)
  require(dplyr)
  require(tibble)
  
  mod_results <- tibble(
    fit_index = seq_along(mod_fit),
    logLik = sapply(mod_fit, function(x) logLik(x)),
    AICc = sapply(mod_fit, function(x) x$AICc),
    converged = sapply(mod_fit, function(x) x$convergence),
    n_samps = sapply(mod_fit, function(x) x$samp.size),
    n_params = sapply(mod_fit, function(x) x$num.params),
    U_n = sapply(mod_fit, function(x) length(x$par$U)),
    Q_n = sapply(mod_fit, function(x) length(x$par$Q)),
    A_n = sapply(mod_fit, function(x) length(x$par$A)),
    R_n = sapply(mod_fit, function(x) length(x$par$R))
  )
  
  if (!is.null(model_grid)) {
    mod_results <- bind_cols(
      model_grid %>% select(model_id, Z, Q, R, x0, U, output_file),
      mod_results
    )
  }
  
  mod_results <- mod_results %>%
    mutate(deltaAIC = AICc - min(AICc, na.rm = TRUE)) %>%
    arrange(AICc)
  
  return(mod_results)
}
