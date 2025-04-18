#' Summarize MARSS model fits.
#' @author Ryan N. Kinzer
#'
#' @param mod_fit list of MARSS model objects 
#'
#' @return
#' @export
#'
#' @examples
summarize_ModelFits <- function(mod_fit){
  
  require(MARSS)
  
  mod_results <- tibble(
    model_id = seq_along(mod_fit),
    logLik = sapply(mod_fit, function(x) logLik(x)),
    AICc = sapply(mod_fit, function(x) x$AICc),
    #AICb = sapply(mod_fit, function(x) MARSSaic(x, output = 'AICbp')),
    converged = sapply(mod_fit, function(x) x$convergence),
    n_samps = sapply(mod_fit, function(x) x$samp.size),
    n_params = sapply(mod_fit, function(x) x$num.params),
    U = sapply(mod_fit, function(x) length(x$par$U)),
    Q = sapply(mod_fit, function(x) length(x$par$Q)),
    A = sapply(mod_fit, function(x) length(x$par$A)),
    R = sapply(mod_fit, function(x) length(x$par$R))
  ) %>%
    mutate(deltaAIC = AICc - min(AICc)) %>%
    arrange(AICc)
  
  # mod_results <- mod_results %>%
  #   filter(!(model_id %in% c(3, 4, 5, 6))) %>%
  #   mutate(model_id = 1:n()) %>%
  #   arrange(AICc)# a single process only has a single variance (no model runs for diag and unequal or equalvarcov)
  
  return(mod_results)
}
