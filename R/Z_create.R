#' Create Z matrix for MARSS model
#' @author Ryan N. Kinzer
#' @param .ts 
#' @param .states 
#'
#' @return
#' @export
#'
#' @examples
Z_create <- function(.ts, .states){

  Z = matrix(0, nrow = length(.ts), ncol = length(.states))
  colnames(Z) <- .states
  rownames(Z) <- .ts
  
  for(i in 1:length(.ts)){
    for(j in 1:length(.states)){
      t <- .ts[i]
      s <- .states[j]
      if(grepl(s, t)) Z[i,j] <- 1
    }
  }  
  
  return(Z)
  
}