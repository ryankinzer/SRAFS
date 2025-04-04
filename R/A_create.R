#' Create A matrix for MARSS model
#' @author Ryan N. Kinzer
#' @param .ts 
#' @param .states 
#'
#' @return
#' @export
#'
#' @examples
A_create <- function(.ts, .states) {
  # Initialize A matrix with row names
  A <- matrix(0, nrow = length(.ts), ncol = 1)
  rownames(A) <- .ts
  
  # Dictionary to track how many times each state has been assigned
  state_counts <- setNames(rep(0, length(.states)), .states)  # Counter for each state
  
  # Assign values to A matrix
  for (i in seq_along(.ts)) {
    # Find which state the time series belongs to
    matching_state <- .states[sapply(.states, function(s) grepl(s, .ts[i]))]  #which state name is in the time series name?
    
    if (length(matching_state) == 1) {  # Ensure exactly one match
      state_counts[matching_state] <- state_counts[matching_state] + 1  # Increment count
      
      if (state_counts[matching_state] == 1) {
        A[i, 1] <- 0  # First occurrence gets 0
      } else {
        A[i, 1] <- paste0("a", matching_state, state_counts[matching_state])  
      }
    } else {
      warning("Multiple or no states matched for ", .ts[i])  # Handle incorrect matches
    }
  }
  
  return(A)
}