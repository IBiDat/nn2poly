#' dimensions checker
#'
#' @param weights_matrices_list List of weight matrices of a given neural network.
#'
#' @return `TRUE` if the dimensiones are correct, `FALSE` if not.
#'
check_weights_dimensions <- function(weights_matrices_list) {
  for (matrix_index in 2:length(weights_matrices_list)) {
    nrows_current <- nrow(weights_matrices_list[[matrix_index]])
    ncols_prev    <- ncol(weights_matrices_list[[matrix_index - 1]])

    if (nrows_current != ncols_prev + 1)
      return(FALSE)
  }
  return(TRUE)
}
