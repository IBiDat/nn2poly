#' Title
#'
#' @param coeffs_list_input The coefficients obtained in regression in the same
#' layer for the needed neuron/node.
#' @param layer_index An index denoting which layer we are considering.
#' @param weights_list List containing a matrix of weights and bias for each
#' layer
#' @param output_index An index denoting which of the neurons (or outputs) in
#' the layer we are computing.
#'
#' @return
#' @noRd
#'
obtain_PR_coefficients_regression_deep_layer <- function(coeffs_list_input,
                                                         layer_index,
                                                         weights_list,
                                                         output_index) {

  # Extract the needed weights at the layer L
  weights_layer <- weights_list[[layer_index]][, output_index]

  # Number of neurons from layer L-1 to L:
  h_layer <- length(weights_layer) - 1

  # The number of coefficients is the same as the obtained in the previous classification:
  length_coeffs <- length(coeffs_list_input[[1]])

  # Initialize output and set labels:
  coeffs_output <- matrix(rep(0, length_coeffs), nrow = 1)
  colnames(coeffs_output) <- colnames(coeffs_list_input[[1]])

  # Special case: the intercept:
  coeffs_output[1] <- weights_layer[1]
  for (j in 1:h_layer) {
    coeffs_output[1] <- coeffs_output[1] + weights_layer[j + 1] * coeffs_list_input[[j]][1]
  }

  # Rest of the coeffs
  for (i in 2:length_coeffs) {
    for (j in 1:h_layer) {
      coeffs_output[i] <- coeffs_output[i] + weights_layer[j + 1] * coeffs_list_input[[j]][i]
    }
  }

  return(coeffs_output)
}
