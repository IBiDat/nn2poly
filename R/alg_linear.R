#' Title
#'
#' @param coeffs_list_input The coefficients obtained in regression in the same
#' layer for the needed neuron/node.
#' @param current_layer An index denoting which layer we are considering.
#' @param weights_list List containing a matrix of weights and bias for each
#' layer
#' @param output_index An index denoting which of the neurons (or outputs) in
#' the layer we are computing.
#'
#' @noRd
#'
alg_linear <- function(coeffs_list_input,
                                                         current_layer,
                                                         weights_list,
                                                         output_index) {

  # Extract the needed weights at the given layer
  weights_layer <- weights_list[[current_layer]][, output_index]

  # Number of neurons from layer previous to current layer:
  h_layer <- length(weights_layer) - 1

  # The number of coefficients is the same as the obtained in the previous
  # non linear case:
  length_coeffs <- length(coeffs_list_input[[1]])

  # Initialize output
  coeffs_output <- rep(0, length_coeffs)

  # Special case: the intercept:
  coeffs_output[1] <- weights_layer[1]
  for (j in 1:h_layer) {
    coeffs_output[1] <- coeffs_output[1] + weights_layer[j + 1] * coeffs_list_input[[j+1]][1]
  }

  # Rest of the coeffs
  for (i in 2:length_coeffs) {
    for (j in 1:h_layer) {
      coeffs_output[i] <- coeffs_output[i] + weights_layer[j + 1] * coeffs_list_input[[j+1]][i]
    }
  }

  return(coeffs_output)
}
