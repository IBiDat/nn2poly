#' Auxiliar function used to transform some of the used activation functions
#' in the neural network from their string name into an R function.
#'
#' @param af_string_list List of strings containing the predefined possible
#' activation functions, i.e., "softplus", "tanh", "sigmoid" or "linear".
#'
#' @return List of R functions associated with the string names provided.
#'
#' @noRd
change_string_to_function <- function(af_string_list) {
  n <- length(af_string_list)

  af_function_list <- vector(mode = "list", length = n)

  for (i in 1:n) {
    if (af_string_list[[i]] == "softplus") {
      af_function_list[[i]] <- function(x) log(1 + exp(x)) # Softplus
    } else if (af_string_list[[i]] == "tanh") {
      af_function_list[[i]] <- function(x) tanh(x) # Hyperbolic Tangent
    } else if (af_string_list[[i]] == "sigmoid") {
      af_function_list[[i]] <- function(x) 1 / (1 + exp(-x)) # Sigmoid
    } else if (af_string_list[[i]] == "linear") {
      af_function_list[[i]] <- function(x) x # Linear
    }
  }

  return(af_function_list)
}
