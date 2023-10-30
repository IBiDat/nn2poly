#' Obtain needed derivatives up to the chosen order (q Taylor)
#'
#' This function is internally used in nn2poly_algorithm to obtain the
#' derivatives of the given activation function at 0 up to the desired order $q$
#' for each layer.
#'
#' @param af_string_list List with the names of the activation function used
#' at each layer as a string. Currently accepted values: "softplus", "linear",
#' "tanh" and "sigmoid.
#' @param taylor_orders List containing the degree up to which Taylor
#' expansion should be performed at each layer.
#'
#' @return list of vectors with the derivatives
#' @noRd
obtain_derivatives_list <- function(af_string_list, taylor_orders) {

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

  af_derivatives_list <- vector(mode = "list", length = n)

  for (i in 1:n) {
    # Obtain the vector with the derivatives of the activation function up to the given degree:
    # centered at 0
    # and use rev to reverse and match our notation.
    af_derivatives_list[[i]] <- rev(pracma::taylor(af_function_list[[i]], 0, taylor_orders[i]))

    # here we have a problem: if the last term of the taylor expansion is 0,
    # the previous method deletes that entry and then the dimensions willm not macth later
    # therefore, we add 0's if needed:
    diff_len <- (taylor_orders[i] + 1) - length(af_derivatives_list[[i]])
    if (diff_len > 0) {
      af_derivatives_list[[i]] <- c(af_derivatives_list[[i]], rep(0, diff_len))
    }
  }

  return(af_derivatives_list)
}
