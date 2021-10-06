#' Title
#'
#' @param af_function_list List with the names of the activation fucntion used
#' at each layer
#' @param q_taylor_vector List containing the degree up to which Taylor
#' expansion should be performed at each layer.
#'
#' @return
#' @noRd
#'
obtain_derivatives_list <- function(af_function_list, q_taylor_vector) {
  n <- length(af_function_list)

  af_derivatives_list <- vector(mode = "list", length = n)

  for (i in 1:n) {
    # Obtain the vector with the derivatives of the activation function up to the given degree:
    # centered at 0
    # and use rev to reverse and match our notation.
    af_derivatives_list[[i]] <- rev(pracma::taylor(af_function_list[[i]], 0, q_taylor_vector[i]))

    # here we have a problem: if the last term of the taylor expansion is 0,
    # the previous method deletes that entry and then the dimensions willm not macth later
    # therefore, we add 0's if needed:
    diff_len <- (q_taylor_vector[i] + 1) - length(af_derivatives_list[[i]])
    if (diff_len > 0) {
      af_derivatives_list[[i]] <- c(af_derivatives_list[[i]], rep(0, diff_len))
    }
  }

  return(af_derivatives_list)
}
