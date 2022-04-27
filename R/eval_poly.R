#' Evaluates a polynomial on a single observation of variables.
#'
#' @param x Input data as matrix row, vector or dataframe row.
#' The number of columns should be the number of variables in the polynomial
#' (the dimension p).
#' @param labels Labels of the coefficients, that is, a list of vectors where
#' each one represents the combination of variables associated with that
#' coefficient. The variables are numbered from 1 to p.
#' @param coefficients The vector of numeric coefficients for the polynomial.
#'
#' @return Numerical value with the polynomial evaluated at x.
#' @export
#'

eval_poly <- function(x, labels, coeffs) {

  # Remove names and transform into matrix
  x <- unname(as.matrix(x))

  response <- coeffs[1] # Intercept

  # Loop over the rest of coefficients
  for (i in 2:length(coeffs)) {

    # Obtain the labels of that coefficient
    label_i <- labels[[i]]

    # Intialize the product as 1 and loop over all the indexes l_j to obtain the product of all the needed variables x
    product <- 1
    for (j in 1:length(label_i)) {
      product <- product * x[label_i[j]]
    }

    # We add to the response the product of those variables with their associated beta
    response <- response + coeffs[i] * product
  }
  return(response)
}
