#' Evaluates a polynomial (or several polynomials with the same terms) on
#' some given data.
#'
#' @param x Input data as matrix, vector or dataframe.
#' The number of columns should be the number of variables in the polynomial
#' (the dimension p). Response variable to be predicted should not be included.
#'
#' @param coeffs List containing 2 items: \code{labels} and \code{values}.
#' - \code{labels} List of integer vectors with same length (or number of rows)
#' as \code{values},
#' where each integer vector denotes the combination of variables associated to
#' the coefficient value stored at the same position in \code{values}. Note
#' that the variables are numbered from 1 to p.
#' - \code{values} Vector (or matrix)
#'
#' Example: If \code{labels} contains the integer vector c(1L,1L,3L) at position
#' 5, then the value stored in \code{values} at position 5 is the coefficient
#' associated with the term $x_1^2*x_3$.
#'
#'
#' @return Numerical value with the polynomial evaluated at x.
#' @export
#'

eval_poly <- function(x, coeffs) {

  # Remove names and transform into matrix
  x <- unname(as.matrix(x))

  # If values is a single vector, transform into matrix
  if (!is.matrix(coeffs$values)){
    coeffs$values <- as.matrix(coeffs$values)
  }

  # Initialize list which will contain results for each desired polynomial,
  # with length equal to the rows of `coeffs$values`
  n_polynomials <- nrow(coeffs$values)
  response <- vector(mode = "list", length = n_polynomials)
  for (j in 1:n_polynomials){

    # Select the desired polynomial values (row of coeffs$values)
    values_j <- coeffs$values[j,]

    # Intercept (label = 0). should always be the first element of labels.
    if (coeffs$labels[[1]] == c(0)){
      response_j <- rep(values_j[1], nrow(x))
      start_loop <- 2
    } else {
      response_j <- rep(0, nrow(x))
      start_loop <- 1
    }

    # Loop over all terms (labels) except the intercept
    for (i in start_loop:length(values_j)) {

      label_i <- coeffs$labels[[i]]

      # Need to differenciate between 1 single label or more to use rowProds
      if(length(label_i) == 1){
        product <- x[,label_i]
      } else {
        # Obtain the product of each variable as many times as label_i indicates
        product <- matrixStats::rowProds(x[,label_i])
      }


      # We add to the response the product of those variables
      # with their associated coefficient value
      response_j <- response_j + values_j[i] * product
    }
    response[[j]] <- response_j
  }

  return(response)
}
