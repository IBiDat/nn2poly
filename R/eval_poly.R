#' Polynomial evaluation
#'
#' Evaluates one or several polynomials on the given data.
#'
#' Note that this function is unstable and subject to change. Therefore it is
#' not exported but this documentations is left available so users can use it if
#' needed to simulate data by using \code{nn2poly:::eval_poly()}
#'
#' @param poly List containing 2 items: \code{labels} and \code{values}.
#' - \code{labels}: List of integer vectors with same length (or number of cols)
#' as \code{values}, where each integer vector denotes the combination of
#' variables associated to the coefficient value stored at the same position in
#' \code{values}. That is, the monomials in the polynomial. Note that the
#' variables are numbered from 1 to p, with the intercept is represented by 0.
#' - \code{values}: Matrix (can also be a vector if single polynomial), where
#' each column represents a polynomial, with same number of rows as the length
#' of \code{labels}, containing at each row the value of the coefficient
#' of the monomial given by the equivalent label in that same position.
#'
#' Example: If \code{labels} contains the integer vector c(1,1,3) at position
#' 5, then the value stored in \code{values} at row 5 is the coefficient
#' associated with the term x_1^2*x_3.
#'
#' @param newdata Input data as matrix, vector or dataframe.
#' Number of columns (or elements in vector) should be the number of variables
#' in the polynomial (dimension p). Response variable to be predicted should
#' not be included.
#'
#' @return Returns a matrix containing the evaluation of the polynomials.
#' Each column corresponds to each polynomial used and each row to each
#' observation, meaning that each column vector corresponds to the results of
#' evaluating all the given data for each polynomial.
#'
#' @seealso \code{eval_poly()} is also used in [predict.nn2poly()].
#'
eval_poly <- function(poly, newdata) {

  newdata <- preprocess_newdata(newdata)

  aux <- preprocess_poly(poly)
  poly <- aux$poly
  intercept_position <- aux$intercept_position


  # Initialize matrix which will contain results for each desired polynomial,
  # with columns equal to the columns of `poly$values`, that is, the number of
  # polynomials and rows equal to the number of observations evaluated.
  n_polynomials <- ncol(poly$values)
  response <- matrix(0, nrow = nrow(newdata), ncol = n_polynomials)
  for (j in 1:n_polynomials){

    # Select the desired polynomial values (column of poly$values)
    values_j <- poly$values[,j]

    # Intercept (label = 0) should always be the first element of labels at this
    # point of the function (labels reordered previously in preprocess_poly).
    # Poly had no intercept if intercept_position is NULL
    if (is.null(intercept_position)){
      # initialize hte vector with 0s repeated as needed.
      response_j <- rep(0, nrow(newdata))
      start_loop <- 1
    } else {
      # Initialize the vector with the intercept value repeated as needed.
      response_j <- rep(values_j[1], nrow(newdata))
      start_loop <- 2
    }

    # Loop over all terms (labels) except the intercept
    for (i in start_loop:length(values_j)) {

      label_i <- poly$labels[[i]]

      # Need to differentiate between 1 single label or more to use colProds
      if(length(label_i) == 1){
        # When single variable, it is included in 1:p, that are also the
        # number of columns in newdata
        var_prod <- newdata[,label_i]
      } else {
        # Special case if newdata is a single observation.
        # Selecting the vars in newdata returns a column instead of row in this case
        if(nrow(newdata)==1){
          var_prod <- matrixStats::colProds(as.matrix(newdata[,label_i]))
        } else {
          # Obtain the product of each variable as many times as label_i indicates
          var_prod <- matrixStats::rowProds(newdata[,label_i])
        }

      }


      # We add to the response the product of those variables
      # with their associated coefficient value

      response_j <- response_j + values_j[i] * var_prod
    }
    response[,j] <- response_j
  }

  # Check if it is a single polynomial and transform to vector:
  if (dim(response)[2]==1){
    response <- as.vector(response)
  }

  return(response)
}
